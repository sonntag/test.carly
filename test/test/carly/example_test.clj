(ns test.carly.example-test
  (:require
    [clojure.test :refer :all]
    [clojure.test.check.generators :as gen]
    [test.carly.core :as carly :refer [defop]]))


;; Here we define a no-parameter operation which reads from the system. It does
;; not change the model state, but does check that the system returned the
;; expected results.
(defop ListKeys
  []

  (call
    [this system]
    (keys @system))

  (check
    [this {:keys [data]} result]
    (is (= (not-empty (sort (keys data))) result))))


;; This operation specifies a key to lookup in the store, so it defines a
;; `gen-args` form. By generating a tuple, the positional values are used
;; for each field in the operation.
(defop GetEntry
  [k]

  (gen-args
    [state]
    (gen/tuple
      (gen/elements (:keys state))))

  (call
    [this system]
    (get @system k))

  (check
    [this state result]
    (is (= (get-in state [:data k]) result))))


;; Put is a side-effecting entry, so it defines an `next-state` method. This
;; returns an updated version of the model state after applying the operation.
;; This op also shows another way to generate args, by generating a map of field
;; keys to values.
(defop PutEntry
  [k v]

  (gen-args
    [state]
    (gen/hash-map
      :k (gen/elements (:keys state))
      :v gen/large-integer))

  (call
    [this system]
    (swap! system assoc k v)
    v)

  (check
    [this state result]
    (is (= v result)))

  (next-state
    [this state]
    (update state :data assoc k v)))


;; Remove is also side-effecting, but does not define any checking logic. It
;; generates a map of args with a full generator expression, which is passed
;; to the record's map constructor.
(defop RemoveEntry
  [k]

  (gen-args
    [state]
    (gen/hash-map :k (gen/elements (:keys state))))

  (call
    [this system]
    (swap! system dissoc k)
    nil)

  (next-state
    [this state]
    (update state :data dissoc k)))


(def op-generators
  "Returns a vector of operation generators when called with the initial state."
  (juxt gen->ListKeys
        gen->GetEntry
        gen->PutEntry
        gen->RemoveEntry))


(def gen-init-state
  "Generator for initial states; this contains the set of possible keys to use in
  operations."
  (gen/hash-map :keys (gen/set (gen/fmap (comp keyword str) gen/char-alpha) {:min-elements 1})))


(deftest linear-store-test
  (carly/check-system "basic linear store tests" 100
    #(atom (sorted-map))
    op-generators
    :init-state-gen gen-init-state
    :concurrency 1
    :repetitions 1))


(deftest ^:concurrent concurrent-store-test
  (carly/check-system "concurrent store tests" 20
    #(atom (sorted-map))
    op-generators
    :init-state-gen gen-init-state
    :concurrency 4
    :repetitions 3))
