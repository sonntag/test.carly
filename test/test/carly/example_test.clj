(ns test.carly.example-test
  (:require
    [clojure.test :refer :all]
    [clojure.test.check.generators :as gen]
    [clojure.test.check.properties :as prop]
    [clojure.test.check.clojure-test :refer [defspec]]
    [test.carly.core :as carly :refer [defop]]))


(def ^:private new-key-gen
  (gen/fmap (comp keyword str) gen/char-alpha))

(defn- gen-some-key
  [{:keys [data deleted]}]
  (gen/one-of
    (cond-> [new-key-gen]
      (seq data) (conj (gen/elements (keys data)))
      (seq deleted) (conj (gen/elements deleted)))))

(defop ::list-keys
  "Here we define a no-parameter operation which reads from the system. It does
  not change the model state, but does check that the system returned the
  expected results."
  []

  (call [system]
    (keys @system))

  (check [{:keys [data]} result]
    (= (not-empty (sort (keys data))) @result)
    #_(is (= (not-empty (sort (keys data))) result))))


(defop ::get-entry
  "This operation specifies a key to lookup in the store, so it defines a
  `gen-args` form. By generating a tuple, the positional values are used
  for each field in the operation."
  [k]

  (gen-args [state]
    (gen/tuple (gen-some-key state)))

  (call [system]
    (get @system k))

  (check [state result]
    (= (get-in state [:data k]) @result)
    #_(is (= (get-in state [:data k]) @result))))


(defop ::put-entry
  "Put is a side-effecting entry, so it defines an `next-state` method. This
  returns an updated version of the model state after applying the operation.
  This op also shows another way to generate args, by generating a map of field
  keys to values."
  [k v]

  (gen-args [state]
    (gen/hash-map
      :k (gen-some-key state)
      :v gen/large-integer))

  (call [system]
    (swap! system assoc k v))

  (next-state [state result]
    (update state :data assoc k v)))


(defop ::remove-entry
  "Remove is also side-effecting, but does not define any checking logic. It
  generates a map of args with a full generator expression, which is passed
  to the record's map constructor."
  [k]

  (gen-args [state]
    (gen/hash-map :k (gen-some-key state)))

  (call [system]
    (swap! system dissoc k))

  (next-state [state result]
    (-> state
        (update :data dissoc k)
        (update :deleted conj k))))


(def store-model
  (carly/create-model
    #(atom (sorted-map))
    [::list-keys ::get-entry ::put-entry ::remove-entry]))


(defspec store-prop
  (prop/for-all [[state ops] (carly/gen-test-case store-model)]
    (carly/check-test-case store-model [state ops])))


#_(deftest linear-store-test
  (carly/check-system "basic linear store tests" 100
    store-model))

#_(deftest ^:concurrent concurrent-store-test
  (carly/check-system "concurrent store tests" 100
    store-model
    :parallel? true))
