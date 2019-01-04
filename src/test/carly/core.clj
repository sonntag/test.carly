(ns test.carly.core
  "Suite of tests to verify that a given system implementation conforms to the
  spec during a sequence of operations performed on it."
  (:require
    [clojure.test :as ctest]
    [clojure.test.check.generators :as gen]
    (test.carly
      [check :as check]
      [op :as op]
      [report :as report]
      [result :as result]
      [search :as search]))
  (:import (clojure.lang ArityException)))


;; ## Test Operation Definition

(defn- generator-body
  "Macro helper to build a generator constructor."
  [attr-vec body]
  `(gen/fmap
     (fn [op-args#]
       (if (vector? op-args#)
         (zipmap ~(mapv keyword attr-vec) op-args#)
         op-args#))
     (do ~@body)))

(defn- op-method
  "Macro helper to build the op method definition"
  [op-type attr-vec [method args & body]]
  (let [op-destruct [{:keys attr-vec}]]
    `(defmethod ~(symbol "test.carly.op" (name method)) ~op-type
       ~(into op-destruct args)
       ~@body)))


(defmacro defop
  "Defines a new specification for a system operation test."
  [op-type attr-vec & forms]
  (let [[doc-str attr-vec forms] (if (string? attr-vec)
                                   [attr-vec (first forms) (rest forms)]
                                   [nil attr-vec forms])
        defined (zipmap (map first forms) forms)]
    (when-let [unknown-forms (seq (dissoc defined 'gen-args 'call 'check 'next-state
                                          'can-generate?))]
      (throw (ex-info "Unknown forms defined in operation body"
                      {:unknown (map first unknown-forms)})))
    `(do
       (op/register ~op-type ~doc-str)

       ~@(map (partial op-method op-type attr-vec)
              (vals (dissoc defined 'gen-args 'check 'can-generate?)))

       ~(when-let [[method args & body] (defined 'check)]
          (op-method op-type attr-vec [method args (report/wrap-report-check body)]))

       ~(when-let [[_ args & body] (defined 'gen-args)]
          `(defmethod op/gen-args ~op-type
             ~(into ['op-type] args)
             ~(generator-body attr-vec body)))

       ~(when-let [[_ args & body] (defined 'can-generate?)]
          `(defmethod op/can-generate? ~op-type
             ~(into ['op-type] args)
             ~@body)))))


(defop ::wait
  [duration]

  (gen-args [_]
    (gen/tuple (gen/choose 1 100)))

  (call [system]
    (Thread/sleep duration)))


(defn- waitable-ops
  "Takes a function from state to vector of op generators and returns a
  new function which additionally returns the wait op as the first result"
  [op-generators]
  (comp (partial cons (op/gen-op ::wait nil)) op-generators))



;; ## Test Harness

(defn- gen-op-seq
  [init-state state->op-gen length]
  (reduce
    (fn [test-gen index]
      (gen/let [[state op-seq] test-gen
                op (state->op-gen state)]
        [(op/next-state op state (result/pending index)) (conj op-seq op)]))
    (gen/return [init-state []])
    (range length)))

(defn- gen-test-inputs
  "Create a generator for inputs to a system under test. This generator
  produces an initial state and a collection of sequences of operations
  generated from the initial state."
  [init-state-gen state->op-gen _parallel?]
  (gen/let [init-state init-state-gen
            ops-size (gen/choose 0 50)]
    (gen/tuple
      (gen/return init-state)
      (gen/fmap second
                (gen-op-seq init-state state->op-gen ops-size)))))


(defn- elapsed-since
  "Returns the number of milliseconds elapsed since an initial start time
  in system nanoseconds."
  [start]
  (/ (- (System/nanoTime) start) 1000000.0))


(defn- run-ops!
  "Construct a system, run a collection of op sequences on the system (possibly
  concurrently), and shut the system down. Returns a map from thread index to
  operations updated with results."
  [constructor finalize! op-seq state]
  (let [start (System/nanoTime)
        system (constructor)]
    (try
      (op/run-seq-ops! system state op-seq)
      (finally
        (when finalize!
          (finalize! system))
        (ctest/do-report
          {:type ::report/run-ops
           :op-count (count op-seq)
           :elapsed (elapsed-since start)})))))


(defn- run-test!
  "Runs a generative test iteration. Returns a test result map."
  [constructor finalize! state op-seq]
  (ctest/do-report
    {:type ::report/test-start})
  (let [result (run-ops! constructor finalize! op-seq state)]
    (ctest/do-report result)
    result))


(defn- run-trial!
  "Run a generative trial involving multiple test repetitions."
  [runner-fn op-seq]
  (let [start (System/nanoTime)
        repetitions 1]
    (ctest/do-report
      {:type ::report/trial-start
       :op-count (count op-seq)})
    (loop [i 0
           result nil]
      (if (== repetitions i)
        (do (ctest/do-report
              {:type ::report/trial-pass
               :repetitions repetitions
               :elapsed (elapsed-since start)})
            result)
        (let [result (runner-fn op-seq)]
          (if (:world result)
            (recur (inc i) result)
            (do (ctest/do-report
                  {:type ::report/trial-fail
                   :repetition (inc i)
                   :elapsed (elapsed-since start)})
                result)))))))


(defn- report-test-summary
  "Emit clojure.test reports for the summarized results of the generative
  tests."
  [summary]
  (if (and (:result summary) (not (instance? Throwable (:result summary))))
    (ctest/report
      (assoc summary :type ::report/summary))
    (ctest/report
      (assoc summary
             :type ::report/shrunk
             :shrunk-result (::check/result (meta (get-in summary [:shrunk :smallest])))))))

(defn- create-ops-gen
  [op-types]
  (fn [state]
    (gen/bind
      (gen/elements
        (filter #(op/can-generate? % state) op-types))
      (fn [op-type]
        (op/gen-op op-type state)))))

(defn create-model
  [new-system op-types
   & {:keys [state-gen finalize!]
      :or {state-gen (gen/return {})}}]
  {:pre [(fn? new-system)]}
  (let [ops-gen (create-ops-gen op-types)]
    {:state-gen state-gen
     :new-system new-system
     :ops-gen ops-gen
     :finalize! finalize!}))

(defn gen-test-case
  "Create a test-case generator for the given model."
  [model & {:keys [parallel?]}]
  (let [{:keys [state-gen ops-gen]} model
        ops-gen (cond-> ops-gen
                  parallel? (waitable-ops))]
    (gen-test-inputs state-gen ops-gen parallel?)))

(defn check-test-case
  "Given a model and a generated test case, executes the test case returning
  the result of test execution."
  [model [state op-seq]]
  (let [{:keys [new-system finalize!]} model]
    (let [constructor (fn system-constructor
                        []
                        (try
                          (new-system state)
                          (catch ArityException ae
                            (new-system))))]
      (run-trial!
        (partial run-test!
                 constructor
                 finalize!
                 state)
        op-seq))))

(defn check-system
  "Uses generative tests no validate the behavior of a system under a sequence
  of operations dictated by a state machine model.

  Takes a test message, a model configuration object, and the following options
  which control the behavior of the tests:

  - `parallel?`
    If truthy will generate and run parallel tests to check for race conditions.
  - `report`
    Options to override the default report configuration."
  [message iteration-opts model & {:keys [parallel? report]}]
  (ctest/testing message
    (binding [report/*options* (merge report/*options* report)]
      (report-test-summary
        (check/check-and-report
          iteration-opts
          (gen-test-case model :parallel? parallel?)
          (partial check-test-case model))))))
