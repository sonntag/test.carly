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
      [search :as search]))
  (:import (clojure.lang ArityException)))


;; ## Test Operation Definition

(defn- generator-body
  "Macro helper to build a generator constructor."
  [op-name body]
  `(gen/fmap
     (fn [op-args#]
       (if (vector? op-args#)
         (apply ~(symbol (str "->" (name op-name))) op-args#)
         (~(symbol (str "map->" (name op-name))) op-args#)))
     (do ~@body)))


(defmacro defop
  "Defines a new specification for a system operation test."
  [op-name attr-vec & forms]
  (let [defined (zipmap (map first forms) forms)]
    (when-let [unknown-forms (seq (dissoc defined 'gen-args 'apply-op 'check 'update-model))]
      (throw (ex-info "Unknown forms defined in operation body"
                      {:unknown (map first unknown-forms)})))
    `(do
       (defrecord ~op-name
         ~attr-vec

         op/TestOperation

         ~(or (defined 'apply-op)
              '(apply-op [op system] nil))

         ~(if-let [[sym args & body] (defined 'check)]
            (list sym args (report/wrap-report-check body))
            '(check [op state result] true))

         ~(or (defined 'update-model)
              '(update-model [op state] state)))

       (defn ~(symbol (str "gen->" (name op-name)))
         ~(str "Constructs a " (name op-name) " operation generator.")
         ~@(if-let [[_ args & body] (defined 'gen-args)]
             [args (generator-body op-name body)]
             [['state]
              `(gen/return (~(symbol (str "->" (name op-name)))))])))))


(defop Wait
  [duration]

  (gen-args
    [_]
    (gen/tuple (gen/choose 1 100)))

  (apply-op
    [this system]
    (Thread/sleep duration)))


(defn- waitable-ops
  "Takes a function from state to vector of op generators and returns a
  new function which additionally returns the wait op as the first result"
  [op-generators]
  (comp (partial cons (gen->Wait nil)) op-generators))



;; ## Test Harness

(defn- gen-test-inputs
  "Create a generator for inputs to a system under test. This generator
  produces an initial state and a collection of sequences of operations
  generated from the initial state."
  [init-state-gen ctx->op-gens max-concurrency]
  (gen/bind
    (gen/tuple
      init-state-gen
      (if (<= max-concurrency 1)
        (gen/return 1)
        (gen/choose 1 max-concurrency)))
    (fn [[init-state concurrency]]
      (gen/tuple
        (gen/return init-state)
        (-> (ctx->op-gens init-state)
            (gen/one-of)
            (gen/list)
            (gen/not-empty)
            (gen/vector concurrency))))))


(defn- elapsed-since
  "Returns the number of milliseconds elapsed since an initial start time
  in system nanoseconds."
  [start]
  (/ (- (System/nanoTime) start) 1000000.0))


(defn- run-ops!
  "Construct a system, run a collection of op sequences on the system (possibly
  concurrently), and shut the system down. Returns a map from thread index to
  operations updated with results."
  [constructor finalize! op-seqs]
  (let [start (System/nanoTime)
        system (constructor)]
    (try
      (case (count op-seqs)
        0 op-seqs
        1 {0 (op/apply-ops! system (first op-seqs))}
          (op/run-threads! system op-seqs))
      (finally
        (when finalize!
          (finalize! system))
        (ctest/do-report
          {:type ::report/run-ops
           :op-count (reduce + 0 (map count op-seqs))
           :concurrency (count op-seqs)
           :elapsed (elapsed-since start)})))))


(defn- run-test!
  "Runs a generative test iteration. Returns a test result map."
  [constructor finalize! state thread-count op-seqs]
  (ctest/do-report
    {:type ::report/test-start})
  (let [op-results (run-ops! constructor finalize! op-seqs)
        result (search/search-worldlines thread-count state op-results)]
    (ctest/do-report
      (assoc result :type (if (:world result)
                            ::report/test-pass
                            ::report/test-fail)))
    (assoc result :op-results op-results)))


(defn- run-trial!
  "Run a generative trial involving multiple test repetitions."
  [repetitions runner-fn op-seqs]
  (let [start (System/nanoTime)]
    (ctest/do-report
      {:type ::report/trial-start
       :repetitions repetitions
       :concurrency (count op-seqs)
       :op-count (reduce + 0 (map count op-seqs))})
    (loop [i 0
           result nil]
      (if (== repetitions i)
        (do (ctest/do-report
              {:type ::report/trial-pass
               :repetitions repetitions
               :elapsed (elapsed-since start)})
            result)
        (let [result (runner-fn op-seqs)]
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


(defn check-system
  "Uses generative tests to validate the behavior of a system under a linear
  sequence of operations.

  Takes a test message, a single-argument constructor function which takes the
  state and produces a new system for testing, and a function which will
  return a vector of operation generators when called with the initial state.
  The remaining options control the behavior of the tests:

  - `:init-state-gen`
    Generator for a fresh state.
  - `finalize!`
    Called with the system after running all operations. This function may
    contain additional test assertions and should clean up any resources by
    stopping the system.
  - `concurrency`
    Maximum number of operation threads to run in parallel.
  - `repetitions`
    Number of times to run per generation to ensure repeatability.
  - `search-threads`
    Number of threads to run to search for valid worldlines.
  - `report`
    Options to override the default report configuration."
  [message
   iteration-opts
   init-system
   ctx->op-gens
   & {:keys [init-state-gen finalize!
             concurrency repetitions search-threads]
      :or {init-state-gen (gen/return {})
           concurrency 4
           repetitions 5
           search-threads (. (Runtime/getRuntime) availableProcessors)}
      :as opts}]
  {:pre [(fn? init-system) (fn? ctx->op-gens)]}
  (ctest/testing message
    (binding [report/*options* (merge report/*options* (:report opts))]
      (report-test-summary
        (check/check-and-report
          iteration-opts
          (gen-test-inputs
            init-state-gen
            (cond-> ctx->op-gens
              (< 1 concurrency) (waitable-ops))
            concurrency)
          (fn [state op-seqs]
            (let [constructor (fn system-constructor
                                []
                                (try
                                  (init-system state)
                                  (catch ArityException ae
                                    (init-system))))]
              (run-trial!
                repetitions
                (partial run-test!
                         constructor
                         finalize!
                         state
                         search-threads)
                op-seqs))))))))
