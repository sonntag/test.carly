(ns test.carly.op
  "Test operations and related functionality."
  (:require
    [clojure.spec.alpha :as s]
    [clojure.test.check.generators :as gen]
    [test.carly.report :as report]
    [test.carly.result :as result]))

(s/def ::type keyword?)

(s/def ::args (s/map-of keyword? any?))

(s/def ::operation
  (s/keys :req [::type]))

;; Registry of all defined operations for convenience.
;; Keys are the op-type and vals are the doc-strings

(defonce op-registry (atom {}))

(defn register [op-type doc-str]
  (swap! op-registry assoc op-type doc-str))

;; Multimethods defined on all operations

(defmulti call
  "Apply the operation to the system, returning a result value."
  (fn [op _system] (::type op)))

(defmulti check
  "Validate an operation given the model state and the response from the
  system being tested. May include `clojure.test/is` assertions, and should
  return a boolean value indicating overall success or failure."
  (fn [op _state _result] (::type op)))

(defmulti next-state
  "Apply the operation to the model state, returning the next state."
  (fn [op _state _result] (::type op)))

(defmulti gen-args
  "Creates a generator for the arguments of the given operator type using
  the given state."
  (fn [op-type _state] op-type))

(defmulti gen-op
  "Creates a generator for the given operator type using the given state."
  (fn [op-type _state] op-type))

(defmulti can-generate?
  "Predicate to determine if the op-type can be generated in the current
  state."
  (fn [op-type _state] op-type))


;; Default multimethod implementations

(defmethod call :default
  [_op _system])

(defmethod check :default
  [_op _state _result]
  true)

(defmethod next-state :default
  [_op state _result]
  state)

(defmethod gen-args :default
  [_op-type _state]
  (gen/return {}))

(defmethod gen-op :default
  [op-type state]
  (gen/fmap
    (fn [args] (merge args {::type op-type}))
    (gen-args op-type state)))

(defmethod can-generate? :default
  [_op-type _state]
  true)


;; Functions for operation execution

(defn- concretize
  "Inspects the operation for any pending results, and if it exists replaces
  it with a new result containing the concrete value."
  [op prev-ops]
  (into {}
    (map (fn [[k v]]
           (if (result/pending? v)
             [k (::result (get prev-ops (result/index v)))]
             [k v])))
    op))

(defn- apply-op!
  "Applies the given op (which should be concrete) to the given system,
  returning a result value with the result of the computation."
  [system op]
  (try
    (result/success (call op system))
    (catch Throwable ex
      (result/failed ex))))

(defn apply-ops!
  "Apply a sequence of operations to a system, returning a vector of pairs of
  the operations with their results."
  [system ops]
  (reduce
    (fn [prev-ops op]
      (let [op (concretize op prev-ops)
            res (apply-op! system op)]
        (conj prev-ops (assoc op ::result res))))
    []
    ops))


(defn run-seq-ops!
  "Runs an op sequence on the given system and initial state. The state is checked
  in-between each op application, short-circuiting if a failure is encountered.
  Returns a result map containing the final state."
  [system initial-state op-seq]
  (loop [prev-ops []
         next-ops op-seq
         state initial-state]
    (if (seq next-ops)
      (let [op (concretize (first next-ops) prev-ops)
            res (apply-op! system op)]
        (if (check op state res)
          (recur (conj prev-ops (assoc op ::result res))
                 (rest next-ops)
                 (next-state op state res))
          {:type ::report/test-fail
           :op-results prev-ops
           :final-state state}))
      {:type ::report/test-pass
       :op-results prev-ops
       :final-state state})))

(defn run-ops!
  "Applies a sequence of operations in a separate thread. Returns a promise for
  the results of the application."
  [latch system thread-id ops]
  (future @latch (apply-ops! system ops)))


(defn run-threads!
  "Run each of the given operation sequences in a separate thread. Returns a
  vector of the operation results for each thread."
  [system op-seqs]
  (let [latch (promise)
        threads (map (partial run-ops! latch system) (range) op-seqs)]
    (dorun threads)
    (deliver latch :start)
    ; TODO: timeout on deref?
    (->> (map deref threads)
         (map vector (range))
         (into {}))))
