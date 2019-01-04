(ns test.carly.result
  (:require
    [clojure.pprint :as pp])
  (:import
    (clojure.lang IDeref)
    (java.io Writer)))

;; Super simple deferred results

(defrecord PendingResult [index]
  IDeref
  (deref [_]
    (throw (ex-info "Result values cannot be accessed during test case generation" {}))))

(defrecord SuccessResult [value]
  IDeref
  (deref [_] value))

(defrecord FailedResult [ex]
  IDeref
  (deref [_] (throw ex)))


;; Helper functions

(defn pending? [x] (instance? PendingResult x))
(defn success? [x] (instance? SuccessResult x))
(defn index [res] (:index res))

(defn pending [index] (->PendingResult index))
(defn success [value] (->SuccessResult value))
(defn failed [ex] (->FailedResult ex))


;; fix printing

(defmethod pp/simple-dispatch PendingResult
  [o]
  (str "<< Pending " (:index o) " >>"))

(defmethod print-method PendingResult
  [o ^Writer w]
  (print-method (str "<< Pending " (:index o) " >>") w))

(defmethod pp/simple-dispatch SuccessResult
  [o]
  (:value o))

(defmethod print-method SuccessResult
  [o ^Writer w]
  (print-method (:value o) w))

(defmethod pp/simple-dispatch FailedResult
  [o]
  (:ex o))

(defmethod print-method FailedResult
  [o ^Writer w]
  (print-method (:ex o) w))
