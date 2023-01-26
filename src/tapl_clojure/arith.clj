(ns tapl-clojure.arith
  (:require [instaparse.core :as insta]
            [clojure.walk
             :refer [postwalk]]
            [clojure.core.match
             :refer [match]])
  (:refer-clojure :exclude [eval]))

(def p
  (insta/parser
   "TERM   = true | false | if | zero | succ | pred | iszero
    true   = <'true'>
    false  = <'false'>
    if     = <'if'> TERM TERM TERM
    zero   = <'0'>
    succ   = <'succ'>   TERM
    pred   = <'pred'>   TERM
    iszero = <'iszero'> TERM"
   {:auto-whitespace :standard}))

(let [singleton? #{:zero :true :false}]
  (defn parse [s]
    (->> s
         p
         (postwalk
          (fn [x]
            (if-not (vector? x)
              x
              (let [h (first x)]
                (cond
                  (= h :TERM)    (second x)
                  (singleton? h) h
                  :else          x))))))))

(defn num-val? [t]
  (match [t]
    [:zero]     true
    [[:succ v]] (recur v)
    :else       false))

(defn nested-num? [x]
  (if (vector? x)
    (recur (last x))
    (num-val? x)))

(defn eval1 [t]
  ;; This is the "small step" (i.e. one-step) evaluator implemented in the book,
  ;; after a discusssion of operational semantics.
  (match t
    [:if :true  t1 _]    t1
    [:if :false _  t2]   t2
    [:if t1     t2 t3]   [:if (eval1 t1) t2 t3]
    [:succ t1]           [:succ (eval1 t1)]
    [:pred :zero]        :zero
    ([:pred :succ nv1]   :guard nested-num?) nv1
    [:pred t1]           [:pred (eval1 t1)]
    [:iszero :zero]      :true
    ([:iszero :succ _]   :guard nested-num?) :false
    [:iszero t1]         [:iszero (eval1 t1)]
    :else                (throw (ex-info "No match" {::term t}))))

(defn eval [t]
  (try
    (let [t1 (eval1 t)]
      (eval t1))
    (catch clojure.lang.ExceptionInfo e
      (-> e ex-data ::term))))
