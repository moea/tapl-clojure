(ns tapl-clojure.arith
  (:require [instaparse.core :as insta]
            [clojure.walk
             :refer [postwalk]]
            [clojure.core.match
             :refer [match]]))

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
  (if (keyword? x)
    (num-val? x)
    (recur (last x))))

(defn eval1 [t]
  ;; This is the "small step" (i.e. one-step) evaluator implemented in the book,
  ;; after a discusssion of operational semantics.  It's beyond useless.
  (match t
    [:if :true  t1 _]    t1
    [:if :false _  t2]   t2
    [:if t1     t2 t3]   [:if (eval1 t1) t2 t3]
    [:succ t1]           [:succ (eval1 t1)]
    [:pred :zero]        :zero
    ([:pred :succ nv1]   :<< nested-num?) nv1
    [:pred t1]           [:pred (eval1 t1)]
    [:iszero :zero]      :true
    ([:iszero :succ _]   :<< nested-num?) :false
    [:iszero t1]         [:iszero (eval1 t1)]))
