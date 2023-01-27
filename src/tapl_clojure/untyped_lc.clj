(ns tapl-clojure.untyped-lc
  (:require [clojure.core.match :refer [match]])
  (:refer-clojure :exclude [eval]))

(defn term-walk [on-var c t]
  (let [walk (fn walk [c t]
               (match t
                 [:term/var x  t]   (on-var c x t)
                 [:term/abs x  t']  [:term/abs x (walk c t')]
                 [:term/app t' t''] [:term/app (walk c t') (walk c t'')]))]
    (walk c t)))

(defn term-shift-above [d c t]
  (term-walk
   (fn [c x n]
     [:term/var (cond-> x (<= c x) (+ d)) (+ n d)])
   c t))

(defn term-shift [d t]
  (term-shift-above d 0 t))

(defn term-sub [j s t]
  (term-walk
   (fn [c x n]
     (if (= x (+ j c))
       (term-shift c s)
       [:term/var x n]))
   0 t))

(defn term-sub-top [s t]
  (term-shift (- 1) (term-sub 0 (term-shift 1 s) t)))

(defn is-val? [x & _]
  (identical? x :term/abs))

(defn eval1 [ctx t]
  (match t
    ([:term/app [:term/abs _ t12] v] :guard (comp is-val? last))   (term-sub-top v t12)
    ([:term/app _ _]                 :guard (comp is-val? second)) (update t 2 (partial eval1 ctx))
    [:term/app  _ _]                                               (update t 1 (partial eval1 ctx))
    :else                                                          (throw (ex-info "No rule applies" {::term t})))  )

(defn eval [ctx t]
  (let [ctx (into (sorted-map) ctx)]
    (try
      (let [t' (eval1 ctx t)]
        (eval ctx t'))
      (catch clojure.lang.ExceptionInfo e
        (->> e ex-data ::term)))))
