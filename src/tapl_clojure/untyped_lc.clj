(ns tapl-clojure.untyped-lc
  (:require [clojure.core.match
             :refer [match]]
            [instaparse.core :as insta]
            [clojure.walk    :as walk])
  (:refer-clojure :exclude [eval]))

(def p
  (insta/parser
   "EXPR = abs | app | var
    var  = #'[\\w]+'
    abs  = <'(L'> var <'. '> EXPR <')'>
    app  = (<'('> (abs | app) (abs | app) <')'>) | (var var)"
   {:auto-whitespace :standard}))

(defn- parse [s]
  (->> s
       p
       (walk/postwalk
        (fn [form]
          (match form
            [:EXPR body]         body
            [:abs [:var v] body] [:abs v body]
            :else                form)))))

(defn- de-brujin
  ([expr d binds]
   (match expr
     [:abs v body] [:term/abs v (de-brujin body (inc d) (conj binds [d v]))]
     [:app l r]    [:term/app   (de-brujin l d binds) (de-brujin r d binds)]
     [:var v]      [:term/var
                    (- (count binds)
                       (some
                        (fn [[i v']]
                          (when (= v' v)
                            i))
                        (rseq binds))
                       1)
                    (count binds)]))
  ([expr]
   (de-brujin expr 0 [])))

(defn- mint-name [vname bindings]
  (if (some #{vname} bindings)
    (recur (str vname "'") bindings)
    vname))

(defn- un-brujin* [node binds]
  (match node
    [:term/abs vname body] (let [vname  (mint-name vname binds)
                                 vname' (symbol (str vname "."))]
                             (list 'L vname' (un-brujin* body (conj binds vname))))
    [:term/app l r]        (list (un-brujin* l binds) (un-brujin* r binds))
    [:term/var idx _]      (symbol (binds (- (count binds) idx 1)))
    :else                  node))

(defn- un-brujin [expr]
  (walk/prewalk str (un-brujin* expr [])))

(defn- rewrite-vars [f c t]
  (match t
    [:term/var x  t']  (f c x t')
    [:term/abs x  t']  [:term/abs x (rewrite-vars f (inc c) t')]
    [:term/app t' t''] [:term/app (rewrite-vars f c t') (rewrite-vars f c t'')]))

(defn- term-shift-above [d c t]
  (rewrite-vars
   (fn [c x n]
     [:term/var (cond-> x (<= c x) (+ d)) (+ n d)])
   c t))

(defn- term-shift [d t]
  (term-shift-above d 0 t))

(defn- term-sub [j s t]
  (rewrite-vars
   (fn [c x n]
     (if (= x (+ j c))
       (term-shift c s)
       [:term/var x n]))
   0 t))

(defn- term-sub-top [s t]
  (term-shift (- 1) (term-sub 0 (term-shift 1 s) t)))

(defn- is-abs? [x]
  (and (vector? x) (identical? (first x) :term/abs)))

(defn- eval1 [ctx t]
  (match t
    ([:term/app [:term/abs _ t12] v] :guard (comp is-abs? last))   (term-sub-top v t12)
    ([:term/app _ _]                 :guard (comp is-abs? second)) (update t 2 (partial eval1 ctx   ))
    [:term/app  _ _]                                               (update t 1 (partial eval1 ctx))
    :else                                                          (throw (ex-info "No rule applies" {::term t}))))

(defn- eval' [ctx t]
  (recur ctx (eval1 ctx t)))

(defn eval
  ([ctx t]
   (let [t   (cond-> t (string? t) (-> parse de-brujin))
         res (try
               (eval' ctx t)
               (catch clojure.lang.ExceptionInfo e
                 (->> e ex-data ::term)))]
     (un-brujin res)))
  ([t]
   (eval {} t)))
