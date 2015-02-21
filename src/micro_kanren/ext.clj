(ns micro-kanren.ext
  (:refer-clojure :only [defn defmacro for first vec rest fn fn? seq map take
                         cond let symbol str count cons list empty? zero? dec
                         loop < inc conj]
                  :rename {take clj-take})
  (:require [micro-kanren.core :as c]))

;;# Recovering miniKanren's control operators

(defmacro Zzz
  "Snoozes the given goal `g` by performing the inverse-η-delay."
  [g]
  `(fn [s-c#]
     (fn []
       (~g s-c#))))

(defmacro conj+
  "Like `conj` but works for one or many goals."
  ([g] `(Zzz ~g))
  ([g1 g2 & more]
   `(c/conj (Zzz ~g1) (conj+ ~g2 ~@more))))

(defmacro disj+
  "Like `disj` but works for one or many goals."
  ([g] `(Zzz ~g))
  ([g1 g2 & more]
   `(c/disj (Zzz ~g1) (disj+ ~g2 ~@more))))

(defmacro conde
  "The `disj+` of the `conj+` of the given `clauses`.
  Every clause is a sequence of goals."
  [& clauses]
  `(disj+ ~@(for [c clauses]
              `(conj+ ~@c))))

(defmacro fresh
  "Introduces the fresh `vars` (a vector of symbols) and then performs the
  `conj+` of `goals`."
  [vars & goals]
  (if (seq vars)
    `(c/call-fresh (fn [~(first vars)]
                     (fresh ~(vec (rest vars))
                       ~@goals)))
    `(conj+ ~@goals)))

;;# From streams to lists

(defn pull
  "If `$` is an immature stream, evaluate and pull the result.  Else return
  `$`."
  [$]
  (if (fn? $) (pull ($)) $))

(defn take-all
  "Lazily pulls all states from stream `$`."
  [$]
  (let [$ (pull $)]
    (if (empty? $)
      '()
      (cons (first $) (take-all (rest $))))))

(defn take
  "Takes the `n` first states from the stream `$`."
  [n $]
  (if (zero? n)
    '()
    (let [$ (pull $)]
      (if (empty? $)
        '()
        (cons (first $) (take (dec n) (rest $)))))))

;;# Recovering reification

(defn ^:private walk* [v s]
  (let [v (c/walk v s)]
    (cond
      (c/lvar? v) v
      (c/binding? v) (c/->Binding (walk* (:var v) s)
                                  (walk* (:val v) s))
      :else v)))

(defn ^:private reify-name [n]
  (symbol (str "_." n)))

(defn ^:private reify-s [v s]
  (let [v (c/walk v s)]
    (cond
      (c/lvar? v) (let [n (reify-name (count s))]
                    (cons (c/->Binding v n) s))
      (c/binding? v) (reify-s (:val v) (reify-s (:var v) s))
      :else s)))

(defn ^:private reify-state-all-vars [s-c]
  (let [s (:subst s-c)]
    (loop [n (dec (count s)), r []]
      (if (< n 0)
        r
        (let [v (walk* (c/->LVar n) s)]
          (recur (dec n) (cons (walk* v (reify-s v '())) r)))))))

(defn mK-reify
  "Reifies the list of states `s-c*` by reifying each state's substitution
  wrt. the first variable."
  [s-c*]
  (map reify-state-all-vars s-c*))

;;# Recovering the interface to Clojure

(defn call-empty-state [g]
  (g c/empty-state))

(defmacro run
  [n vars & gs]
  `(mK-reify (take ~n (call-empty-state
                       (fresh ~vars
                         ~@gs)))))

(defmacro run*
  [vars & gs]
  `(mK-reify (take-all (call-empty-state
                        (fresh ~vars
                          ~@gs)))))
