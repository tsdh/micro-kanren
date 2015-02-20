(ns micro-kanren.ext
  (refer-clojure :exclude [==])
  (:require [micro-kanren.core :as c]))

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
