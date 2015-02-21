(ns micro-kanren.core
  (:refer-clojure :only [defrecord defn instance? first rest filter = nil?
                         list identical? let and fn cons cond inc fn? empty?]))

;; A Binding represents a logic variable binding in a substitution, i.e., a
;; substitution is a sequence or logic variable bindings.
(defrecord Binding [var val])
(defn binding? [x] (instance? Binding x))
(defn lvar-binding
  "Returns the Binding of LVar `num` in substitution s."
  [num s]
  (first (filter #(= num (:num (:var %))) s)))

;; A State is a substitution paired with a fresh variable counter.  Then a
;; Stream is a sequence of States which might be empty, or it is an immature
;; stream (state thunk) where state is a State and thunk is a thunk that
;; computes the remainder of states.
(defrecord State [subst counter])
(def empty-state (->State '() 0))

;; We represent logic vars using a separate record type instead of as vectors
;; as does the original Scheme implementation.
(defrecord LVar [num])
(defn lvar? [x] (instance? LVar x))
(defn lvar=? [v1 v2]
  (= (:num v1) (:num v2)))

(defn walk
  [u s]
  (let [pr (and (lvar? u)
                (first (filter (fn [v] (lvar=? u (:var v))) s)))]
    (if pr (walk (:val pr) s) u)))

(defn ext-s [x v s]
  (cons (->Binding x v) s))

(defn unify [u v s]
  (let [u (walk u s)
        v (walk v s)]
    (cond
      (and (lvar? u) (lvar? v) (lvar=? u v)) s
      (lvar? u) (ext-s u v s)
      (lvar? v) (ext-s v u s)
      (and (binding? u) (binding? v)) (let [s (unify (:var u) (:var v) s)]
                                        (and s (unify (:val u) (:val v) s)))
      :else (and (= u v) s))))

(def mzero '())

(defn unit [s-c] (cons s-c mzero))

(defn == [u v]
  (fn [s-c]
    (let [s (unify u v (:subst s-c))]
      (if s
        (unit (->State s (:counter s-c)))
        mzero))))

(defn call-fresh [f]
  (fn [s-c]
    (let [c (:counter s-c)]
      ((f (->LVar c)) (->State (:subst s-c) (inc c))))))

(defn mplus [$1 $2]
  (cond
    (fn? $1) (fn [] (mplus $2 ($1)))
    (empty? $1) $2
    :else (cons (first $1) (let [x (mplus (rest $1) $2)]
                             (if (fn? x) (list x) x)))))

(defn bind [$ g]
  (cond
    (fn? $) (fn [] (bind ($) g))
    (empty? $) mzero
    :else (mplus (g (first $)) (bind (rest $) g))))

(defn disj [g1 g2]
  (fn [s-c]
    (mplus (g1 s-c) (g2 s-c))))

(defn conj [g1 g2]
  (fn [s-c]
    (bind (g1 s-c) g2)))
