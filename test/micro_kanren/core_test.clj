(ns micro-kanren.core-test
  (:refer-clojure :exclude [== disj conj])
  (:require [clojure.test :refer :all]
            [micro-kanren.core :refer :all]))

(deftest test-1
  (let [r ((call-fresh (fn [q] (== q 5))) empty-state)]
    (is (= 1 (count r)))
    (let [state (first r)
          subst (:subst state)]
      (is (= 1 (:counter state)))
      (is (= 1 (count subst)))
      (is (= 5 (:val (var-val 0 subst)))))))

(def a-and-b
  (conj
   (call-fresh (fn [a] (== a 7)))
   (call-fresh (fn [b] (disj (== b 5) (== b 6))))))

(deftest test-2
  (let [r (a-and-b empty-state)]
    (is (= 2 (count r)))
    (let [state1 (first r)
          state2 (second r)
          subst1 (:subst state1)
          subst2 (:subst state2)]
      (is (= 2 (:counter state1)))
      (is (= 2 (:counter state2)))
      (is (= 2 (count subst1)))
      (is (= 2 (count subst2)))
      (is (= 7 (:val (var-val 0 subst1))))
      (is (= 5 (:val (var-val 1 subst1))))
      (is (= 7 (:val (var-val 0 subst2))))
      (is (= 6 (:val (var-val 1 subst2)))))))

(defn fives [x]
  (disj (== x 5)
        (fn [s-c]
          (fn []
            ((fives x) s-c)))))

(deftest test-3
  (loop [r ((call-fresh fives) empty-state), i 0]
    (is (= 2 (count r)))
    (let [state1 (first r)
          subst1 (:subst state1)
          state2 (second r)]
      (is (= 1 (count subst1)))
      (is (= 5 (:val (var-val 0 subst1))))
      (is (fn? state2))
      (when (< i 10)
        (recur (state2) (inc i))))))
