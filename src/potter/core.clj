(ns potter.core
  (:require [clojure.math.combinatorics :as combo]))

(def book-price 8)

(def fast-price-lookup
  "Pre-calculated prices (with discount) for a stack of up to 5 distinct books.'"
  {1 book-price
   2 (* 2 book-price 95/100)
   3 (* 3 book-price 9/10)
   4 (* 4 book-price 8/10)
   5 (* 5 book-price 75/100)})

(defn price
  [books]
  (loop [counts (->> books frequencies vals sort)
         total 0]
    (if (seq counts)
      ;; Two four-book stacks are cheaper than two stacks of three and
      ;; five unique books each. Catch that special case here.
      (if (= '(1 1 2 2 2) counts)
        (+ total (* 2 (fast-price-lookup 4)))
        (recur (->> counts (map dec) (remove zero?))
               (+ total (fast-price-lookup (count counts)))))
      total)))
