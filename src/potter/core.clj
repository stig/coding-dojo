(ns potter.core
  (:require [clojure.math.combinatorics :as combo]))

(def discounts {2 5/100
                3 10/100
                4 20/100
                5 25/100})

(defn multiplier [n]
  (- 1 (or (discounts n) 0)))

(def book-price 8)

(defn price-partition
  [books]
  (let [unique (count (distinct books))
        mult (multiplier unique)]
    (* book-price
       (+ (* unique mult)
          (* (- (count books) unique))))))

(defn price-partition-seq
  [partitions]
  (->> partitions
       (map price-partition)
       (reduce +)))

(defn best-price
  [books]
  (->> books
       combo/partitions
       (map price-partition-seq)
       sort
       first))

(defn fast-price
  [books]
  (loop [counts (->> books frequencies vals sort)
         total 0]
    (if (seq counts)
      (let [c (first counts)
            n (count counts)
            p (* c (price-partition (take n (iterate inc 1))))]
        (recur (->> counts
                    (map #(- % c))
                    (remove zero?))
               (+ total p)))
      total)))

(defn price
  [books]
  (if (< (count books) 10)
    (best-price books)
    (fast-price books)))
