(ns potter.core
  (:require [clojure.math.combinatorics :as combo]))

(def book-price 8)

(def discounts
  {2 5/100
   3 10/100
   4 20/100
   5 25/100})

(defn price-partition
  "Price a partition of N distinct books."
  [n]
  (let [discount (or (discounts n) 0)
        multiplier (- 1 discount)]
    (* book-price n multiplier)))

(defn sum-price-partitions
  "Calculate the sum of a sequence of book partitions."
  [parts]
  (->> parts
       (map price-partition)
       (reduce +)))

(defn max-partition-size
  "Given a map of discounts picks the max partition size to consider."
  [discounts]
  (->> discounts keys sort last))

(defn partitions
  "Produce all sequence of possible partitions representing N number
  of books, constrained by a max size for each partition."
  [n max-part-size]
  (->> (repeat n 1)
       combo/partitions
       (map #(map count %))
       (remove #(> (first %) max-part-size))))

(defn sort-partitions-by-price
  "Zip sequences of prices & partitions together,
  and sort by price so the cheapest sequence of partitions comes
  first."
  [prices parts]
  (->> (map vector prices parts)
       (sort-by first)))

(defn pick-books
  "Pick books from the given stacks according to indices given; return
  remaining stacks of books."
  [stacks indices]
  (loop [stacks (vec stacks) ;; update-in *really* doesn't like seqs, hence this hack
         [x & xs] indices]
    (if-not x
      (remove zero? stacks)
      (recur (update-in stacks [x] dec) xs))))

(defn pick-combinations
  "All the unique ways to pick N books from a set of stacks."
  [stacks n]
  (combo/combinations (range (count stacks)) n))

(defn picks-completely?
  "Is it possible to pick the given partitions from the stacks of books,
  such that all the stacks are used up?"
  [parts stacks]
  (loop [stacks stacks
         potential-picks (pick-combinations stacks (first parts))
         remaining-parts (rest parts)
         backtrack-stack []]

    ;; Have we reached a dead end?
    (if (empty? potential-picks)

      ;; Can we backtrack to try a different path?
      (if (empty? backtrack-stack)
        false
        (let [prev (peek backtrack-stack)
              stacks (nth prev 0)
              potential-picks (nth prev 1)
              parts (nth prev 2)]
          (recur stacks
                 (rest potential-picks)
                 parts
                 (pop backtrack-stack))))

      (let [remaining-stacks (pick-books stacks (first potential-picks))]
        ;; Have we depleted our stacks of books?
        (if (empty? remaining-stacks)
          true
          (recur remaining-stacks
                 (pick-combinations remaining-stacks (first remaining-parts))
                 (rest remaining-parts)
                 (conj backtrack-stack [stacks potential-picks remaining-parts])))))))

(defn price
  "Calculates the best price you can get for a collection of books,
  by splitting it into different partitions and getting the optimal
  discount achievable."
  [books]
  (if (empty? books)
    0
    (let [n (count books)
          max-part-size (max-partition-size discounts)
          parts (partitions n max-part-size)
          prices (map sum-price-partitions parts)
          price-parts (sort-partitions-by-price prices parts)
          stacks (-> books frequencies vals)]
      (loop [[[price parts] & rest] price-parts]
        (if (picks-completely? parts stacks)
          price
          (recur rest))))))
