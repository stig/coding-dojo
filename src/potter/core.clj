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
  such that all the stacks are used up and we don't have partitions
  left to pick?"
  [parts stacks]
  (loop [stacks stacks
         combinations (pick-combinations stacks (first parts))
         patterns (rest parts)
         backtrack-info []]
    (if (empty? combinations)
      (if (empty? backtrack-info)
        false
        (let [prev (peek backtrack-info)
              b (nth prev 0)
              c (nth prev 1)
              p (nth prev 2)]
          ;; Backtrack trying alternative combinations
          (recur b (rest c) p (pop backtrack-info))))
      (let [remaining (pick-books stacks (first combinations))]
        (if (empty? remaining)
          true
          (recur remaining
                 (pick-combinations remaining (first patterns))
                 (rest patterns)
                 (conj backtrack-info [stacks combinations patterns])))))))

(defn price
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
