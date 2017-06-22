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

(defn book-combinations
  "All the different combinations of indices to pick N books from a
  collection of indexed piles of books."
  [books n]
  ;;  (println "book-combinations:" books n)
  (combo/combinations (range (count books)) n))

(defn pattern-match?
  "Can we match this pattern to the books we are pricing?"
  [patterns books]
  (loop [books books
         combinations (book-combinations books (first patterns))
         patterns (rest patterns)
         stack []]
    (if (empty? combinations)
      (if (empty? stack)
        false
        (let [prev (peek stack)
              b (nth prev 0)
              c (nth prev 1)
              p (nth prev 2)]
          ;; Backtrack trying alternative combinations
          (recur b (rest c) p (pop stack))))
      (let [remaining (pick-books books (first combinations))]
        (if (empty? remaining)
          true
          (recur remaining
                 (book-combinations remaining (first patterns))
                 (rest patterns)
                 (conj stack [books combinations patterns])))))))

(defn price
  [books]
  (if (empty? books)
    0
    (let [n (count books)
          max-part-size (max-partition-size discounts)
          parts (partitions n max-part-size)
          prices (map sum-price-partitions parts)
          price-parts (sort-partitions-by-price prices parts)
          piles-of-distinct-books (-> books frequencies vals)]
      (loop [[[cost parts] & rest] price-parts]
        (if (pattern-match? parts piles-of-distinct-books)
          cost
          (recur rest))))))
