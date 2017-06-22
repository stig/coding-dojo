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

(def max-discount-books
  (->> discounts keys sort last))

(defn partition-patterns
  "Candidates of book partitions from a set number of books."
  [n]
  (->> (repeat n 1)
       combo/partitions
       (map #(map count %))
       (remove #(> (first %) max-discount-books))))

(defn price-partitions
  "Price collection of book partitions"
  [parts]
  (->> parts
       (map price-partition)
       (reduce +)))

(defn sorted-price-patterns
  "Zip prices & associated patterns together,
  and sort by price so that best pattern comes first."
  [prices patterns]
  (->> (map vector prices patterns)
       (sort-by first)))

(defn pick-books
  "Pick books according to indices given; return
  remaining stacks of books."
  [books indices]
  ;;  (println "pick-books:" books indices)
  (if (empty? indices)
    (remove zero? books)
    (recur (update-in (vec books) [(first indices)] dec)
           (rest indices))))

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
          patterns (partition-patterns n)
          prices (map price-partitions patterns)
          price-patterns (sorted-price-patterns prices patterns)
          piles-of-distinct-books (-> books frequencies vals)]
      (loop [[[pric pattern] & rest] price-patterns]
        (if (pattern-match? pattern piles-of-distinct-books)
          pric
          (recur rest))))))
