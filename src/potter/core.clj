(ns potter.core
  (:require [clojure.math.combinatorics :as combo]))

(def discount {2 0.95
               3 0.9
               4 0.8
               5 0.75})

(defn price-partition
  [books]
  (let [unique (count (distinct books))
        mult (or (discount unique) 1)]
    (* 8
       (+ (* unique mult)
          (* (- (count books) unique))))))

(defn price-partition-seq
  [partitions]
  (->> partitions
       (map price-partition)
       (reduce +)))

(defn price
  [books]
  (->> books
       combo/partitions
       (map price-partition-seq)
       sort
       first))
