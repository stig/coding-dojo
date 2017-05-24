(ns potter.core)

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

  ;; Separate the books into piles of individual books
  (loop [book-piles (->> books frequencies vals sort)
         total 0]

    ;; Any more piles of books left?
    (if (seq book-piles)

      ;; Do we hit the special case where two four-book stacks are
      ;; cheaper than two stacks of three and five unique books each?
      (if (= '(1 1 2 2 2) book-piles)

        ;; Return current total plus the cost of two stacks of four
        ;; unique books.
        (+ total (* 2 (fast-price-lookup 4)))

        ;; Take one book from each remaining pile & add the cost of
        ;; this stack of books to the running total.
        (recur (->> book-piles (map dec) (remove zero?))
               (+ total (fast-price-lookup (count book-piles)))))

      ;; No more piles of books left; return total.
      total)))
