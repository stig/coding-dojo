(ns potter.core-test
  (:require [clojure.test :refer :all]
            [potter.core :refer :all]))

(deftest price-test
  (testing "no discounts"
    (are [total books]
        (= total (price books))
      0 []
      8 [1]
      16 [2 2]
      24 [3 3 3]
      32 [4 4 4 4]
      40 [5 5 5 5 5]))

  (testing "Simple discounts"
    (are [total books]
        (= total (price books))
      (* 16 0.95) [1 2]
      (* 16 0.95) [1 3]
      (* 16 0.95) [1 4]
      (* 16 0.95) [1 5]
      (* 8 3 0.9) [1 3 5]
      (* 8 4 0.8) [1 2 3 5]
      (* 8 5 0.75) [1 2 3 4 5]))

  (testing "Multiple discounts"
    (are [total books]
        (= total (price books))
      (+ 8 (* 2 8 0.95)) [1 1 2]
      (* 2 (* 2 8 0.95)) [1 1 2 2]
      (+ (* 8 4 0.8) (* 8 2 0.95)) [1 1 2 3 3 4]
      (+ 8 (* 5 8 0.75)) [1 2 2 3 4 5]))

  (testing "Edge cases"
    (are [total books]
        (= total (price books))
      (* 2 (* 8 4 0.8)) [1 1 2 2 3 3 4 5]
      (+ (* 3 (* 8 5 0.75)) (* 2 8 4 0.8)) [1 1 1 1 1
                                            2 2 2 2 2
                                            3 3 3 3
                                            4 4 4 4 4
                                            5 5 5 5])))
