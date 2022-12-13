(ns aoc22.day13-test
  (:require [aoc22.day13 :refer :all]
            [clojure.test :refer :all]))

(deftest check-order-test
  (is (= true (check-order 1 1)))
  (is (= true (check-order 1 6)))
  (is (= false (not (check-order 1 6))))

  (is (= true (check-order [1] 1)))
  (is (= true (check-order [1 1 3 1 1][1 1 5 1 1])))
  (is (= false (check-order [1 1 3 1 1] 1)))

  (is (= false (check-order [5 6 7] [5 6 0])))

  (is (= true (check-order [2 3 4] 4)))
  (is (= true (check-order [[1] [2 3 4]] [[1] 4])))

  (is (= false (check-order [9] [[8 7 6]])))
  (is (= true (check-order [[4 4] 4 4] [[4 4] 4 4 4])))

  (is (= false (check-order [7 7 7 7] [7 7 7])))

  ; handling empty lists
  (is (= true (check-order [] [3])))
  (is (= false (check-order [[[]]] [[]])))

  ; complex
  (is (= false (check-order [1 [2 [3 [4 [5 6 7]]]] 8 9] [1 [2 [3 [4 [5 6 0]]]] 8 9])))

  )

(deftest p1-test
  (is (= 13 (p1 sample-input))))

(run-tests 'aoc22.day13.test)
