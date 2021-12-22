(ns advent2021.day17-test
  (:require [advent2021.day17 :refer [meets-target? part-1 part-2]]
            [clojure.test :refer [deftest testing is]]))

(deftest day17-tests

  (testing "parts 1 and 2"
    (is (= 45 (part-1 [20 30 -10 -5])))

    (is (meets-target? [20 30 -10 -5] [7 2]))
    (is (meets-target? [20 30 -10 -5] [6 3]))
    (is (meets-target? [20 30 -10 -5] [9 0]))
    (is (not (meets-target? [20 30 -10 -5] [17 -4])))
    (is (meets-target? [20 30 -10 -5] [6 9]))

    (is (= 112 (part-2 [20 30 -10 -5])))))

