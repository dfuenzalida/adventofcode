(ns aoc-2019.day01-test
  (:require [clojure.test :refer :all]
            [aoc-2019.day01 :refer :all]))

(deftest tests
  (testing "Day 01 part 1"
    (is (= 2 (fuel 12)))
    (is (= 2 (fuel 12)))
    (is (= 654 (fuel 1969)))
    (is (= 33583 (fuel 100756))))

  (testing "Day 01 part 2"
    (is (= 2 (fuel2 14)))
    (is (= 966 (fuel2 1969)))
    (is (= 50346 (fuel2 100756)))))



