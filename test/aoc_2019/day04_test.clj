(ns aoc-2019.day04-test
  (:require [clojure.test :refer :all]
            [aoc-2019.day04 :refer :all]))

(deftest tests
  (testing "Day 04 part 1"
    (is (valid? 111111))
    (is (false? (valid? 223450)))
    (is (false? (valid? 123789))))

  (testing "Day 04 part 2"
    (is (valid2? 112233))
    (is (false? (valid2? 123444)))
    (is (valid2? 111122))))

