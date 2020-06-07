(ns aoc-2019.day02-test
  (:require [clojure.test :refer :all]
            [aoc-2019.day02 :refer [execute]]))

(deftest tests
  (testing "Day 02 part 1"
    (is (= (execute [1,0,0,0,99]) [2,0,0,0,99]))
    (is (= (execute [2,3,0,3,99]) [2,3,0,6,99]))
    (is (= (execute [2,4,4,5,99,0]) [2,4,4,5,99,9801]))
    (is (= (execute [1,1,1,4,99,5,6,0,99]) [30,1,1,4,2,5,6,0,99]))))


