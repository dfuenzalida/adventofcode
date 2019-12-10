(ns aoc-2019.day05-test
  (:require [clojure.test :refer :all]
            [aoc-2019.day05 :refer [execute]]))

(deftest tests

  (testing "Compare if the input is equal to 8 (position mode)"
    (is (= 1 (->> (execute [3,9,8,9,10,9,4,9,99,-1,8] [8]) last last)))
    (is (= 0 (->> (execute [3,9,8,9,10,9,4,9,99,-1,8] [666]) last last))))

  (testing "Compare if the input is equal to 8 (immediate mode)"
    (is (= 1 (->> (execute [3,3,1108,-1,8,3,4,3,99] [8]) last last)))
    (is (= 0 (->> (execute [3,3,1108,-1,8,3,4,3,99] [666]) last last))))

  (testing "Jump tests that take an input - position mode"
    (is (= 0 (->> (execute [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9] [0]) last last)))
    (is (= 1 (->> (execute [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9] [1]) last last))))

  (testing "Jump tests that take an input - immediate mode"
    (is (= 0 (->> (execute [3,3,1105,-1,9,1101,0,0,12,4,12,99,1] [0]) last last)))
    (is (= 1 (->> (execute [3,3,1105,-1,9,1101,0,0,12,4,12,99,1] [1]) last last)))))

