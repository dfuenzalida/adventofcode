(ns advent2023.day02-test
  (:require [advent2023.day02 :refer [example part-1 part-2]]
            [clojure.test :refer [deftest testing is]]))

(deftest day01-tests

  (testing "part 1"
    (is (= 8 (part-1 {:red 12, :green 13, :blue 14} example))))

  (testing "part 2"
    (is (= 2286 (part-2 example)))))

