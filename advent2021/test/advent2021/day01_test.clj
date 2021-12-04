(ns advent2021.day01-test
  (:require [advent2021.day01 :refer [example part-1 part-2]]
            [clojure.test :refer [deftest testing is]]))

(deftest day01-tests

  (testing "part 1"
    (is (= 7 (part-1 example))))

  (testing "part 2"
    (is (= 5 (part-2 example)))))

