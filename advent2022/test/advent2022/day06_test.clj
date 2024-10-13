(ns advent2022.day06-test
  (:require [advent2022.day06 :refer [examples part-1 part-2]]
            [clojure.test :refer [deftest testing is]]))

(deftest day06-tests

  (testing "part 1"
    (is (= [7 5 6 10 11] (map part-1 examples))))

  (testing "part 2"
    (is (= [19 23 23 29 26] (map part-2 examples)))))

