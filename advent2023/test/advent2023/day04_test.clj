(ns advent2023.day04-test
  (:require [advent2023.day04 :refer [example part-1 part-2]]
            [clojure.test :refer [deftest testing is]]))

(deftest tests

  (testing "part 1"
    (is (= 13 (part-1 example))))

  (testing "part 2"
    (is (= 30 (part-2 example)))))

