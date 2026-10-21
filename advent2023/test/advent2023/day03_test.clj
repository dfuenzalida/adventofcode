(ns advent2023.day03-test
  (:require [advent2023.day03 :refer [example part-1 part-2]]
            [clojure.test :refer [deftest testing is]]))

(deftest day01-tests

  (testing "part 1"
    (is (= 4361 (part-1 example))))

  (testing "part 2"
    (is (= 467835 (part-2 example)))))


