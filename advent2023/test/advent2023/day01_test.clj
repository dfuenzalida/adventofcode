(ns advent2023.day01-test
  (:require [advent2023.day01 :refer [example example2 part-1 part-2]]
            [clojure.test :refer [deftest testing is]]))

(deftest day01-tests

  (testing "part 1"
    (is (= 142 (part-1 example))))

  (testing "part 2"
    (is (= 281 (part-2 example2)))))

