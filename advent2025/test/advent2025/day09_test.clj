(ns advent2025.day09-test
  (:require [advent2025.day09 :refer [example part-1 part-2 read-input]]
            [clojure.test :refer [deftest testing is]]))

(deftest day09-tests

  (testing "part 1"
    (is (= 50 (->> example read-input part-1))))

  (testing "part 2"
    (is (= (part-2 example) 24))))
