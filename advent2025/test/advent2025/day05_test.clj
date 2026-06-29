(ns advent2025.day05-test
  (:require [advent2025.day05 :refer [example part-1 part-2]]
            [clojure.test :refer [deftest testing is]]))

(deftest day05-tests

  (testing "part 1"
    (is (= 3 (part-1 example))))

  (testing "part 2"
    (is (= 14 (part-2 example)))))

