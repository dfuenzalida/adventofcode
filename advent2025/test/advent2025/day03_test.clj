(ns advent2025.day03-test
  (:require [advent2025.day03 :refer [example part-1 part-2]]
            [clojure.test :refer [deftest testing is]]))

(deftest day03-tests

  (testing "part 1"
    (is (= 357 (part-1 example))))

  (testing "part 2"
    (is (= 3121910778619 (part-2 example)))))
