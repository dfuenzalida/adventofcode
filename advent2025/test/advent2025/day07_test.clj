(ns advent2025.day07-test
  (:require [advent2025.day07 :refer [example part-1 part-2]]
            [clojure.test :refer [deftest testing is]]))

(deftest day07-tests

  (testing "part 1"
    (is (= 21 (part-1 example))))

  (testing "part 2"
    (is (= 40 (part-2 example)))))
