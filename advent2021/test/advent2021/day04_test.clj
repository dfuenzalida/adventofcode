(ns advent2021.day04-test
  (:require [advent2021.day04 :refer [example part-1 part-2]]
            [clojure.test :refer [deftest testing is]]))

(deftest day04-tests
  (testing "parts 1 and 2"
    (is (= 4512 (part-1 example)))
    (is (= 1924 (part-2 example)))))

