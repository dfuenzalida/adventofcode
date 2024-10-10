(ns advent2022.day04-test
  (:require [advent2022.day04 :refer [example read-input part-1 part-2]]
            [clojure.test :refer [deftest testing is]]))

(deftest day04-tests

  (testing "part 1"
    (is (= 2 (part-1 example))))

  (testing "part 2"
    (is (= 4 (part-2 example)))))
