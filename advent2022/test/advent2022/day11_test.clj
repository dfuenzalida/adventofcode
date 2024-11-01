(ns advent2022.day11-test
  (:require [advent2022.day11 :refer [example read-input part-1 part-2]]
            [clojure.test :refer [deftest testing is]]))

(deftest day11-tests

  (testing "part 1"
    (is (= 10605 (part-1 example))))

  (testing "part 2"
    (is (= 2713310158 (part-2 example)))))

