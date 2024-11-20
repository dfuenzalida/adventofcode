(ns advent2022.day15-test
  (:require [advent2022.day15 :refer [example read-input part-1 part-2]]
            [clojure.test :refer [deftest testing is]]))

(deftest day15-tests

  (testing "part 1"
    (is (= 26 (part-1 example 10))))

  (testing "part 2"
    (is (= 56000011 (part-2 example 20)))))

