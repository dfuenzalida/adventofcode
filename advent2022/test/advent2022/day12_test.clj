(ns advent2022.day12-test
  (:require [advent2022.day12 :refer [example read-input part-1 part-2]]
            [clojure.test :refer [deftest testing is]]))

(deftest day12-tests

  (testing "part 1"
    (is (= 31 (part-1 example))))

  (testing "part 2"
    (is (= 29 (part-2 example)))))

