(ns advent2025.day02-test
  (:require [advent2025.day02 :refer [example part-1 part-2]]
            [clojure.test :refer [deftest testing is]]))

(deftest day02-tests

  (testing "part 1"
    (is (= 1227775554 (part-1 example))))

  (testing "part 2"
    (is (= 4174379265 (part-2 example)))))
