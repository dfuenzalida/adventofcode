(ns advent2022.day05-test
  (:require [advent2022.day05 :refer [example part-1 part-2]]
            [clojure.test :refer [deftest testing is]]))

(deftest day01-tests

  (testing "part 1"
    (is (= (part-1 example) "CMZ")))

  (testing "part 2"
    (is (= (part-2 example) "MCD"))))
