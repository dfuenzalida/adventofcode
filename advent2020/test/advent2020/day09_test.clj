(ns advent2020.day09-test
  (:require [advent2020.day09 :refer [example find-invalid find-weak-pair]]
            [clojure.test :refer [deftest testing is]]))

(deftest day09-tests
  (testing "part 1"
    (is (= 127 (find-invalid 5 example))))

  (testing "part 2"
    (is (= [15 47] (find-weak-pair 5 example)))))

