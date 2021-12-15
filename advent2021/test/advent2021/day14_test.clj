(ns advent2021.day14-test
  (:require [advent2021.day14 :refer [example read-input make-pairmap part-1]]
            [clojure.test :refer [deftest testing is]]))

(deftest day14-tests

  (testing "parts 1 and 2"
    (is (= {[\N \N] 1, [\N \C] 1, [\C \B] 1} (->> example read-input first make-pairmap)))
    (is (= {\N 2, \C 1, \B 1} (->> example read-input first frequencies)))
    (is (= 1588 (part-1 example 10)))
    (is (= 2188189693529 (part-1 example 40)))))

