(ns advent2021.day12-test
  (:require [advent2021.day12 :refer [example example2 example3 part-1 part-2
                                      upto-one-repeat-lowercase?]]
            [clojure.test :refer [deftest testing is]]))

(deftest day12-tests

  (testing "part 1"
    (is (false? (upto-one-repeat-lowercase? ["start" "a" "b" "a" "c" "c"])))
    (is (= 10 (part-1 example)))
    (is (= 19 (part-1 example2)))
    (is (= 226 (part-1 example3))))

  (testing "part 2"
    (is (= 36 (part-2 example)))
    (is (= 103 (part-2 example2)))
    (is (= 3509 (part-2 example3)))))

