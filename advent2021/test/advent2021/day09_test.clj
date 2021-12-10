(ns advent2021.day09-test
  (:require [advent2021.day09 :refer [example read-input make-height-map low-point?
                                      part-1 part-2]]
            [clojure.test :refer [deftest testing is]]))

(deftest day09-tests

  (testing "parts 1 and 2"

    (is (true? (-> example read-input make-height-map (low-point? [1 0]))))
    (is (= 15 (part-1 example)))
    (is (= 1134 (part-2 example)))))

