(ns advent2021.day08-test
  (:require [advent2021.day08 :refer [example example2 lookup-segment
                                      make-segment-map read-input part-1 part-2]]
            [clojure.test :refer [deftest testing is]]))

(deftest day08-tests

  (testing "parts 1 and 2"
    (is (= 26 (part-1 example2)))
    (is (= 5353 (part-2 example)))
    (is (= 8 (-> example read-input
                 ffirst make-segment-map
                 (lookup-segment "fdgacbe"))))))

