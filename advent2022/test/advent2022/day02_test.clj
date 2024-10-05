(ns advent2022.day02-test
  (:require [advent2022.day02 :refer [example score part-1 part-2]]
            [clojure.test :refer [deftest testing is are]]))

(deftest day02-tests

  (testing "part 1"
    (are [x y] (= x y)
      (score [:rock :paper]) 8
      (score [:paper :rock]) 1
      (score [:scissors :scissors]) 6
      (part-1 example) 15))

  (testing "part 2"
    (is   (= 12 (part-2 example)))))
