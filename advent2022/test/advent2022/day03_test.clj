(ns advent2022.day03-test
  (:require [advent2022.day03 :refer [example read-input priority part-1 part-2]]
            [clojure.test :refer [deftest testing is]]))

(deftest day03-tests

  (testing "part 1"
    (is (= (priority \a) 1))
    (is (= (priority \z) 26))
    (is (= (priority \A) 27))
    (is (= (priority \Z) 52))
    (is (= (part-1 example) 157)))
  
  (testing "part 2"
    (is (= (part-2 example) 70))))

