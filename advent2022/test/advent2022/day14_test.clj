(ns advent2022.day14-test
  (:require [advent2022.day14 :refer [example read-input part-1 part-2]]
            [clojure.test :refer [deftest testing is]]))

(deftest day14-tests

  (testing "part 1"
    (is (= 24 (part-1 example))))

  (testing "part 2"
    (is (= 93 (part-2 example)))))

