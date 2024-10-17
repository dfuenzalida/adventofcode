(ns advent2022.day07-test
  (:require [advent2022.day07 :refer [example part-1 part-2]]
            [clojure.test :refer [deftest testing is]]))

(deftest day07-tests

  (testing "part 1"
    (is (= (part-1 example) 95437)))


  (testing "part 2"
    (is (= (part-2 example) 24933642))))
