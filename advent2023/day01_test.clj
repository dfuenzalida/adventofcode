(ns advent2025.day01-test
  (:require [advent2025.day01 :refer [example read-input part-1 part-2]]
            [clojure.test :refer [deftest testing is]]))

(deftest day01-tests

  (testing "part 1"
    (is (= 3 (part-1 (read-input example)))))

  (testing "part 2"
    (is (= 6 (part-2 (read-input example))))))
