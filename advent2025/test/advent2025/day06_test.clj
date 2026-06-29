(ns advent2025.day06-test
  (:require [advent2025.day06 :refer [example read-input read-input2 part-1 part-2]]
            [clojure.test :refer [deftest testing is]]))

(deftest day06-tests

  (testing "part 1"
    (is (= 4277556 (part-1 (read-input example)))))

  (testing "part 2"
    (is (= 3263827 (part-2 (read-input2 example))))))

