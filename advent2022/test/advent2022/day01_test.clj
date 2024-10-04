(ns advent2022.day01-test
  (:require [advent2022.day01 :refer [example read-input part-1 part-2]]
            [clojure.test :refer [deftest testing is]]))

(deftest day01-tests

  (testing "part 1"
    (is (= 24000 (part-1 (read-input example)))))

  (testing "part 2"
    (is (= 45000 (part-2 (read-input example))))))
