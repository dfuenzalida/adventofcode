(ns advent2021.day07-test
  (:require [advent2021.day07 :refer [example read-input cost cost2 part-1 part-2]]
            [clojure.test :refer [deftest testing is]]))

(deftest day07-tests

  (testing "part 1"
    (is (= 37 (part-1 example)))
    (is (= 41 (-> example read-input frequencies (cost 1))))
    (is (= 39 (-> example read-input frequencies (cost 3))))
    (is (= 71 (-> example read-input frequencies (cost 10)))))

  (testing "part 2"
    (is (= 168 (part-2 example)))
    (is (= 206 (-> example read-input frequencies (cost2 2))))))

