(ns advent2022.day08-test
  (:require [advent2022.day08 :refer [example read-input visible? viewing-distance scenic-score part-1 part-2]]
            [clojure.test :refer [deftest testing is]]))

(deftest day08-tests

  (testing "part 1"
    (is (visible? (read-input example) 1 1))
    (is (= 21 (part-1 example))))

  (testing "part 2"
    (is (= 4 (-> (read-input example) (scenic-score 2 1))))
    (is (= 8 (-> (read-input example) (scenic-score 2 3))))

    (is (= 1 (viewing-distance 5 [3])))
    (is (= 1 (viewing-distance 5 [5 2 2])))
    (is (= 2 (viewing-distance 5 [1 2])))
    (is (= 2 (viewing-distance 5 [3 5 3])))

    (is (= 8 (part-2 example)))))

