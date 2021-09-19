(ns advent2020.day06-test
  (:require [advent2020.day06 :refer [common-answers example part-1 part-2]]
            [clojure.string :refer [split-lines]]
            [clojure.test :refer [deftest testing is]]))

(deftest day06-tests
  (testing "part 1"
    (is (= 6 (part-1 "abcx\nabcy\nabcz")))
    (is (= 11 (part-1 example))))

  (testing "part 2"
    (is (= "a" (common-answers "ab" "ac")))
    (is (= 6 (part-2 example)))

    (is (= [3 0 1 1 1]
           (->> example split-lines (partition-by empty?)
                (remove (comp empty? first))
                (map (partial reduce common-answers))
                (map count))))
    ))
