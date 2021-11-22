(ns advent2020.day15-test
  (:require [advent2020.day15 :refer [gen-sequence]]
            [clojure.test :refer [deftest testing is]]))

(deftest day15-tests
  (testing "parts 1 and 2"
    (is (= [0 3 6 0 3 3 1 0 4 0] (->> (gen-sequence [0 3 6]) (take 10))))
    (is (= 436 (nth (gen-sequence [0 3 6]) (dec 2020))))
    (is (= 1 (nth (gen-sequence [1 3 2]) (dec 2020))))
    (is (= 10 (nth (gen-sequence [2 1 3]) (dec 2020))))
    (is (= 27 (nth (gen-sequence [1 2 3]) (dec 2020))))
    (is (= 78 (nth (gen-sequence [2 3 1]) (dec 2020))))
    (is (= 438 (nth (gen-sequence [3 2 1]) (dec 2020))))
    (is (= 1836 (nth (gen-sequence [3 1 2]) (dec 2020))))))

