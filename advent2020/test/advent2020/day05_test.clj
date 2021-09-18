(ns advent2020.day05-test
  (:require [advent2020.day05 :refer [column row seat-id split-range]]
            [clojure.test :refer [deftest testing is]]))

(deftest day05-tests
  (testing "part 1"

    (is (= [0 63] (split-range [0 127] \F)))
    (is (= [32 63] (split-range [0 63] \B)))
    (is (= [32 47] (split-range [32 63] \F)))
    (is (= [40 47] (split-range [32 47] \B)))
    (is (= [44 47] (split-range [40 47] \B)))
    (is (= [44 45] (split-range [44 47] \F)))
    (is (= [44 44] (split-range [44 45] \F)))

    (is (= 44 (row "FBFBBFFRLR")))
    (is (= 5 (column "FBFBBFFRLR")))
    (is (= 357 (seat-id "FBFBBFFRLR")))

    (let [coords (juxt row column seat-id)]
      (is (= [70 7 567] (coords "BFFFBBFRRR")))
      (is (= [14 7 119] (coords "FFFBBBFRRR")))
      (is (= [102 4 820] (coords "BBFFBBFRLL"))))

    ))
