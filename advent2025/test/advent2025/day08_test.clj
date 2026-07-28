(ns advent2025.day08-test
  (:require [advent2025.day08 :refer [example part-1 part-2 read-input
                                      pairs-by-distance reduce-boxes]]
            [clojure.test :refer [deftest testing is]]))

(deftest day08-tests

  (testing "part 1"
    (is (= (-> example read-input pairs-by-distance (nth 0)) [[162,817,812] [425,690,689]]))
    (is (= (-> example read-input pairs-by-distance (nth 1)) [[162,817,812] [431,825,988]]))
    (is (= (-> example read-input pairs-by-distance (nth 2)) [[906,360,560] [805, 96,715]]))
    (is (= (-> example read-input pairs-by-distance (nth 3)) [[431,825,988] [425,690,689]]))

    (is (= (->> example read-input pairs-by-distance (take 2) (reduce reduce-boxes #{}))
         #{#{[162,817,812] [425,690,689] [431,825,988]}}))

    (is (= (->> example read-input pairs-by-distance (take 3) (reduce reduce-boxes #{}))
         #{#{[162,817,812] [425,690,689] [431,825,988]} #{[906,360,560] [805,96,715]}}))

    ;; The 4th pair doesn't change the result, boxes already in the same circuit
    (is (= (->> example read-input pairs-by-distance (take 4) (reduce reduce-boxes #{}))
         #{#{[162,817,812] [425,690,689] [431,825,988]} #{[906,360,560] [805,96,715]}}))

    (is (= 40 (part-1 example 10))))

  (testing "part 2"
    (is (= (part-2 example) 25272))))

