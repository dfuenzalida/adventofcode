(ns advent2020.day14-test
  (:require [advent2020.day14 :refer [example-mask example example2
                                      addresses-for mask-value part-1 part-2]]
            [clojure.test :refer [deftest testing is]]))

(deftest day14-tests
  (testing "part 1"
    (is (= 73 (mask-value example-mask 11)))
    (is (= 101 (mask-value example-mask 101)))
    (is (= 64 (mask-value example-mask 0)))
    (is (= 165 (part-1 example))))

  (testing "part 2"
    (is (= [26 27 58 59] (addresses-for "000000000000000000000000000000X1001X" 42)))
    (is (= [16 17 18 19 24 25 26 27] (addresses-for "00000000000000000000000000000000X0XX" 26)))
    (is (= 208 (part-2 example2)))))

