(ns advent2021.day16-test
  (:require [advent2021.day16 :refer [create-buffer read-packet part-1 part-2]]
            [clojure.test :refer [deftest testing is]]))

(deftest day16-tests

  (testing "part 1"
    (is (= 2021 (-> (create-buffer "D2FE28") read-packet first :val)))
    (is (= 16 (part-1 "8A004A801A8002F478")))
    (is (= 12 (part-1 "620080001611562C8802118E34")))
    (is (= 23 (part-1 "C0015000016115A2E0802F182340")))
    (is (= 31 (part-1 "A0016C880162017C3686B18A3D4780"))))

  (testing "part 2"
    (is (= 3 (part-2 "C200B40A82")))
    (is (= 54 (part-2 "04005AC33890")))
    (is (= 7 (part-2 "880086C3E88112")))
    (is (= 9 (part-2 "CE00C43D881120")))
    (is (= 1 (part-2 "D8005AC2A8F0")))
    (is (= 0 (part-2 "F600BC2D8F")))
    (is (= 0 (part-2 "9C005AC2F8F0")))
    (is (= 1 (part-2 "9C0141080250320F1802104A08")))))

