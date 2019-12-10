(ns aoc-2019.day07-test
  (:require [clojure.test :refer :all]
            [aoc-2019.day05 :refer [execute]]
            [aoc-2019.day07 :refer :all]))

(deftest tests
  (testing "Day 07 part 1"
    (let [prog1 [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]
          prog2 [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,
                 23,1,24,23,23,4,23,99,0,0]
          prog3 [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
                 1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0]]

      (is (= 43210 (execute-phases prog1 [4 3 2 1 0])))
      (is (= 54321 (execute-phases prog2 [0 1 2 3 4])))
      (is (= 65210 (execute-phases prog3 [1,0,4,3,2])))

      (let [best (apply max-key (partial execute-phases prog1) (generate-phases))]
        (is (= 43210 (execute-phases prog1 best))))

      (let [best (apply max-key (partial execute-phases prog2) (generate-phases))]
        (is (= 54321 (execute-phases prog2 best))))

      (let [best (apply max-key (partial execute-phases prog3) (generate-phases))]
        (is (= 65210 (execute-phases prog3 best))))))

  ;; TODO part 2
  )

