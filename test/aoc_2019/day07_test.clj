(ns aoc-2019.day07-test
  (:require [clojure.test :refer :all]
            [aoc-2019.day07 :refer [gen-phases exec-phases loop-amps]]))

(deftest tests
  (let [prog1  [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]
        prog2  [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,
                23,1,24,23,23,4,23,99,0,0]
        prog3  [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
                1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0]
        phases (gen-phases (range 5))]

    (testing "Day 07 part 1"

      (is (= 43210 (exec-phases prog1 [4 3 2 1 0])))
      (is (= 54321 (exec-phases prog2 [0 1 2 3 4])))
      (is (= 65210 (exec-phases prog3 [1 0 4 3 2])))

      (let [best (apply max-key (partial exec-phases prog1) phases)]
        (is (= 43210 (exec-phases prog1 best))))

      (let [best (apply max-key (partial exec-phases prog2) phases)]
        (is (= 54321 (exec-phases prog2 best))))

      (let [best (apply max-key (partial exec-phases prog3) phases)]
        (is (= 65210 (exec-phases prog3 best))))))

  (let [p2ex1  [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
                27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]
        p2ex2  [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
                -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
                53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10]
        phases (gen-phases (range 5 10))]

    (is (= 139629729 (loop-amps p2ex1 [9,8,7,6,5])))
    (is (= 18216 (loop-amps p2ex2 [9,7,8,5,6])))

    (is (= 139629729 (reduce max (map (partial loop-amps p2ex1) phases))))
    (is (= 18216 (reduce max (map (partial loop-amps p2ex2) phases))))
  )

)
