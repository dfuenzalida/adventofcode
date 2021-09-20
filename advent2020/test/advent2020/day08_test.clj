(ns advent2020.day08-test
  (:require [advent2020.day08 :refer [example exec flip part-1 part-2 read-input]]
            [clojure.test :refer [deftest testing is]]))


(deftest day08-tests
  (testing "part 1"
    (is (= 5 (second (exec (read-input example))))))

  (testing "part 2"
    ;; flip helper
    (is (= [["jmp" 10] ["jmp" 11] ["acc" 12]] (flip [["nop" 10] ["jmp" 11] ["acc" 12]] 0)))
    (is (= [["nop" 10] ["nop" 11] ["acc" 12]] (flip [["nop" 10] ["jmp" 11] ["acc" 12]] 1)))
    (is (= [["nop" 10] ["jmp" 11] ["acc" 12]] (flip [["nop" 10] ["jmp" 11] ["acc" 12]] 2)))

    (is (= 8 (part-2 (read-input example)))))
  )
    
