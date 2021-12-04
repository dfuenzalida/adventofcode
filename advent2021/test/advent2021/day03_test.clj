(ns advent2021.day03-test
  (:require [advent2021.day03 :refer [example digit-freqs digits-from-freqs
                                      from-bin rating part-1 part-2]]
            [clojure.string :refer [split-lines]]
            [clojure.test :refer [deftest testing is]]))

(deftest day03-tests

  (testing "part 1"
    (is (= [\1 \0 \1 \1 \0] (->> example digit-freqs (digits-from-freqs >))))
    (is (= 22 (from-bin [\1 \0 \1 \1 \0])))
    (is (= 198 (part-1 example)))
    (is (= 23 (-> example split-lines (rating > \1))))
    (is (= 10 (-> example split-lines (rating < \0)))))

  (testing "part 2"
    (is (= 230 (part-2 example)))))


