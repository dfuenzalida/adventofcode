(ns advent2020.day02-test
  (:require [advent2020.day02 :refer [parse-line valid1? valid2?]]
            [clojure.test :refer [deftest testing is]]))

(deftest day02-tests
  (testing "part 1"
    (is (true? (->> "1-3 a: abcde" parse-line valid1?)))
    (is (false? (->> "1-3 b: cdefg" parse-line valid1?)))
    (is (true? (->> "2-9 c: ccccccccc" parse-line valid1?))))

  (testing "part 2"
    (is (true? (valid2? [1 3 \a "abcde"])))
    (is (false? (valid2? [1 3 \b "cdefg"])))
    (is (false? (valid2? [2 9 \c "ccccccccc"])))))

