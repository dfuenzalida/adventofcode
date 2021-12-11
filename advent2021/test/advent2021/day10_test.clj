(ns advent2021.day10-test
  (:require [advent2021.day10 :refer [corrupt-char complete example part-1 score2 part-2]]
            [clojure.test :refer [deftest testing is]]))

(deftest day10-tests

  (testing "part 1"
    (is (nil? (corrupt-char "()")))
    (is (nil? (corrupt-char "(((((((((())))))))))")))
    (is (every? some? (map corrupt-char ["(]" "{()()()>" "(((()))}" "<([]){()}[{}])"])))
    (is (= 26397 (part-1 example))))

  (testing part-2
    (is (->> (complete "[({(<(())[]>[[{[]{<()<>>") (= "}}]])})]")))
    (is (= 288957 (score2 "[({(<(())[]>[[{[]{<()<>>")))
    (is (= 5566 (score2 "[(()[<>])]({[<{<<[]>>(")))
    (is (= 1480781 (score2 "(((({<>}<{<{<>}{[]{[]{}")))
    (is (= 995444 (score2 "{<[[]]>}<{[{[{[]{()[[[]")))
    (is (= 294 (score2 "<{([{{}}[<[[[<>{}]]]>[]]")))
    (is (= 288957 (part-2 example)))))
  
