(ns advent2021.day10
  (:require [clojure.string :refer [split-lines]]))

(declare example)

(def opening #{\( \{ \[ \<})
(def closing {\( \), \{ \}, \[ \], \< \>})

(def score {\) 3, \] 57, \} 1197, \> 25137})

(defn corrupt-char
  ([xs] (corrupt-char [] xs))
  ([stack xs]
   (when-let [x (first xs)]
     (let [top (peek stack)]
       (cond
         (opening x) (recur (conj stack x) (rest xs))
         :default    (if (= x (closing top))
                       (recur (pop stack) (rest xs))
                       x))))))

;; (nil? (corrupt-char "()"))
;; (nil? (corrupt-char "(((((((((())))))))))"))
;; (every? some? (map corrupt-char ["(]" "{()()()>" "(((()))}" "<([]){()}[{}])"]))

(defn part-1 [s]
  (->> s split-lines
       (map corrupt-char)
       (remove nil?)
       (map score)
       (reduce +)))

;; (= 26397 (part-1 example))
;; (part-1 (slurp "resources/day10.txt"))

(defn complete
  ([xs] (complete [] xs))
  ([stack xs]
   (if-let [x (first xs)]
     (let [top (peek stack)]
       (cond
         (opening x) (recur (conj stack x) (rest xs))
         :default    (if (= x (closing top))
                       (recur (pop stack) (rest xs))
                       x)))
     (->> stack reverse (map closing) (reduce str)))))

;; (->> (complete "[({(<(())[]>[[{[]{<()<>>") (= "}}]])})]")

(def points2 {\) 1, \] 2, \} 3, \> 4})

(defn score2 [s]
  (->> (complete s)
       (map points2)
       (reduce #(+ %2 (* 5 %1)) 0)))

;; (= 288957 (score2 "[({(<(())[]>[[{[]{<()<>>"))
;; (= 5566 (score2 "[(()[<>])]({[<{<<[]>>("))
;; (= 1480781 (score2 "(((({<>}<{<{<>}{[]{[]{}"))
;; (= 995444 (score2 "{<[[]]>}<{[{[{[]{()[[[]"))
;; (= 294 (score2 "<{([{{}}[<[[[<>{}]]]>[]]"))

(defn part-2 [s]
  (let [lines (->> s split-lines (remove #(some? (corrupt-char %))))]
    (-> (map score2 lines)
        sort
        (nth (quot (count lines) 2)))))

;; (= 288957 (part-2 example))
;; (part-2 (slurp "resources/day10.txt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def example "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]")
