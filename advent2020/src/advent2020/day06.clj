(ns advent2020.day06
  (:require [clojure.string :refer [split-lines]]))

(declare example)

(defn part-1 [input]
  (->> input split-lines (partition-by empty?)
       (map (partial reduce str))
       (remove empty?)
       (map (comp count set))
       (reduce +)))

;; (= 6 (part-1 "abcx\nabcy\nabcz"))
;; (= 11 (part-1 example))

(defn read-input []
  (slurp "resources/day06.txt"))

;; (part-1 (read-input))

(defn common-answers [word1 word2]
  (reduce str "" (filter (set word1) word2)))

;; (common-answers "ab" "ac")

(defn part-2 [input]
  (->> input split-lines (partition-by empty?)
       (remove (comp empty? first))
       (map (partial reduce common-answers))
       (map count)
       (reduce +)))

;; (= 6 (part-2 example))
;; (part-2 (read-input))

(comment
  ;; part-1
  (->> example split-lines (partition-by empty?)
       (map (partial reduce str))
       (remove empty?)
       (map (comp count set)))

  ;; part-2
  (->> example split-lines (partition-by empty?)
       (remove (comp empty? first))
       (map (partial reduce common-answers))
       (map count)
       #_(reduce +))

  )


(def example "abc

a
b
c

ab
ac

a
a
a
a

b")
