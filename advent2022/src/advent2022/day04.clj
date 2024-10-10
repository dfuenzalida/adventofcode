(ns advent2022.day04)

(defn contained? [[a b c d]]
  (or (<= a c d b) (<= c a b d)))

(defn read-input [s]
  (->> (re-seq #"\d+" s)
       (map read-string)
       (partition 4)))

(defn part-1 [s]
  (->> s read-input (filter contained?) count))

;; (part-1 (slurp "resources/day04.txt"))

(defn overlap? [[a b c d]]
  (or (contained? [a b c d])
      (<= a c b d)
      (<= c a d b)))

(defn part-2 [s]
  (->> s read-input (filter overlap?) count))

;; (part-2 (slurp "resources/day04.txt"))

(comment

  (->> example
       (re-seq #"\d+")
       (map read-string)
       (partition 4))

  (->> example read-input (filter contained?) count)

  (= (part-1 example) 2)
  (= (part-2 example) 4)
  )

(def example "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8")
