(ns advent2025.day05
  (:require [clojure.string :as s]
            [clojure.edn :as edn]))

(declare example)

(defn read-input [input]
  (let [[ranges ingredients] (s/split input #"\n\n")
        ranges               (->> ranges s/split-lines (map #(map edn/read-string (s/split % #"-"))))
        ingredients          (->> ingredients s/split-lines (map edn/read-string))]
    [ranges ingredients]))

;; (read-input example)

(defn fresh? [ranges ingredient]
  (some (fn [[a b]] (<= a ingredient b)) ranges))

(defn part-1 [input]
  (let [[ranges ingredients] (read-input input)]
    (->> (filter (partial fresh? ranges) ingredients)
         count)))

;; (= 3 (part-1 example))
;; (-> "resources/day05.txt" slurp part-1)

(defn fixed-point
  "Returns the value of applying f repeatedly to x until the result is constant"
  [f x]
  (reduce
   (fn [a b] (if (= a b) (reduced a) b))
   (iterate f x)))

(defn merge-ranges [[a b] [c d]]
  (cond
    (<= a c d b) [a b]  ;; fully contained
    (<= c a b d) [c d]
    (<= a c b d) [a d]  ;; overlapping
    (<= c a d b) [c b]
    (= (inc b) c) [a d] ;; adjacent
    (= (inc d) a) [c b]))

(defn iter-ranges [ranges]
  (if-let [merged (first
                   (for [rx ranges ry ranges
                         :when (not= rx ry)
                         :let [merged (merge-ranges rx ry)]
                         :when (some? merged)]
                     ;; if we could merge, remove the original ranges and insert the merged range
                     (-> ranges (disj rx) (disj ry) (conj merged))))]
    merged
    ranges))

(defn part-2 [input]
  (let [[ranges _] (read-input input)
        ranges     (fixed-point iter-ranges (set ranges))]
    (->> (map (fn [[a b]] (inc (- b a))) ranges)
         (reduce +))))

;; (= 14 (part-2 example))
;; (->> "resources/day05.txt" slurp part-2)

(comment

  (let [[ranges _] (read-input example)]
    (println "---")
    (fixed-point iter-ranges (set ranges)))
  
  (some identity [nil nil 1 nil])
  
  (let [[ranges ingredients] (s/split example #"\n\n")
        ranges               (->> ranges s/split-lines (map #(map edn/read-string (s/split % #"-"))))
        ingredients          (->> ingredients s/split-lines (map edn/read-string))]
    ranges)

  (let [[ranges _] (read-input example)]
    (fresh? ranges 32))

  )

(def example
  "3-5
10-14
16-20
12-18

1
5
8
11
17
32")

