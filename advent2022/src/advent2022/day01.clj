(ns advent2022.day01
  (:require [clojure.string :as s]))

(declare example-input)

(defn read-input [input]
  (let [groups (s/split input #"\n\n")]
    (->> groups
         (map #(str \[ % \]))
         (map read-string))))

(defn part-1 [groups]
  (->> groups
       (map (partial reduce +))
       (reduce max)))

;; (part-1 (read-input example))
;; (->> "resources/day01.txt" slurp read-input part-1)

(defn part-2 [groups]
  (->> groups
       (map (partial reduce +))
       (into (sorted-set-by >))
       (take 3)
       (reduce +)))

;; (part-2 (read-input example))
;; (->> "resources/day01.txt" slurp read-input part-2)

(comment

  ;; (read-string (str \[ example-input \]))
  (let [groups (s/split example-input #"\n\n")]
    (->> groups
         (map #(str \[ % \]))
         (map read-string)
         (map (partial reduce +))
         (reduce max)
          ))

  (read-input example-input)

  (into (sorted-set-by >) [2 4 6 1 3 5])

  )


(def example
  "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000")
