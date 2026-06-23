(ns advent2025.day03
  (:require [clojure.string :as s]
            [clojure.edn :as edn]))

(declare example)

(defn read-input [s]
  (->> (s/split-lines s)
       (mapv #(mapv (comp edn/read-string str) %))))

(defn take-highest [res n xs]
  (if (zero? n)
    res
    (let [x-range (inc (- (count xs) n)) ;; leave (n-1) digits for the next iterations
          max-x   (reduce max (take x-range xs))
          xs      (rest (drop-while #(not (= max-x %)) xs))] ;; drop until first occurrence of max-x (inclusive)
      (recur (into res [max-x]) (dec n) xs))))

(defn solve [n input]
  (->> (read-input input)
       (map (partial take-highest [] n))
       (map (partial s/join ""))
       (map edn/read-string)
       (reduce +)))

;; (->> "234234234234278" read-input first (take-highest [] 12) (s/join "") (= "434234234278"))
;; (->> "818181911112111" read-input first (take-highest [] 12) (s/join "") (= "888911112111"))

(defn part-1 [input]
  (solve 2 input))

;; (= 357 (part-1 example))
;; (->> "resources/day03.txt" slurp part-1)

(defn part-2 [input]
  (solve 12 input))

;; (= 3121910778619 (part-2 example))
;; (->> "resources/day03.txt" slurp part-2)

(def example
  "987654321111111
811111111111119
234234234234278
818181911112111")
