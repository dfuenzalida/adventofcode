(ns advent2025.day02
  (:require [clojure.string :as s]
            [clojure.edn :as edn]))

(declare example)

(defn invalid1? [n]
  (re-matches #"^(\d+)\1$" (str n)))

(defn invalid2? [n]
  (re-matches #"^(\d+)(\1)+$" (str n)))

(defn solve [filter-fn s]
  (->> (s/split s #",")
       (map #(mapv edn/read-string (s/split % #"-")))
       (mapcat (fn [[a b]] (range a (inc b))))
       (filter filter-fn)
       (reduce +)))

(defn part-1 [s]
  (solve invalid1? s))

;; (= 1227775554 (part-1 example))
;; (->> "resources/day02.txt" slurp part-1)

(defn part-2 [s]
  (solve invalid2? s))

;; (= 4174379265 (part-2 example))
;; (->> "resources/day02.txt" slurp part-2)

(comment
  (->> (s/split example #",")
       (map #(mapv edn/read-string (s/split % #"-")))
       (mapcat (fn [[a b]] (range a (inc b))))
       (filter invalid?)
       (reduce +)
       (= 1227775554)
       )

  (re-matches #"^(\d+)\1$" "1188511885")
  )

(def example
  (str "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,"
       "1698522-1698528,446443-446449,38593856-38593862,565653-565659,"
       "824824821-824824827,2121212118-2121212124"))
