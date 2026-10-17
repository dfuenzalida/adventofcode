(ns advent2023.day01
  (:require [clojure.string :as s]
            [clojure.edn :as edn]))

(declare example example2)

(defn calibration [s]
  (->> (re-seq #"\d" s)
       ((juxt first last))
       (apply str)
       (edn/read-string)))

(defn part-1 [s]
  (->> s s/split-lines (map calibration) (reduce +)))

;; (= 142 (part-1 example))
;; (->> "resources/day01.txt" slurp part-1)

(def digits-map
  {"0" 0, "1" 1, "2" 2, "3" 3, "4" 4, "5" 5, "6" 6, "7" 7, "8" 8, "9" 9,
   "zero" 0, "one" 1, "two" 2, "three" 3, "four" 4,
   "five" 5, "six" 6, "seven" 7, "eight" 8, "nine" 9})

(defn calib2 [s]
  (loop [s s
         digits []]
    (if (empty? s)
      (if (empty? digits) 0 (+ (* 10 (first digits)) (last digits)))
      ;; Find the first pair [k v] such that `s` starts with `k`
      (if-let [[k v] (first (filter (fn [[k v]] (s/starts-with? s k)) digits-map))]
        (recur (subs s 1) (conj digits v))
        (recur (subs s 1) digits)))))

;; (= [29 83 13 24 42 14 76] (->> example2 s/split-lines (map calib2)))
;; (= 44 (calib2 "4r"))

(defn part-2 [s]
  (->> s s/split-lines (map calib2) (reduce +)))

;; (= 281 (part-2 example2))
;; (->> "resources/day01.txt" slurp part-2)

(comment

  (let [lines (->> "resources/day01.txt" slurp s/split-lines (take 40))]
    (doseq [x lines]
      (println x (calib2 x))))  
  
  (let [s1 "two1nine"]
    (->> (filter (fn [[k v]] (s/starts-with? s1 k)) calib-map)))
  
  (->> example s/split-lines second calibration)
  
  (->> (s/split-lines example)
       first
       (re-seq #"\d")
       ((juxt first last))
       (apply str)
       (edn/read-string))

  )


(def example
  "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet")

(def example2
  "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen")
