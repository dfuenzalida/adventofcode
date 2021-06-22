(ns advent2015.day05
  (:require [clojure.string :refer [includes? split-lines]]))

(defn at-least-3-vowels? [s]
  (->> (filter #{\a \e \i \o \u} s)
       count
       (< 2)))

(defn one-letter-twice-in-a-row? [s]
  (->> (partition-all 2 1 s)
       (filter (fn [[a b]] (= a b)))
       seq boolean))

(defn safe-word? [s]
  (->> (map (partial includes? s) ["ab" "cd" "pq" "xy"])
       (every? false?)))

(defn read-input []
  (->> (slurp "resources/day05.txt") split-lines))

(defn nice? [s]
  (and (at-least-3-vowels? s) (one-letter-twice-in-a-row? s) (safe-word? s)))

(defn part-1 []
  (->> (read-input) (filter nice?) count))

;; (part-1)

(defn two-pairs? [s]
  (boolean (re-matches #".*(..).*\1.*" s)))

(defn matches-aba? [s] ;; a string in the shape of "...ABA..."
  (->> (partition-all 3 1 s)
       (filter (fn [[a b c]] (= a c)))
       seq boolean))

(defn nice2? [s]
  (and (two-pairs? s) (matches-aba? s)))

(defn part-2 []
  (->> (read-input) (filter nice2?) count))

;; (part-2)
  
