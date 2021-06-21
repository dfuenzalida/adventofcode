(ns advent2015.day01
  (:require [clojure.string :refer [trim]])
  (:gen-class))

(defn read-input []
  (trim (slurp "resources/day01.txt")))

(defn part-1 []
  (->> (read-input)
       (map {\( 1 \) -1})
       (reduce + 0)))

;; (part-1)

(defn part-2 []
  (->> (read-input)
       (map {\( 1 \) -1})
       (reductions + 0)
       (take-while #(not= -1 %)) ;; while we don't reach the basement
       (count)))

;; (part-2)
