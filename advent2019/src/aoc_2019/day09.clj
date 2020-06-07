(ns aoc-2019.day09
  (:require [aoc-2019.intcode :refer [execute]]))

(defn solve-1 []
  (let [prog (->> (slurp "resources/input09.txt")
                  (format "[%s]")
                  (clojure.edn/read-string))]
    (->> (execute prog [1]) last)))

;; (solve-1)
