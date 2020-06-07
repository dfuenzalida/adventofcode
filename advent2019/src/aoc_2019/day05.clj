(ns aoc-2019.day05
  (:require [aoc-2019.intcode :refer [execute]]))

(defn read-program []
  (->> (slurp "resources/input05.txt")
       (format "[%s]")
       (clojure.edn/read-string)))

(defn solve-1 []
  (->> (execute (read-program) [1]) last last))

(defn solve-2 []
  (->> (execute (read-program) [5]) last last))

;; (solve-1)
;; (solve-2)

