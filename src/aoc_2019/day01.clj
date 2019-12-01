(ns aoc-2019.day01)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn read-input []
  (->> (str "[" (slurp "resources/input01.txt") "]")
       (clojure.edn/read-string)))

(defn fuel [mass]
  (max 0 (- (quot mass 3) 2)))

(defn solve-1 []
  (->> (read-input)
       (map fuel)
       (reduce +)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn fuel2 [mass]
  (->> mass (iterate fuel) rest (take-while pos?) (reduce +)))

(defn solve-2 []
  (->> (read-input)
       (map fuel2)
       (reduce +)))

