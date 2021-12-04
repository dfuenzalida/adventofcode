(ns advent2021.day01)

(declare example)

(defn parse-input [s]
  (read-string (format "[%s]" s)))

(defn part-1 [s]
  (let [input (parse-input s)]
    (->> (map < input (rest input))
         (filter true?)
         count)))

;; (= 7 (part-1 example))
;; (part-1 (slurp "resources/day01.txt"))

(defn part-2 [s]
  (let [input   (parse-input s)
        windows (->> (partition 3 1 input) (map (partial apply +)))]
    (->> (map < windows (rest windows))
         (filter true?)
         count)))

;; (= 5 (part-2 example))
;; (part-2 (slurp "resources/day01.txt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def example "199
200
208
210
200
207
240
269
260
263")
