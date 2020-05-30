(ns advent2016.day03)

(defn read-input []
  (->> (slurp "resources/day03.txt")
       (re-seq #"\d+")
       (map read-string)))

(defn valid-triangle? [[a b c]]
  (and (< a (+ b c)) (< b (+ a c)) (< c (+ a b))))

;; (valid-triangle? [3 4 5])
;; (valid-triangle? [5 10 25])

(defn part-1 []
  (->> (read-input)
       (partition 3)
       (filter valid-triangle?)
       count))

;; (part-1)

(defn part-2 []
  (let [rows (->> (read-input) (partition 3) (mapv vec))
        cols (->> (for [i (range 3), j (range (count rows))]
                    ((rows j) i))
                  (partition 3))]
    (->> cols
         (filter valid-triangle?)
         count)))

;; (part-2)

