(ns advent2015.day15)

(defn read-input []
  (->> (slurp "resources/day15.txt")
       (re-seq #"-?\d+")
       (map clojure.edn/read-string) (partition 5)))

(defn pos-or-zero [x]
  (if (pos? x) x 0))

(defn score [f xs ys]
  (->> (map (fn [xs y] (map * xs (repeat y))) xs ys)
       (apply map +) ;; vertically sum the properties
       f
       butlast ;; drop the calories from the score
       (map pos-or-zero)
       (reduce *)))

;; Lazy, non-general approach because I know there are just 4 ingredients
(defn max-score [f xs]
  (let [xs (read-input)]
    (reduce max
            (for [a (range 101)
                  b (range (- 101 a))
                  c (range (- 101 a b))
                  :let [d (- 100 a b c)]]
              (score f xs [a b c d])))))

(defn part-1 []
  (let [xs (read-input)]
    (max-score identity xs)))

;; (part-1)

(defn enforce-500-calories [xs]
  ;; force a zero score for mixes that don't have 500 cals exact
  (if (= 500 (last xs)) xs [0]))

(defn part-2 []
  (let [xs (read-input)]
    (max-score enforce-500-calories xs)))

;; (part-2)

