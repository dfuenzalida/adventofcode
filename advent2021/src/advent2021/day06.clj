(ns advent2021.day06)

(declare example example18)

(defn read-input [s]
  (->> (format "[%s]" s) read-string frequencies))

(defn iterate-fish [m]
  (let [zeros (get m 0 0)]
    (->> (filter (comp pos? first) m)
         (map (fn [[k v]] [(dec k) v]))
         (reduce merge {})
         (merge-with + {8 zeros 6 zeros}))))

;; (iterate-fish (read-input example))

(defn count-fish [s n]
  (let [state (nth (iterate iterate-fish (read-input s)) n)]
    (->> (map second state) (reduce +))))

;; (= 26 (count-fish example 18))
;; (= 5934 (count-fish example 80))
;; (count-fish (slurp "resources/day06.txt") 80)

;; part 2
;; (= 26984457539 (count-fish example 256))

;; (time (count-fish (slurp "resources/day06.txt") 256))
;; => "Elapsed time: 6.320288 msecs"

(comment
  (-> (iterate iterate-fish (read-input example))
       (nth 18))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def example "3,4,3,1,2")
(def example18 "6,0,6,4,5,6,0,1,1,2,6,0,1,1,1,2,2,3,3,4,6,7,8,8,8,8")

