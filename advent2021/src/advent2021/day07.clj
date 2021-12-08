(ns advent2021.day07)

(def example "16,1,2,0,4,2,7,1,2,14")

(defn read-input [s]
  (->> s (format "[%s]") read-string))

(defn abs [x]
  (if (pos? x) x (* -1 x)))

(defn cost [m x] ;; cost of moving entries in m to position x
  (->> (map (fn [[k v]] (* v (abs (- x k)))) m)
       (reduce +)))

(defn part-1 [s]
  (let [freqs (->> s read-input frequencies)
        a (reduce min (keys freqs))
        b (reduce max (keys freqs))]
    (->> (map (partial cost freqs) (range a (inc b)))
         (reduce min))))

;; (= 37 (part-1 example))
;; (= 41 (-> example read-input frequencies (cost 1)))
;; (= 39 (-> example read-input frequencies (cost 3)))
;; (= 71 (-> example read-input frequencies (cost 10)))
;; (part-1 (slurp "resources/day07.txt"))

(defn cost2 [m x]
  (->> (map
        (fn [[k v]]
          (let [dist (abs (- x k))]
            (quot (* v dist (inc dist)) 2)))
        m)
       (reduce +)))

(defn part-2 [s]
  (let [freqs (->> s read-input frequencies)
        a (reduce min (keys freqs))
        b (reduce max (keys freqs))]
    (->> (map (partial cost2 freqs) (range a (inc b)))
         (reduce min))))

;; (= 168 (part-2 example))
;; (= 206 (-> example read-input frequencies (cost2 2)))
;; (part-2 (slurp "resources/day07.txt"))

