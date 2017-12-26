(ns advent.day13
  (:gen-class))

(def example [[0 3] [1 2] [4 4] [6 4]])

(def input [[0 3] [1 2] [2 5] [4 4] [6 4] [8 6] [10 6] [12 6] [14 8]
            [16 6] [18 8] [20 8] [22 8] [24 12] [26 8] [28 12] [30 8]
            [32 12] [34 12] [36 14] [38 10] [40 12] [42 14] [44 10]
            [46 14] [48 12] [50 14] [52 12] [54 9] [56 14] [58 12]
            [60 12] [64 14] [66 12] [70 14] [76 20] [78 17] [80 14]
            [84 14] [86 14] [88 18] [90 20] [92 14] [98 18]])

(defn caught? [[depth range]]
  (zero? (mod depth (* 2 (dec range)))))

(defn severity [xs]
  (->> xs
      (filter caught?)
      (map (fn [[a b]] (* a b)))
      (reduce +)))

;; (severity example)

;; part 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (severity input)

(defn caught2? [t [depth range]]
  (zero? (mod (+ t depth) (* 2 (dec range)))))

(defn undetected? [xs t]
  (->> xs
       (filter (partial caught2? t))
       empty?))

;; part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (loop [xs example t 0] (if (undetected? xs t) t (recur xs (inc t)))) => 10
;; (loop [xs input t 0] (if (undetected? xs t) t (recur xs (inc t))))
