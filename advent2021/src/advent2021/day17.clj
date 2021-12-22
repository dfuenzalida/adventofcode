(ns advent2021.day17)

(defn trace-probe
  ([[vx vy]] (trace-probe [0 0] [vx vy]))
  ([[x y] [vx vy]]
   (cons [x y]
         (lazy-seq
          (let [vx' (max 0 (dec vx))
                vy' (dec vy)]
            (trace-probe [(+ x vx) (+ y vy)] [vx' vy']))))))

(defn not-past-target? [[left right bottom top] [x y]]
  (and (<= x right) (<= bottom y)))

(defn meets-target? [[left right bottom top] [vx vy]]
  (let [xs (trace-probe [vx vy])]
    (->> (take-while (partial not-past-target? [left right bottom top]) xs)
         (some (fn [[x y]] (and (<= left x right) (<= bottom y top)))))))

(defn gauss [n]
  (quot (* n (inc n)) 2))

(defn horizontal-vel-range [[left right bottom top]]
  (->> (range)
       (drop-while #(< (gauss %) left))
       (take-while #(<= % right))))

(defn vertical-vel-range [[left right bottom top]]
  (->> (range)
       (map #(+ % bottom))
       (drop-while #(< (gauss %) bottom))
       (take-while #(<= % (* -1 bottom)))))

(defn part-1 [target]
  (let [coords (for [vx (horizontal-vel-range target)
                     vy (vertical-vel-range target)
                     :when (meets-target? target [vx vy])]
                 (take-while (partial not-past-target? target) (trace-probe [vx vy])))]
    (->> (reduce concat coords)
         (map second)
         (reduce max))))

(defn read-input []
  (->> (slurp "resources/day17.txt")
       (re-seq #"[-]?\d+")
       (mapv read-string)))

;; (= 45 (part-1 [20 30 -10 -5]))
;; (part-1 (read-input))

(defn part-2 [target]
  (let [coords (for [vx (horizontal-vel-range target)
                     vy (vertical-vel-range target)
                     :when (meets-target? target [vx vy])]
                 [vx vy])]
    (count (set coords))))

;; (= 112 (part-2 [20 30 -10 -5]))
;; (part-2 (read-input))

(comment
  (take-while (partial above-target? [20 30 -10 -5]) (trace-probe [7 2]))

  (meets-target? [20 30 -10 -5] [7 2])
  (meets-target? [20 30 -10 -5] [6 3])
  (meets-target? [20 30 -10 -5] [9 0])
  (meets-target? [20 30 -10 -5] [17 -4])
  (meets-target? [20 30 -10 -5] [6 9])

  (horizontal-vel-range [20 30 -10 -5])
  (vertical-vel-range [20 30 -10 -5])
  
  )
