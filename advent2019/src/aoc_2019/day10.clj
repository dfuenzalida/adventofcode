(ns aoc-2019.day10)

(defn gcd [a b]
  (if (zero? b) a (recur b (mod a b))))

(defn coords [strs]
  (let [width  (count (first strs))
        height (count strs)
        vs     (-> (mapv (partial mapv str) strs))]
    (for [x (range width)
          y (range height)
          :when (= "#" (-> vs (nth y) (nth x)))]
      [x y])))

(defn deltas [[a b] [c d]]
  [(- a c) (- b d)])

(defn normalize [[a b]]
  (let [d (gcd a b)
        d (if (neg? d) (* -1 d) d)]
    [(quot a d) (quot b d)]))

(defn visible [points location]
  (let [result (->> (for [point points
                          :when (not= point location)]
                      (normalize (deltas point location)))
                    set
                    count)]
    [result location]))

(defn best-location [strs]
  (let [points (coords strs)]
    (apply max-key (comp first (partial visible points)) points)))

(defn read-input []
  (->> (slurp "resources/input10.txt")
       clojure.string/split-lines
       vec))

(defn solve-1 []
  (let [input (read-input)
        best  (best-location input)]
    (first (visible (coords input) best))))

;; (solve-1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn angle [[dx dy]]
  (let [deg (-> (Math/atan2 (* -1 dy) dx) (/ Math/PI) (* 180))]
    (mod (+ 0 (- 90 deg)) 360)))

(defn groups-by-angle [input]
  (let [best (best-location input)
        cs   (-> (coords input) set (disj best))]
    (->> (group-by (comp angle #(deltas % best)) cs)
         (map (fn [[k v]] [k (set v)]))
         (into {}))))

(defn ast-distance [[a b] [c d]]
  (+ (* (- a c) (- a c)) (* (- b d) (- b d))))

(defn laser-list [input]
  (let [best (best-location input)]
    (loop [grouped (groups-by-angle input)
           ord-keys (->> grouped keys sort cycle) ;; (take 1000))
           res []]
      (if (< (count res) 299)
        (let [key   (first ord-keys)
              p-set (grouped key)]
          (if (seq p-set)
            (let [closest (->> p-set (sort-by (partial ast-distance best)) first)
                  p-set'  (disj p-set closest)]
              (recur (assoc grouped key p-set') (rest ord-keys) (conj res closest)))
            (recur grouped (rest ord-keys) res)))
        res))))

(defn solve-2 []
  (let [[x y] (nth (laser-list (read-input)) 199)]
    (+ (* 100 x) y)))

;; (solve-2)

