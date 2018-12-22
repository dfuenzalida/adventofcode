(ns advent2018.day11)

(defn power-level [^long x ^long y ^long serial]
  (let [rack-id (+ x 10)
        pow-lvl (-> rack-id (* y) (+ serial) (* rack-id) (/ 100) int (rem 10) (- 5))]
    pow-lvl))

;; (power-level 3,5 8)      ;; => 4
;; (power-level 122,79 57)  ;; => -5
;; (power-level 217,196 39) ;; => 0
;; (power-level 101,153 71) ;; => 4

(def grid-size 300)

(defn power-grid [serial]
  (into {} (for [x (range grid-size)
                 y (range grid-size)]
             {[x y] (power-level x y serial)})))

(defn group-power [^long x ^long y grid]
  (reduce + (for [dx (range 3)
                  dy (range 3)]
              (grid [(+ x dx) (+ y dy)]))))

(defn max-group [[x1 y1 pow1] [x2 y2 pow2]]
  (if (> pow1 pow2) [x1 y1 pow1] [x2 y2 pow2]))

(defn best-group [serial]
  (let [grid   (power-grid serial)
        groups (for [x (range (- grid-size 2))
                     y (range (- grid-size 2))]
                 [x y (group-power x y grid)])]
    (reduce max-group groups)))

;; (best-group 18)
;; (best-group 7347)

;; part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn bit-divide [n] ;; returns a seq of powers of 2 that sum N
  (filter #(pos? (bit-and n %)) [256 128 64 32 16 8 4 2 1]))

;; (bit-divide 300)
