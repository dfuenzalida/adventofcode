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
  (if (>= pow1 pow2) [x1 y1 pow1] [x2 y2 pow2]))

(defn best-group [serial]
  (let [grid   (power-grid serial)
        groups (for [x (range (- grid-size 2))
                     y (range (- grid-size 2))]
                 [x y (group-power x y grid)])]
    (reduce max-group groups)))

;; (time (best-group 18))
;; (time (best-group 7347))
;; (time (best-group2 7347))

;; part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn max-group2 [[x1 y1 size1 pow1] [x2 y2 size2 pow2]]
  (let [best (if (> pow1 pow2)
               [x1 y1 size1 pow1]
               [x2 y2 size2 pow2])]
    ;; (println "best so far:" best)
    best))

(defn max-group2' [[x1 y1 size1 pow1] [x2 y2 size2 pow2]]
  (let [best (if (> pow1 pow2)
               [x1 y1 size1 pow1]
               [x2 y2 size2 pow2])]
    (println "best so far:" best)
    best))

(declare group-power2new-memo)

(defn group-power2new [grid x y size]
  (if (= 1 size)
    (grid [x y])
    (let [prev-power       (group-power2new-memo grid x y (dec size))
          right-col-power  (reduce + (for [dy (range (- size 1))]
                                       (grid [(+ x size -1) (+ y dy)])))
          bottom-row-power (reduce + (for [dx (range (- size 1))]
                                       (grid [(+ x dx) (+ y size -1)])))
          bottom-right     (grid [(+ x size -1) (+ y size -1)])]
      (+ prev-power right-col-power bottom-row-power bottom-right))))

(def group-power2new-memo (memoize group-power2new))

(defn best-of-size [grid size]
  ;; (println size)
  (let [all-tuples (for [x    (range 1 (- 300 size))
                         y    (range 1 (- 300 size))]
                     [x y size (group-power2new-memo grid x y size)])
        best       (reduce max-group2 all-tuples)]
    (println "best:" best)
    best))

(defn part2 [serial]
  (let [grid       (power-grid serial)
        all-tuples (for [size (range 1 33)]
                     (best-of-size grid size))]
    (reduce max-group2' all-tuples)))

;; (time (part2 18))

;; (time (part2 7347))
;; "Elapsed time: 79467.896779 msecs"

;; Visualization of the grid below. Positive values in green, negative in red.
(comment

  ;; Import the Java image APIs
  (import 'java.awt.image.BufferedImage)
  (import 'javax.imageio.ImageIO)

  ;; Map value to boosted color for better visualization
  (defn to-color [z]
    (int
     (if (pos? z)
       (min (* 256 64 z) 0xff00)
       (min (* -1 256 256 64 z) 0xff0000))))

  ;; Renders the grid as a PNG image
  (defn render-grid [serial]
    (let [grid (power-grid serial)
          image (java.awt.image.BufferedImage.
                 (int grid-size)
                 (int grid-size)
                 java.awt.image.BufferedImage/TYPE_INT_RGB)]
      (dorun
       (for [x (range grid-size)
             y (range grid-size)
             :let [rgb (to-color (grid [x y]))]]
         (.setRGB image x y rgb)))
      (ImageIO/write image "png" (java.io.File. (str "output-" serial ".png")))))

  ;; (render-grid 18)
  ;; (render-grid 7347)
)
