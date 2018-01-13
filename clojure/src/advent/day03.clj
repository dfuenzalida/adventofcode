(ns advent.day03
  (:gen-class))

;; 17  16  15  14  13
;; 18   5   4   3  12
;; 19   6  [1]  2  11
;; 20   7   8   9  10
;; 21  22  23---> ...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; part 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn moves [n]
  (concat (repeat n (if (odd? n) :right :left))
          (repeat n (if (odd? n) :up :down))))

(defn path [n]
  (->> (range)
       (drop 1)
       (map moves)
       (apply concat)
       (take (dec n))))

(def shifts {:left [-1 0] :right [1  0]
             :up   [ 0 1] :down  [0 -1]})

(defn distance [n]
  (let [steps (map shifts (path n))]
    (+
     (Math/abs (reduce + (map first steps)))
     (Math/abs (reduce + (map second steps))))))

;; (distance 1024) ;; => 31
;; (distance 289326)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; part 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def all-moves ;; lazy-seq of all the moves from the center of the grid
  (->> (range)
       (drop 1)
       (map moves)
       (apply concat)))

(defn coords-plus [[a b] [c d]]
  [(+ a c) (+ b d)])

(defn compute-grid [grid [x y]]
  (reduce +
          (for [i [-1 0 1]
                j [-1 0 1]
                :when (and (not= 0 i j)
                           (some? (grid [(+ x i) (+ y j)])))]
            (grid [(+ x i) (+ y j)]))))

(defn fill-grid [grid [x y] moves-seq limit]
  (let [curr (compute-grid grid [x y])]
    (if (>= curr limit)
      curr
      (let [new-grid  (assoc grid [x y] curr)
            curr-move (first moves-seq)
            new-coord (coords-plus [x y] (shifts curr-move))]
        (fill-grid new-grid new-coord (rest moves-seq) limit)))))

;; start with a single 1 at the center, computing from
;; the first cell on the right

;; (fill-grid {[0 0] 1} [1 0] (drop 1 all-moves) 80)
;; (fill-grid {[0 0] 1} [1 0] (drop 1 all-moves) 289326)
