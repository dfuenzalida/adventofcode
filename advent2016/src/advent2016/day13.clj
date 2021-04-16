(ns advent2016.day13)

(defn wall? [n x y]
  (let [poly (+ (* x x) (* 3 x) (* 2 x y) y (* y y) n)
        bits (Long/toBinaryString poly)
        ones (count (filter #{\1} bits))]
    (odd? ones)))

;; (wall? 10 9 6)

(defn minimap [n width height]
  (let [rows (for [y (range height)
                   x (range width)]
               (if (wall? n x y) \# \.))]
    (->> rows (partition width) (interpose ["\n"]) (reduce concat) (reduce str) println)))

;; (minimap 10 60 39)
;; (minimap 1362 60 39)
;; (println)

;; Start with the points [[1 1]] and distance 0, for each point look in the current step
;; look for neighbors up-left-right-down that are not walls and NOT already in the visited set.
;; The new points are added to the set and we loop over.
;; You can iterate a fixed number of times or until the set contains the target, and we
;; return the current distance.

(def moves [[-1 0] [1 0] [0 1] [0 -1]])

(defn distance [n start end]
  (loop [visited #{start}, distance 0, iter [start]]
    (if (or (some #{end} iter) (> distance 100))
      distance
      (let [nbors (for [[x y]   iter
                        [dx dy] moves
                        :let [x' (+ x dx), y' (+ y dy)]
                        :when (and (false? (wall? n x' y'))
                                   (nil? (visited [x' y'])))]
                    [x' y'])
            vis'  (into visited nbors)]
        (recur vis' (inc distance) nbors)))))

;; (distance 10 [1 1] [7 4]) ;; => 11

(defn part-1 []
  (distance 1362 [1 1] [31 39]))

;; (part-1)

(defn different-points [n max-steps start]
  (loop [visited #{start}, distance 0, iter [start]]
    (if (>= distance max-steps)
      (count visited)
      (let [nbors (for [[x y]   iter
                        [dx dy] moves
                        :let [x' (+ x dx), y' (+ y dy)]
                        :when (and (>= x' 0) (>= y' 0) ;; negative positions invalid
                                   (false? (wall? n x' y'))
                                   (nil? (visited [x' y'])))]
                    [x' y'])
            vis'  (into visited nbors)]
        (recur vis' (inc distance) nbors)))))

(defn part-2 []
  (different-points 1362 50 [1 1]))

;; (part-2)
