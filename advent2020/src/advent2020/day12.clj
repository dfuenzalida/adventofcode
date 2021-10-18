(ns advent2020.day12)

(def example
  [["F" 10] ["N" 3] ["F" 7] ["R" 90] ["F" 11]])

(defn parse-line [[_ c n]]
  [c (read-string n)])

(defn read-input []
  (->> (slurp "resources/day12.txt")
       (re-seq #"(\w)(\d+)")
       (map parse-line)))

;; (read-input)

(def angle-forward ;; angle grows from X axis to Y axis, counterclockwise
  {0 [1 0] 90 [0 1] 180 [-1 0] 270 [0 -1]})

(defn move [[x y angle] [c n]] ;; -> [x' y' angle']
  (condp = c
    "N" [x (+ y n) angle]
    "S" [x (- y n) angle]
    "E" [(+ x n) y angle]
    "W" [(- x n) y angle]
    "L" [x y (rem (+ 360 angle n) 360)]
    "R" [x y (rem (+ 360 angle (* -1 n)) 360)]
    "F" (let [[dx dy] (get angle-forward angle [0 0])]
          [(+ x (* dx n)) (+ y (* dy n)) angle])))

;;  (= [17 -8] (take 2 (reduce move [0 0 0] example)))

(defn mdistance [[x y _]]
  (+ (Math/abs x) (Math/abs y)))

;; (= 25 (mdistance (reduce move [0 0 0] example)))

(defn part-1 []
  (->> (read-input) (reduce move [0 0 0]) mdistance))

;; (part-1)

(defn rotate-around [x y wx wy angle]
  (if (zero? angle)
    [x y wx wy]
    (rotate-around x y (* -1 wy) wx (- angle 90))))

(defn move2 [[x y wx wy] [c n]] ;; -> [x' y' angle' wx' wy']
  (condp = c
    "N" [x y wx (+ wy n)]
    "S" [x y wx (- wy n)]
    "E" [x y (+ wx n) wy]
    "W" [x y (- wx n) wy]
    "L" (rotate-around x y wx wy n)
    "R" (rotate-around x y wx wy (- 360 n))
    "F" [(+ x (* n wx)) (+ y (* n wy)) wx wy]))

;; (= (move2 [0 0 10 1] ["F" 10]) [100 10 10 1])
;; (= (move2 [100 10 10 1] ["N" 3]) [100 10 10 4])
;; (= (move2 [100 10 10 4] ["F" 7]) [170 38 10 4])
;; (= (move2 [170 38 10 4] ["R" 90]) [170 38 4 -10])
;; (= 286 (mdistance (reduce move2 [0 0 10 1] example)))

(defn part-2 []
  (->> (read-input) (reduce move2 [0 0 10 1]) mdistance))

;; (part-2)
