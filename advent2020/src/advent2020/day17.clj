(ns advent2020.day17)

(defn active? [xs [x y z]]
  (let [current    (contains? xs [x y z])
        neighbours (for [dx [-1 0 1], dy [-1 0 1], dz [-1 0 1]
                         :let [x' (+ x dx), y' (+ y dy), z' (+ z dz)]
                         :when (and (not= 0 dx dy dz) (contains? xs [x' y' z']))]
                     true)
        num-active (count neighbours)]
    (if current
      (<= 2 num-active 3)
      (= 3 num-active))))

(defn edges [xs] ;; => [minx miny minz maxx maxy maxz]
  (reduce
   (fn [[a b c d e f] [g h i]]
     [(min a g) (min b h) (min c i) (max d g) (max e h) (max f i)])
   [0 0 0 0 0 0] ;; can assume the origin
   xs))

(defn cubes-iterate [xs]
  (let [[a b c d e f] (edges xs)]
    (->> (for [x (range (dec a) (+ 2 d))
               y (range (dec b) (+ 2 e))
               z (range (dec c) (+ 2 f))
               :when (active? xs [x y z])]
           [x y z])
         set)))

(def glider-set
  #{[1 0 0] [2 1 0] [0 2 0] [1 2 0] [2 2 0]})

;; (= 112 (-> (iterate cubes-iterate glider-set) (nth 6) count))

(defn parse-input [s]
  (let [grid (->> s clojure.string/split-lines (mapv vec))]
    (for [y (range (count grid))
          x (range (count (first grid)))
          :when (= \# (get-in grid [y x]))]
      [x y 0])))

;; (parse-input ".#.\n..#\n###")

(defn part-1 []
  (let [grid (->> (slurp "resources/day17.txt") parse-input set)]
    (-> (iterate cubes-iterate grid) (nth 6) count)))

;; (part-1)

;; Part 2 is the same as above, except with one extra dimension, and a bit slower

(defn active4? [xs [x y z w]]
  (let [current    (contains? xs [x y z w])
        neighbours (for [dx [-1 0 1], dy [-1 0 1], dz [-1 0 1], dw [-1 0 1]
                         :let [x' (+ x dx), y' (+ y dy), z' (+ z dz), w' (+ w dw)]
                         :when (and (not= 0 dx dy dz dw) (contains? xs [x' y' z' w']))]
                     true)
        num-active (count neighbours)]
    (if current
      (<= 2 num-active 3)
      (= 3 num-active))))

(defn edges4 [xs] ;; => [minx miny minz minw maxx maxy maxz maxw]
  (reduce
   (fn [[a b c d, e f g h] [i j k l]]
     [(min a i) (min b j) (min c k) (min d l)
      (max e i) (max f j) (max g k) (max h l)])
   [0 0 0 0, 0 0 0 0] ;; can assume the origin
   xs))

(defn cubes-iterate4 [xs]
  (let [[a b c d, e f g h] (edges4 xs)]
    (->> (for [x (range (dec a) (+ 2 e))
               y (range (dec b) (+ 2 f))
               z (range (dec c) (+ 2 g))
               w (range (dec d) (+ 2 h))
               :when (active4? xs [x y z w])]
           [x y z w])
         set)))

(def glider-set4
  #{[1 0 0 0] [2 1 0 0] [0 2 0 0] [1 2 0 0] [2 2 0 0]})

;; (= 848 (-> (iterate cubes-iterate4 glider-set4) (nth 6) count))

(defn parse-input4 [s]
  (let [grid (->> s clojure.string/split-lines (mapv vec))]
    (for [y (range (count grid))
          x (range (count (first grid)))
          :when (= \# (get-in grid [y x]))]
      [x y 0 0])))

(defn part-2 []
  (let [grid (->> (slurp "resources/day17.txt") parse-input4 set)]
    (-> (iterate cubes-iterate4 grid) (nth 6) count)))

;; (part-2)

(comment

  (-> (iterate cubes-iterate glider-set) (nth 6) count)

  )
