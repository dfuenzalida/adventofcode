(ns advent2025.day09
  (:require [clojure.string :as s]
            [clojure.edn :as edn]))

(declare example)

(defn read-input [s]
  (->> (format "[%s]" s)
       edn/read-string
       (partition 2)
       (mapv vec)))

(defn rectangles [points]
  (for [i (range (count points))
        j (range (inc i) (count points))
        :let [p1 (nth points i)
              p2 (nth points j)]]
    [p1 p2]))

(defn area [[[^long x1 ^long y1] [^long x2 ^long y2]]]
  (* (inc (Math/abs (- x1 x2))) (inc (Math/abs (- y1 y2)))))

;; (= 50 (area [[2 5] [11 1]]))

(defn max-area [rectangles]
  (->> rectangles (map area) (reduce max 0)))

;; (->> example read-input rectangles (some #{[[11 1] [2 5]]}))

(defn part-1 [input]
  (->> input rectangles max-area))

;; (->> example read-input part-1)
;; (->> "resources/day09.txt" slurp read-input part-1)

;; IDEA:
;; Sort the corners to form the shape, alternating the same X coordinate, then the same Y coordinate

(defn sort-corners [points]
  (loop [sorted    [(first points)]
         remaining (set (rest points))
         same-x?   true]
    (if (empty? remaining)
      sorted
      (let [[x1 y1] (last sorted)
            next-p  (first (filter (fn [[x2 y2]] (if same-x? (= x1 x2) (= y1 y2))) remaining))]
        (recur (into sorted [next-p]) (disj remaining next-p) (not same-x?))))))

;; (->> example read-input sort-corners)

;; (defn outline [points]
;;   (let [points (take (inc (count points)) (cycle points))] ;; 

(defn intermediate-points [[x1 y1] [x2 y2]]
  (let [stepx (if (< x1 x2) 1 -1)
        stepy (if (< y1 y2) 1 -1)]
    (for [x (if (= x1 x2) (range x1 (inc x1)) (range (+ x1 stepx) x2 stepx))
          y (if (= y1 y2) (range y1 (inc y1)) (range (+ y1 stepy) y2 stepy))]
      [x y])))

;; (trace-points [2 3] [2 5])

(defn outline [points]
  (let [pairs (partition 2 1 (take (inc (count points)) (cycle points)))]
    (loop [res   []
           pairs pairs]
      (if-let [[p1 p2] (first pairs)]
        (recur (-> res
                   (into [p1])
                   (into (intermediate-points p1 p2))
                   (into [p2]))
               (rest pairs))
        res))))

;; (outline [[3 1] [3 3] [6 3] [6 1]])

;; Cast a line from beyond the boundaries of the outline, vertically and horizontally,
;; up to the point, count the times the point in the line went from not-in-the outline-set
;; to in the line-set. If the number of transitions is odd, the tile is inside the outline

(defn inside? [outline-set [x y]]
  (or (outline-set [x y]) ;; the outline tiles also count as "inside"
      (let [minx (->> outline-set (map first) (reduce min) dec)
            miny (->> outline-set (map second) (reduce min) dec)
            matches-x (loop [res 0
                             prev false
                             x1 minx]
                        (if (= x1 (+ 1 x))
                          res
                          (let [in-set? (contains? outline-set [x1 y])]
                            (if (and (not prev) in-set?)
                              (recur (inc res) in-set? (inc x1))
                              (recur res in-set? (inc x1))))))]
        #_matches-x
        (odd? matches-x))))

(def inside-m? (memoize inside?))

;; (->> example read-input outline (map first) (reduce min) dec)
;; (false? (-> example read-input outline set (contains? [2 2])))
;; (false? (-> example read-input outline set (inside? [2 2])))
;; (-> example read-input outline set (inside? [7 1]))
;; (-> example read-input outline set (inside? [8 2]))
;; (-> example read-input outline set (inside? [9 3]))
;; (-> example read-input outline set (inside? [10 4]))

;; A rectangle is inside if every tile is inside

(defn rectangle-inside? [outline-set [x1 y1] [x2 y2]]
  (let [tiles (for [x (range (min x1 x2) (inc (max x1 x2)))
                    y (range (min y1 y2) (inc (max y1 y2)))]
                [x y])]
    (every? (partial inside-m? outline-set) tiles)))

;; (-> example read-input outline set (rectangle-inside? [7 1] [9 3]))
;; (false? (-> example read-input outline set (rectangle-inside? [2 2] [5 5])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NEW IDEA FOR FAST LOOKUP
;; To determine if a tile is inside, we create 2 maps, one from an X coordinate
;; to a list of Y coordinates of the outline tiles at that X coordinate, and
;; another map from a Y coordinate to a list of the X coordinates of the tiles
;; at that Y coordinate.
;;
;; A tile is inside if:
;; - it's one of the same coordinates (e.g. looking-up for a given X returns
;;   the same Y as one of the tiles, OR
;; - when looking up for a given X, the number Y coordinates that are lesser than
;;   the Y coordinate of the tile is an ODD number (and same with the Y coord)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NEW IDEA FOR FAST LOOKUP
;;
;; A tile of coordinates [i j] is INSIDE the shape defined by segments
;; if the number of segments whose `y` coordinate less-or-equal to `j`,
;; AND those segments contain `i`
;; (e.g. for the segment `[[x1 y] [x2 y]]` then `(<= x1 i x2)` is `true`), then
;; the number of segments is odd.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn segments [points]
  (let [pairs (partition 2 1 (take (inc (count points)) (cycle points)))]
    (mapv (fn [[[a b] [c d]]]
            (if (= a c) [[a (min b d)] [c (max b d)]] [[(min a c) b] [(max a c) d]]))
          pairs)))

#_ (= (segments [[1 3] [1 5] [5 5] [5 3]])
      [[[1 3] [1 5]] [[1 5] [5 5]] [[5 3] [5 5]] [[1 3] [5 3]]])

;; (->> example read-input sort-corners segments)

(defn inside-ss? [ss [^long i ^long j]]
  (or
   ;; same X coordinate
   (->> ss
        (filter (fn [[[^long a ^long b] [^long c ^long d]]] (<= a c i)))
        (filter (fn [[[^long a ^long b] [^long c ^long d]]] (<= b j d)))
        count
        odd?)
   ;; same Y coordinate
   #_(->> ss
        (filter (fn [[[^long a ^long b] [^long c ^long d]]] (<= b d j)))
        (filter (fn [[[^long a ^long b] [^long c ^long d]]] (<= a i c)))
        count
        odd?)
   ))

(def inside-ss-memo?
  (memoize inside-ss?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NEW IDEA
;; There's no need to check for the whole inside of the rectangle to be contained.
;; Only checking that the outline is contained is sufficient.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn rectangle-inside2'? [sgmts [x1 y1] [x2 y2]]
  (let [tiles (for [x (range (min x1 x2) (inc (max x1 x2)))
                    y (range (min y1 y2) (inc (max y1 y2)))]
                [x y])]
    (and
     ;; every corner
     (every? (partial inside-ss? sgmts) [[x1 y1] [x2 y2] [x1 y2] [x2 y1]])
     ;; the insides of the rectangle
     (every? (partial inside-ss? sgmts) tiles))))

(defn rectangle-inside2? [sgmts [^long x1 ^long y1] [^long x2 ^long y2]]
  (let [oline     (outline [[x1 y1] [x1 y2] [x2 y2] [x2 y1]])
        ;; _ (println "outline count" (count oline) "area" (area [[x1 y1] [x2 y2]]))
        ;; in-sgmts? (partial inside-ss? sgmts)
        in-sgmts? (partial inside-ss-memo? sgmts)]
    (every? in-sgmts? oline)))

;; [[9 5] [2 3]]
;; (inside-ss? (->> example read-input sort-corners segments) [2 3])
;; (rectangle-inside2? (->> example read-input sort-corners segments) [9 5] [2 3])

(defn part-2 [s]
  (let [input (read-input s)
        rects (->> input rectangles (sort-by area) reverse) ;; by area, desc
        sgmts (->> input sort-corners segments)]
    (->> (filter (fn [[p1 p2]]
                   (rectangle-inside2? sgmts p1 p2))
                 rects)
         first
         area)))

;; (= 24 (part-2 example))
;; (time (-> "resources/day09.txt" slurp part-2-old))
;;
;; (time (-> "resources/day09.txt" slurp part-2))
;; "Elapsed time: 1863286.457304 msecs"

(comment
  (let [input       (read-input example)
        outline-set (-> input outline set)
        rects       (rectangles input)
        rect        (first
                     (filter (fn [p1 p2] (rectangle-inside? outline-set p1 p2)) rects))]
    rect)
  
  (some? #{} "hello")
  
  (contains? #{:a :e :i :o :u} :e)
  (take 6 (cycle (range 5)))

  (render-svg "./example.svg" (->> example read-input))
  (render-svg "./day9.svg" (->> "resources/day09.txt" slurp read-input))

  )

;; Used this function to render the problem data into an SVG file to
;; understand better about the size/shape of the surface

(defn render-svg [filename input]
  (let [scale 100
        input (map (fn [[a b]] [(int (/ a scale)) (int (/ b scale))]) input)
        width  (->> input (map first) (reduce max) inc)
        height (->> input (map second) (reduce max) inc)
        corners (map (fn [[a b]]
                       (format "  <rect width='1' height='1' x='%s' y='%s' fill='blue' />\n" a b)) input)
        oline   (map (fn [[a b]]
                       (format "  <rect width='1' height='1' x='%s' y='%s' fill='lightblue' />\n" a b)) (->> input outline set))]

    (with-open [w (clojure.java.io/writer filename)]
      (.write w (format "<svg width='%s' height='%s' xmlns='http://www.w3.org/2000/svg'>\n" width height))
      (doseq [point oline]
        (.write w point))
      (doseq [corner corners]
        (.write w corner))
      (.write w "</svg>\n"))))

(def example
  "7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3")

;; ..............
;; .......#XXX#..
;; .......X...X..
;; ..#XXXX#...X..
;; ..X........X..
;; ..#XXXXXX#.X..
;; .........X.X..
;; .........#X#..
;; ..............
