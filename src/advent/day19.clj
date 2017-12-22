(ns advent.day19
  (:gen-class))

(def example-map ["     |          "
                  "     |  +--+    "
                  "     A  |  C    "
                  " F---|----E|--+ "
                  "     |  |  |  D "
                  "     +B-+  +--+ "])

(def left  {:up :left, :left :down, :down :right, :right :up})
(def right {:up :right, :right :down, :down :left, :left :up})
(def speed {:up [0 -1] :down [0 1] :left [-1 0] :right [1 0]})
(def alpha?(into #{} "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(defn find-start [pipe-map]
  [(-> pipe-map first (.indexOf "|")) 0])

(defn travel [pipe-map letters [x y] dir dist]
  (let [c       (-> pipe-map (get y) (.charAt x))
        [sx sy] (speed dir)]
    (cond
      (= \space c) [(apply str letters) dist]
      (alpha? c) (recur pipe-map
                        (conj letters c)
                        [(+ x sx) (+ y sy)]
                        dir
                        (inc dist))
      (= \+ c) (let [speedl  (-> dir left speed)
                     speedr  (-> dir right speed)
                     [xl yl] [(+ x (first speedl)) (+ y (second speedl))]
                     [xr yr] [(+ x (first speedr)) (+ y (second speedr))]
                     cl      (-> pipe-map (get yl) (.charAt xl))]
                 (if (not= \space cl)
                   (recur pipe-map letters [xl yl] (left dir) (inc dist))
                   (recur pipe-map letters [xr yr] (right dir) (inc dist))))
      :else   (let [[sx sy] (speed dir)]
                (recur pipe-map letters [(+ x sx) (+ y sy)] dir (inc dist))))))

;; (travel example-map [] (find-start example-map) :down 0)

(defn read-input []
  (-> (slurp "src/input19.txt")
      (clojure.string/split #"\n")))

(def input-map (read-input))

;; parts 1 and 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (travel input-map [] (find-start input-map) :down 0)
