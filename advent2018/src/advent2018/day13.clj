(ns advent2018.day13
  (:require [clojure.string :as s]))

;; Example input is a bit crooked because '\' needs to be escaped
(def example-input ["/->-\\        "
                    "|   |  /----\\"
                    "| /-+--+-\\  |"
                    "| | |  | v  |"
                    "\\-+-/  \\-+--/"
                    "  \\------/   "])

(def cart-direction
  {"<" :left, ">" :right, "^" :up, "v" :down})

(defn char-at [s i]
  (.substring s i (inc i)))

(defn find-carts [tube-map]
  (for [y (range (count tube-map))
        x (range (count (first tube-map)))
        :let [piece (get (mapv str (get tube-map y)) x)]
        :when (#{"<" ">" "^" "v"} piece)]
    [x y (cart-direction piece) (cycle [:left :straight :right])]))

;; (find-carts example-input) ;; => ([2 0 :right] [9 3 :down])
                        
(def left  {:up :left, :left :down, :down :right, :right :up})
(def right {:up :right, :right :down, :down :left, :left :up})
(def speed {:up [0 -1] :down [0 1] :left [-1 0] :right [1 0]})

;; TODO remember to replace the carts original position with tracks:
;; < and > are horizontal tracks: '-', ^ and v are vertical tracks: '|'.

(defn crash? [carts] ;; [[x y :dir turn-cycles] ... ]
  (->> carts
       (map (juxt first second))
       frequencies
       (filter (fn [[k v]] (> v 1)))
       ffirst))

;; (crash? [[1 0 :up] [2 0 :down] [2 0 :left]]) ;; => [2 0]

;; "when going :right and enter a '\' we turn right (and end facing :down)
(def right-turns
  #{[:right "\\"] [:down "/"] [:left "\\"] [:up "/"]})

(def left-turns
  #{[:right "/"] [:down "\\"] [:left "/"] [:up "\\"]})

(defn cart-order [[x y & _]]
  (+ (* 10000 y) x))

(defn move-cart [tube-map [x y dir turns]]
  (let [c       (char-at (nth tube-map y) x)]
    (cond
      (= "+" c) (let [turning (first turns)
                      dir2    (get {:left (left dir) :right (right dir)} turning dir)
                      speed2  (get speed dir2)
                      [x2 y2] [(+ x (first speed2)) (+ y (second speed2))]]
                  [x2 y2 dir2 (rest turns)])

      :else (let [nextdir-fn (cond (right-turns [dir c]) right
                                   (left-turns  [dir c]) left
                                   :else identity)
                  dir2       (nextdir-fn dir)
                  [dx dy]    (speed dir2)
                  [x2 y2]    [(+ x dx) (+ y dy)]]
              [x2 y2 dir2 turns]))))

(defn part1 [input]
  (let [tube-map input]
    (loop [ticks 0, carts (find-carts input), moved-carts []]
      (let [crashed (crash? (concat carts moved-carts))]
        (if (or crashed (< 1000 ticks))
          [crashed ticks]
          (if (seq carts)
            (recur ticks (rest carts) (conj moved-carts
                                            (move-cart tube-map (first carts))))
            (recur (inc ticks) (sort-by cart-order moved-carts) [])))))))

(defn read-input []
  (->> (slurp "resources/day13.txt")
       s/split-lines))

;; (part1 example-input) ;; => [7 3]
;; (part1 (read-input))
