(ns advent2018.day18
  (:require [clojure.string :as s]))

(def example ".#.#...|#.
.....#|##|
.|..|...#.
..|#.....#
#.#|||#|#|
...#.||...
.|....|...
||...#|.#|
|.||||..|.
...#.|..|.")

(def example-input (s/split-lines example))

(defn char-at [s i]
  (.substring s i (inc i)))

(defn adjacents [terrain x y]
  (merge
   {"." 0 "|" 0 "#" 0} ;; defaults
   (let [width  (count (first terrain))
         height (count terrain)]
     (frequencies
      (for [dx (range -1 2)
            dy (range -1 2)
            :let [x2 (+ x dx)
                  y2 (+ y dy)]
            :when (and (not= [0 0] [dx dy])
                       (<= 0 x2) (< x2 width)
                       (<= 0 y2) (< y2 width))]
        (let [c (char-at (nth terrain y2) x2)]
          c))))))

;; (adjacents example-input 0 0)

(defn iter-acre [terrain x y]
  (let [acre (char-at (nth terrain y) x)
        adjs (adjacents terrain x y)]
    (cond
      (= "." acre) (let [trees (get adjs "|")]
                     (if (>= trees 3) "|" "."))

      (= "|" acre) (let [lumbs (get adjs "#")]
                     (if (>= lumbs 3) "#" "|"))

      (= "#" acre) (let [lumbs (get adjs "#")
                         trees (get adjs "|")]
                     (if (and (pos? lumbs) (pos? trees)) "#" "."))

      :else "?")))

;; (iter-acre example-input 7 0)

(defn step-terrain [terrain]
  (into
   []
   (let [width  (count (first terrain))
         height (count terrain)]
     (for [y (range height)]
       (apply str
              (for [x (range width)]
                (iter-acre terrain x y)))))))

(defn iter-terrain [terrain]
  (iterate step-terrain terrain))

(defn print-terrain [terrain]
  (dorun (map println terrain))
  (println))

(defn part-1 [input iters]
  (let [terrain (apply str (last (take (inc iters) (iter-terrain input))))
        freqs   (frequencies terrain)
        trees   (get freqs \| 0)
        lumbs   (get freqs \# 0)]
    (* trees lumbs)))

;; (part-1 example-input 10) ;; => 1147

(defn read-input []
  (s/split-lines (slurp "resources/day18.txt")))

;; (part-1 (read-input) 10)
