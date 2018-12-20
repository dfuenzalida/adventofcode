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

(defn char-at [^String s ^Integer i]
  (.substring s i (inc i)))

(defn adjacents [terrain ^Integer x ^Integer y]
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

(defn iter-acre [terrain ^Integer x ^Integer y]
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

;; part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (->> (take 50 (iter-terrain example-input)) (map print-terrain))

(defn find-fixed-terrain [terrain-iterator]
  (let [tuples (map-indexed vector (take 1000 terrain-iterator))]
    (loop [tuples tuples, seen {}]
      (let [[i terr] (first tuples)]
        (if (seen terr)
          [i (seen terr)] ;; [current-index index-of-previously-seen-terrain]
          (recur (rest tuples) (assoc seen terr i)))))))

;; (find-fixed-terrain (read-input))

(defn part-2 [terrain]
  (let [terr-i (iter-terrain terrain)
        [b a]  (find-fixed-terrain terr-i)
        period (- b a)
        index  (+ a (mod (- 1000000000 a) period))
        terr   (first (drop index terr-i))
        freqs  (frequencies (apply str terr))]
    (* (get freqs \| 0) (get freqs \# 0))))

;; (time (part-2 (read-input))) ;; => "Elapsed time: 28760.035084 msecs"
;; After optimization (recycling iterator) => "Elapsed time: 14794.859524 msecs"

