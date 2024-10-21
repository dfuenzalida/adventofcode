(ns advent2022.day09
  (:require [clojure.string :as string]))

(defn read-input [s]
  (->> (string/split-lines s)
       (map #(read-string (str "[\\" % "]")))))

;; diagonally adjacent and even overlapping also count as touching
(defn touching? [[hx hy] [tx ty]]
  (and (<= (abs (- hx tx)) 1) (<= (abs (- hy ty)) 1)))

(def moves {\U [0 1] \D [0 -1] \L [-1 0] \R [1 0]})

(defn delta [a b]
  ;;  (if (= a b) 0 (/ (- b a) (abs (- b a)))))
  (compare b a))

(defn move-tail [[hx hy] [tx ty]]
  ;; If the head is ever two steps directly up, down, left, or right from the tail,
  ;; the tail must also move one step in that direction so it remains close enough.
  ;; Otherwise, if the head and tail aren't touching and aren't in the same row or column,
  ;; the tail always moves one step diagonally to keep up
  (cond
    (and (= 2 (abs (- hx tx))) (= hy ty)) [(+ tx (delta tx hx)) ty]
    (and (= 2 (abs (- hy ty))) (= hx tx)) [tx (+ ty (delta ty hy))]
    :else (if (touching? [hx hy] [tx ty])
            [tx ty]
            [(+ tx (delta tx hx)) (+ ty (delta ty hy))])))

(defn reduce-movements [tail-positions [hx hy] [tx ty] motions]
  (if-let [motion (first motions)]
    (let [[dx dy] (get moves motion)
          hx (+ hx dx)
          hy (+ hy dy)
          [tx ty] (move-tail [hx hy] [tx ty])
          tail-positions (conj tail-positions [tx ty])]
      (recur tail-positions [hx hy] [tx ty] (rest motions)))
    tail-positions))

(defn unroll-motions [xs]
  (mapcat (fn [[c n]] (repeat n c)) xs))

(defn part-1 [s]
  (let [motions (unroll-motions (read-input s))
        tail-ps (reduce-movements #{} [0 0] [0 0] motions)]
    (count tail-ps)))

;; (= 13 (part-1 example))
;; (part-1 (slurp "resources/day09.txt"))

(defn move-knots [result current remaining]
  (if-let [knot (first remaining)]
    (let [knot' (move-tail current knot)]
      (recur (conj result current) knot' (rest remaining)))
    (conj result current)))

(defn reduce-movements2 [tail-positions knot-pos motions]
  (if-let [motion (first motions)]
    (let [[dx dy] (get moves motion)
          hx (+ (ffirst knot-pos) dx)
          hy (+ (second (first knot-pos)) dy)
          knot-pos (move-knots [] [hx hy] (rest knot-pos))
          tail-positions (conj tail-positions (last knot-pos))]
      (recur tail-positions knot-pos (rest motions)))
    tail-positions))

(defn part-2 [s]
  (let [motions (unroll-motions (read-input s))
        tail-ps (reduce-movements2 #{} (into [] (repeat 10 [0 0])) motions)]
    (count tail-ps)))

;; (= 1 (part-2 example))
;; (= 36 (part-2 example2))
;; (part-2 (slurp "resources/day09.txt"))

(comment

  (read-input example)
  ((fn [a b] (/ (- b a) (abs (- b a)))) 2 4)

  )


(def example
  "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2")

(def example2
  "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20")
