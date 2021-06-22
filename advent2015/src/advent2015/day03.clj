(ns advent2015.day03
  (:require [clojure.string :refer [trim]]))

(defn read-input []
  (trim (slurp "resources/day03.txt")))

(def movements
  {\^ [0 -1] \v [0 1] \< [-1 0] \> [1 0]})

(defn move [[x y] [dx dy]]
  [(+ x dx) (+ y dy)])

(defn part-1 []
  (->> (read-input) ;; "^v^v^v^v^v" ;; "^>v<"
       (map movements)
       (reductions move)
       (concat [[0 0]])
       frequencies
       (filter (comp pos? second))
       count))

;; (part-1)

(defn coords-by [f]
  (->> (read-input)
       (map-indexed vector)
       (filter (comp f first))
       (map (comp movements second))
       (reductions move)
       (concat [[0 0]])))

(defn part-2 []
  (->> (concat (coords-by odd?) (coords-by even?))
       frequencies
       (filter (comp pos? second))
       count))

;; (part-2)
