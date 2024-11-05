(ns advent2022.day12
  (:require [clojure.string :as string]))

(defn read-input [s]
  (->> s string/split-lines (mapv (partial into []))))

(defn coords-of [xss pred]
  (for [j (range (count xss))
        i (range (count (first xss)))
        :when (pred (nth (nth xss j) i))]
    [i j]))

(defn not-reached-end? [end {:keys [inside edge]}]
  (and
   (not (empty? edge))
   (nil? (some #{end} edge))))

(defn expand-steps [xss {:keys [inside edge]}]
  (let [width  (count (first xss))
        height (count xss)
        pset   (into (set inside) edge)]
    (for [[x y]   edge
          [dx dy] [[-1 0] [1 0] [0 -1] [0 1]]
          :let [h  (nth (nth xss y) x)
                h  (if (= \S h) \a h) ;; Start has height \a
                x' (+ x dx) y' (+ y dy)]
          :when (and (< -1 x' width)
                     (< -1 y' height)
                     (nil? (pset [x' y']))) ;; Don't go over previous steps
          :let [h' (nth (nth xss y') x')
                h' (if (= \E h') \z h')]
          :when (<= (- (int h') (int h)) 1)]
      [x' y'])))

(defn iterate-expand [xss state]
  (let [{:keys [inside edge]} state]
    {:inside (into (set inside) edge) :edge (set (expand-steps xss state))}))

(defn part-1 [s]
  (let [xss   (read-input s)
        start (first (coords-of xss #{\S}))
        end   (first (coords-of xss #{\E}))]
    (->> (iterate (partial iterate-expand xss) {:inside [] :edge [start]})
         (take-while (partial not-reached-end? end))
         count)))

;; (= 31 (part-1 example))
;; (part-1 (slurp "resources/day12.txt"))

(defn part-2 [s]
  (let [xss    (read-input s)
        starts (coords-of xss #{\S \a})
        end    (first (coords-of xss #{\E}))]
    (->> (iterate (partial iterate-expand xss) {:inside [] :edge starts})
         (take-while (partial not-reached-end? end))
         count)))

;; (= 29 (part-2 example))
;; (part-2 (slurp "resources/day12.txt"))

(comment
  (-> example read-input (coords-of "E"))

  (let [xss   (read-input example)
        start (first (coords-of xss #{\S}))
        end   (first (coords-of xss #{\E}))]
    (->> (iterate (partial iterate-expand xss) {:inside [] :edge [start]})
         (take-while (partial not-reached-end? end))
         count
         ))

  )

(def example
  "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi")
