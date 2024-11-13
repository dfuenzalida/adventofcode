(ns advent2022.day14
  (:require [clojure.pprint]
            [clojure.string :as string]))

(defn read-input [s]
  (->> (string/split-lines s)
       (map (fn [line] (->> (re-seq #"\d+" line)
                            (map read-string)
                            (partition 4 2))))
       (reduce concat)))

(defn draw-segments [rset xs]
  (if-let [x (first xs)]
    (let [[x1 y1 x2 y2] x
          r' (reduce conj rset (for [i (range (min x1 x2) (inc (max x1 x2)))
                                     j (range (min y1 y2) (inc (max y1 y2)))]
                                 [i j]))
          xs' (rest xs)]
      (recur r' xs'))
    rset))

(defn draw [{:keys [rocks sands]}] ;; for debugging
  (->> (for [j (range 0 13)
             i (range 490 510)]
         (if (rocks [i j]) "#" (if (sands [i j]) "o" ".")))
       (partition 20)
       (mapv (partial reduce str))
       clojure.pprint/pprint))

;; (read-input example)
;; (draw-segments (sorted-set) (read-input example))
;; (->> example read-input (draw-segments (sorted-set)) (assoc {:sands #{}} :rocks) draw)

(defn iterate-sand [state]
  (let [{:keys [rocks sands lowest x y]} state]
    (if (> y lowest)
      (assoc state :falling true)
      (let [opts [[x (inc y)] [(dec x) (inc y)] [(inc x) (inc y)]]
            opt  (->> opts (remove rocks) (remove sands) first)]
        (if (nil? opt)
          (-> state
              (update-in [:sands] conj [x y])
              (merge {:x 500 :y 0})) ;; put another grain back up
          (let [[x y] opt
                state' (merge state {:x x :y y})]
            (recur state')))))))

(defn part-1 [s]
  (let [rocks  (->> s read-input (draw-segments (sorted-set)))
        lowest (->> rocks (map second) (reduce max))]
    (->> (iterate iterate-sand {:rocks rocks :sands #{} :lowest lowest :x 500 :y 0})
         rest ;; drop the initial state
         (take-while (comp nil? :falling))
         count)))

;; (= 24 (part-1 example))
;; (part-1 (slurp "resources/day14.txt"))

(defn iterate-sand2 [state]
  (let [{:keys [rocks sands lowest x y]} state]
    (let [opts [[x (inc y)] [(dec x) (inc y)] [(inc x) (inc y)]]
          opt  (->> opts (remove rocks) (remove sands) first)]
      (if (or (nil? opt) (> y lowest))
        (-> state
            (update-in [:sands] conj [x y]) ;; rest this sand grain
            (merge {:x 500 :y 0}))          ;; put another grain back up
        (let [[x y] opt
              state' (merge state {:x x :y y})]
          (recur state'))))))

(defn part-2 [s]
  (let [rocks  (->> s read-input (draw-segments (sorted-set)))
        lowest (->> rocks (map second) (reduce max))]
    (->> (iterate iterate-sand2 {:rocks rocks :sands #{} :lowest lowest :x 500 :y 0})
         (partition 2 1)
         (take-while (fn [[a b]] (not (= a b)))) ;; "while there is some change"
         count)))

;; (= 93 (part-2 example))
;; (part-2 (slurp "resources/day14.txt"))

(comment

  (let [rocks  (->> example read-input (draw-segments (sorted-set)))
        lowest (->> rocks (map second) (reduce max))]
    (->> (iterate iterate-sand2 {:rocks rocks :sands #{} :lowest lowest :x 500 :y 0})
         ;; rest ;; drop the initial state
         #_(take 200)
         (partition 2 1)
         (take-while (fn [[a b]] (not (= a b))))
         count
         #_(drop 93)
         #_first
         #_draw))

  ;; lowest of actual input
  (let [s (slurp "resources/day14.txt")
        rocks  (->> s read-input (draw-segments (sorted-set)))
        lowest (->> rocks (map second) (reduce max))]
    lowest)
  
  )

(def example
  "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9")
