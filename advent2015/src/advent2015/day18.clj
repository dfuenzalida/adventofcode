(ns advent2015.day18)

(defn parse-line [xs]
  (mapv #(= \# %) xs))

(defn read-input []
  (->> (slurp "resources/day18.txt")
       clojure.string/split-lines
       (mapv parse-line)))

(defn build-map [] ;; set of [x y] coords that are turned on
  (let [input (read-input)]
    (->> (for [j (range 100)
               i (range 100)
               :when (get-in input [j i])]
           [i j])
         (reduce conj! (transient #{}))
         persistent!)))

(def neighbors
  [[-1 -1] [0 -1] [1 -1] [-1 0] [1 0] [-1 1] [0 1] [1 1]])

(defn num-neighbors [m [x y]]
  (count
   (for [[dx dy] neighbors
         :when (m [(+ x dx) (+ y dy)])]
     true)))

(defn next-state [m]
  (->> (for [y (range 100)
             x (range 100)
             :let [n (num-neighbors m [x y])]
             :when (cond
                     (m [x y]) (<= 2 n 3)
                     :else (= 3 n))]
         [x y])
       (reduce conj! (transient #{}))
       persistent!))

(defn iterate-with [iter-fn]
  (->> (build-map)
       (iterate iter-fn)
       (drop 100)
       first
       count))

(defn part-1 []
  (iterate-with next-state))

;; (part-1)

(defn next-state-with-stuck-on-corners [m]
  (into (next-state m)
        #{[0 0] [0 99] [99 0] [99 99]})) ;; 4 corners always on

(defn part-2 []
  (iterate-with next-state-with-stuck-on-corners))

;; (part-2)

