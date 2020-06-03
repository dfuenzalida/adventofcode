(ns advent2016.day08)

(defn print-state [m] ;; map of [x y] to \c
  (println "--------------------------------------------------")
  (->> (for [j (range 6) i (range 50)] (get m [i j] " "))
       (partition 50)
       (map #(apply str %))
       (clojure.string/join "\n")
       println)
  (println "--------------------------------------------------"))

(defn rect [state x y]
  (let [coords (for [j (range y) i (range x)] [i j])]
    (reduce #(assoc %1 %2 \#) state coords)))

(defn rotate-row [state row offset]
  (let [old-coords (filter (fn [[[x y] c]] (= row y)) state)
        new-coords (map (fn [[[x y] c]] [[(mod (+ x 50 offset) 50) y] c]) old-coords)
        state      (reduce dissoc state (map first old-coords))
        state      (into state new-coords)]
    state))

(defn rotate-col [state col offset]
  (let [old-coords (filter (fn [[[x y] c]] (= col x)) state)
        new-coords (map (fn [[[x y] c]] [[x (mod (+ y 6 offset) 6)] c]) old-coords)
        state      (reduce dissoc state (map first old-coords))
        state      (into state new-coords)]
    state))

(def dispatches {#"rect (\d+)x(\d+)" rect
                 #"rotate row y=(\d+) by (\d+)" rotate-row
                 #"rotate column x=(\d+) by (\d+)" rotate-col})

(defn draw [state line]
  (let [[matches f] (->> (map (fn [[re f]] [(re-matches re line) f]) dispatches)
                         (filter first)
                         first)
        [arg1 arg2] (->> matches rest (map read-string))]
    (f state arg1 arg2)))

(defn parts-1-2 []
  (let [input (clojure.string/split-lines (slurp "resources/day08.txt"))
        state (reduce draw {} input)]
    (print-state state)
    (count state)))

;; (parts-1-2)

