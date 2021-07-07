(ns advent2015.day19
  (:require [clojure.string :refer [includes? replace-first split split-lines starts-with?]]))

(defn parse-line [s]
  (let [[k v] (clojure.string/split s #" => ")]
    {k [v]}))

(defn read-input []
  (let [lines (split-lines (slurp "resources/day19.txt"))
        molecule (last lines)
        pairs (->> (take-while seq lines)
                   (map parse-line)
                   (reduce (partial merge-with into) {}))]
    [pairs molecule]))

(defn indexes-of [s x]
  (->> (map (partial subs s) (range (count s)))
       (map-indexed vector)
       (filter #(starts-with? (second %) x))
       (map first)))

(defn replace-at [s x y i]
  (let [cx (count x)]
    (str (subs s 0 i) y (subs s (+ i cx)))))

(defn replace-once [m s]
  (for [k (keys m)
        i (indexes-of s k)
        v (get m k)]
    (replace-at s k v i)))

(defn part-1 []
  (let [[pairs molecule] (read-input)
        m (reduce merge {} pairs)]
    (->> (replace-once m molecule)
         (reduce conj #{})
         count)))

;; (part-1)

(defn parse-line2 [s]
  (let [[k v] (split s #" => ")]
    {v k}))

(defn read-input2 []
  (let [lines (split-lines (slurp "resources/day19.txt"))
        molecule (last lines)
        pairs (->> (take-while seq lines)
                   (map parse-line2)
                   (reduce (partial merge-with into) {}))]
    [pairs molecule]))

(defn shrink-molecule [s goal m]
  (loop [i 0, s s]
    ;; (println i s)
    (if (= s goal)
      i
      (let [ks (filter (partial includes? s) (keys m))
            k  (first ks)]
        (recur (inc i)
               (replace-first s k (m k)))))))

(defn part-2 []
  (let [[m molecule] (read-input2)]
    (shrink-molecule molecule "e" m)))

;; (part-2)

