(ns aoc-2019.day08)

(defn read-input []
  (->> (slurp "resources/input08.txt")
       (map (comp clojure.edn/read-string str))
       (partition (* 25 6))))

(defn zero-count [layer]
  (count (filter #{0} layer)))

(defn solve-1 []
  (let [few-zero-layer (apply min-key zero-count (read-input))
        ones           (->> few-zero-layer (filter #{1}) count)
        twos           (->> few-zero-layer (filter #{2}) count)]
    (* ones twos)))

;; (solve-1)

;; Part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn pixel-reducer [& pxs]
  (reduce (fn [acc px] (or (#{0 1} px) acc)) 0 pxs))

(defn solve-2 []
  (->> (apply map pixel-reducer (reverse (read-input)))
       (map {0 " " 1 "#"})
       (partition 25)
       (mapv (partial apply str))))

;; (dorun (map println (solve-2)))

