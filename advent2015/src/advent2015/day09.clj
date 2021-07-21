(ns advent2015.day09)

;; Load the table above as tuples [[a b] dist] and also [[b a] dist]
;;
;; Starting from every pair [a b], look for other pairs [b c] and build a list so that
;; the cities are not repeated in the list. We keep track of the unused arcs and stop
;; if the next round of unused arcs is empty. The path must have every city to be valid.

(def example
  ["London to Dublin = 464"
   "London to Belfast = 518"
   "Dublin to Belfast = 141"])

(defn read-input [lines]
  (->> lines
       (map (fn [s] (let [[a _to b c] (re-seq #"[\w\d]+" s)
                          dist        (read-string c)]
                      {[a b] dist, [b a] dist})))
       (reduce merge {})))

;; (read-input example)

;; `tuples` is a map of [a b] to distance
;; `path` is [[a b] [b c] ... ]
(defn build-paths [tuples path]
  (if-let [end (second (last path))]
    (let [visited (set (apply concat path))
          candidates (filter
                      (fn [[a b]] (and (= end a) (not (visited b))))
                      (keys tuples))]
      (if (seq candidates)
        (let [paths' (mapv #(into path [%]) candidates)]
          (mapcat (partial build-paths tuples) paths'))
        [path]))

    ;; spread all possible paths
    (mapcat #(build-paths tuples [%]) (keys tuples))))

(defn path-distance [tuples path]
  (->> (map tuples path)
       (reduce + 0)))

(defn optimize-distance [f]
  (let [tuples (->> (slurp "resources/day09.txt") clojure.string/split-lines read-input)
        paths  (build-paths tuples [])]
    (->> (map (partial path-distance tuples) paths)
         (reduce f))))

(defn part-1 []
  (optimize-distance min))

;; (part-1)

(defn part-2 []
  (optimize-distance max))

;; (part-2)

(comment

  (build-paths (read-input example) [["London" "Dublin"]])
  (build-paths (read-input example) [])

  ;; part 1 with example data
  (let [tuples (read-input example)
        paths  (build-paths tuples [])]
    (->> (map (partial path-distance tuples) paths)
         (reduce min)))
  ;; => 605

  (clojure.pprint/pprint (build-paths (read-input example) []))

  )
