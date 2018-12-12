(ns advent2018.day07
  (:require [clojure.string :as s]))

(def example-input
  "Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.")

(defn load-graph [input]
  (let [lines     (s/split-lines input)
        pairs     (->> lines
                       (map #(re-seq #"Step (\w) .* (\w) can begin." %))
                       (map first)
                       (map rest))
        nodes     (set (into (map first pairs) (map second pairs)))
        defaults  (reduce into {} (map (fn [s] {s []}) nodes))]
    (merge-with into defaults (->> pairs
                                   (map (fn [[k v]] {k [v]}))
                                   (apply merge-with into)))))

;; (load-graph example-input) ;; => {"E" [], "C" ["A" "F"], "F" ["E"], "B" ["E"], "A" ["B" "D"], "D" ["E"]}

(defn graph-order [input]
  (loop [graph (load-graph input)
         plan  []]
    (if (empty? graph)
      (apply str plan)
      (let [parents    (set (keys graph))
            dependents (set (mapcat second graph))
            next-task  (first (sort (for [parent parents
                                          :when (not (dependents parent))]
                                      parent)))]
        (recur (dissoc graph next-task) (conj plan next-task))))))

;; (graph-order example-input) => "CABDFE"
;; (graph-order (slurp "resources/day07.txt"))

;; part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn duration [s base]
  (let [dur (-> s s/upper-case first int (- (int \A)) (+ 1 base))]
    [s dur]))

(defn simulate [input num-workers base-duration]
  (loop [graph   (load-graph input)
         working [] ;; [[name duration] ... ]
         ts      -1]
    ;; (println ts "\t" working) ;; DEBUG
    (if (or (every? empty? [graph working]) (> ts 1000))
      ts
      (let [worked     (mapv #(update-in % [1] dec) working)
            worked-ns  (->> worked (map first) set)
            working-2  (filter (comp pos? second) worked)
            finished   (->> worked (filter (comp zero? second)) (map first))
            graph-2    (reduce dissoc graph finished)
            parents    (set (keys graph-2))
            dependents (set (mapcat second graph-2))
            next-tasks (->> (for [parent parents
                                  :when (and (not (dependents parent))
                                             (not (worked-ns parent)))]
                              parent)
                            sort
                            (take (- num-workers (count working-2)))
                            (map #(duration % base-duration)))]

        (recur graph-2 (into working-2 next-tasks) (inc ts))))))

;; (simulate example-input 2 0)
;; (simulate (slurp "resources/day07.txt") 5 60)

