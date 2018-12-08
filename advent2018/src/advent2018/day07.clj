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
  (let [lines (s/split-lines input)]
    (->> lines
         (map #(re-seq #"Step (.*) must be finished before step (.*) can begin." %))
         (map first)
         (map rest)
         (map (fn [[k v]] {k [v]}))
         (apply merge-with into))))

;; (load-graph example-input) ;; =>  {"C" ["A" "F"], "A" ["B" "D"], "B" ["E"], "D" ["E"], "F" ["E"]}

(defn graph-order [input]
  (loop [graph (load-graph input)
         plan  []]
    (if (empty? graph)
      (let [graph     (load-graph input)
            all-tasks (sort (set (concat (keys graph) (reduce into (vals graph)))))
            plan-set  (set plan)]
        (apply
         str
         (into plan (for [task all-tasks, :when (not (plan-set task))] task))))
      (let [parents    (set (keys graph))
            dependents (set (mapcat second graph))
            next-task  (first (sort (for [parent parents
                                          :when (not (dependents parent))]
                                      parent)))]
        (recur (dissoc graph next-task) (conj plan next-task))))))

;; (graph-order example-input) => "CABDFE"
;; (graph-order (slurp "resources/day07.txt"))

;; part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
