(ns advent2018.day08)

(def example-input
  [2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2])

(defn read-input []
  (->> (slurp "resources/day08.txt") (format "[%s]") read-string))

(defn parse [xs num-nodes] ;; returns [{:children [{...}] :meta [1 2 3]}]
  (loop [xs xs, num-nodes num-nodes, nodes []] 
    (if (zero? num-nodes)
      [nodes xs]
      (let [[[num-children num-meta] xs] (split-at 2 xs)
            [children xs] (if (pos? num-children) (parse xs num-children) [[] xs])
            [meta xs]     (split-at num-meta xs)]
        (recur xs
               (dec num-nodes)
               (conj nodes {:children children :meta meta}))))))

;; (clojure.pprint/pprint (->> (parse example-input 1) first))

(defn part1 [input]
  (let [tree (ffirst (parse input 1))]
    (->> (tree-seq :children :children tree)
         (mapcat :meta)
         (reduce +))))

;; (part1 example-input)
;; (part1 (read-input))

;; (clojure.pprint/pprint data)
;; (clojure.walk/postwalk evaluate (first data))
;; (->> (tree-seq :children :children (first data)) (mapcat :meta) (reduce +))
                                   
