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

;; (clojure.pprint/pprint (->> (parse example-input 1) ffirst))

(defn part1 [input]
  (let [tree (ffirst (parse input 1))]
    (->> (tree-seq :children :children tree)
         (mapcat :meta)
         (reduce +))))

;; (part1 example-input) => 138
;; (part1 (read-input))

(defn eval-tree [{:keys [children meta]}]
  (if (empty? children)
    (reduce + meta) ;; childless node? sum of its meta entries
    (let [num-children (count children)
          indexes      (->> meta (filter pos?) (map dec) (filter #(< % num-children)))]
      (reduce + (map #(eval-tree (get children %)) indexes)))))

(defn part2 [input]
  (let [tree (ffirst (parse input 1))]
    (eval-tree tree)))

;; (part2 example-input) => 66
;; (part2 (read-input))
