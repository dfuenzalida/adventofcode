(ns advent2018.day08)

(def example-input
  [2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2])

;; part 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn sum-metadata [xs num-nodes total]
  (let [[num-children num-meta] (take 2 xs)]
    (if (zero? num-nodes)
      [xs total]
      (if (zero? num-children)
        (recur (drop (+ 2 num-meta) xs)
               (dec num-nodes)
               (reduce + total (take num-meta (drop 2 xs))))
        (let [[xs total] (sum-metadata (drop 2 xs) num-children total)]
          (let [meta (take num-meta xs)]
            (recur (drop num-meta xs) (dec num-nodes) (reduce + total meta))))))))

(defn read-input []
  (->> (slurp "resources/day08.txt") (format "[%s]") read-string))

;; (last (sum-metadata example-input 1 0)) => 138
;; (last (sum-metadata (read-input) 1 0))

