(ns advent.day21
  (:gen-class))

(def example-rules ["../.# => ##./#../..."
                    ".#./..#/### => #..#/..../..../#..#"])

(def hacker-grid [".#." "..#" "###"])

(defn rotate [xs] ;; clockwise
  (let [size (count (first xs))]
    (into []
          (for [i (range size)]
            (apply str
                   (for [j (range size)]
                     (.charAt (get xs (- size j 1)) i)))))))

(defn flip [xs]
  (mapv #(-> % StringBuffer. .reverse .toString) xs))

(defn variations [xs] ;; rotate and flip a figure in every possible way
  (concat (take 4 (iterate rotate xs))
          (take 4 (drop 1 (iterate rotate (flip xs))))))

(defn parse-line [s]
  (let [[_ input output] (first (re-seq #"(.*) => (.*)" s))
        input            (clojure.string/split input #"/")
        output           (clojure.string/split output #"/")]
    [input output]))

(defn rules-map [xs] ;; for each input, compute every variation and map to the output
  (let [io-rules (map parse-line xs)]
    (reduce merge {}
            (map
             #(zipmap (variations (first %)) (repeat (second %)))
             io-rules))))

(defn subdivide [xs size]
  (let [pieces (quot (count xs) size)]
    (into []
          (for [j (range pieces)]
            (into []
                  (for [i (range pieces)]
                    (into []
                          (for [n (range size)]
                            (.substring (get xs (+ (* size j) n))
                                        (* size i)
                                        (* size (inc i)))))))))))

(defn apply-rules [xs rules]
  (into []
        (for [j (range (count xs))]
          (into []
                (for [i (range (count xs))]
                  (let [elem (get (get xs j) i)]
                    (rules elem)))))))

(defn recombine [xs]
  (->> xs
       (map (partial apply map str))
       (apply concat)
       (into [])))

(defn iter [xs rules n]
  (if (neg? n)
    xs
    ;; on each iteration:
    ;; - subdivide grid into cells
    ;; - for each cell, apply the art rules
    ;; - recombine cells back into grid
    (let [size (if (zero? (mod (count xs) 2)) 2 3)
          divided (subdivide xs size)
          applied (apply-rules divided rules)]
      (recur (recombine applied) rules (dec n)))))

(defn read-input []
  (-> (slurp "src/input21.txt")
      (clojure.string/split #"\n")))

(def rules (rules-map (read-input)))

#_(->> (reduce str (iter hacker-grid rules (dec 5)))
     (filter #{\#})
     count)

#_(->> (reduce str (iter hacker-grid rules (dec 18)))
     (filter #{\#})
     count)

