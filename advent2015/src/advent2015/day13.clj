(ns advent2015.day13)

(defn insert-at [xs y i]
  (->> (concat (take i xs) [y] (drop i xs))
       (into [])))

(defn combs [xs]
  (if (= 1 (count xs))
    [xs]
    (let [x  (first xs)
          ys (combs (rest xs))]
      (mapcat
       (fn [zs] (map (partial insert-at zs x) (range (count xs))))
       ys))))

;; (combs [:a :b :c])

(def example
  ["Alice would gain 54 happiness units by sitting next to Bob."
   "Alice would lose 79 happiness units by sitting next to Carol."
   "Alice would lose 2 happiness units by sitting next to David."
   "Bob would gain 83 happiness units by sitting next to Alice."
   "Bob would lose 7 happiness units by sitting next to Carol."
   "Bob would lose 63 happiness units by sitting next to David."
   "Carol would lose 62 happiness units by sitting next to Alice."
   "Carol would gain 60 happiness units by sitting next to Bob."
   "Carol would gain 55 happiness units by sitting next to David."
   "David would gain 46 happiness units by sitting next to Alice."
   "David would lose 7 happiness units by sitting next to Bob."
   "David would gain 41 happiness units by sitting next to Carol."])

(defn parse-line [s]
  (let [xs (re-seq #"[\w\d]+" s)
        [name1 _ verb n _ _ _ _ _ _ name2] xs
        delta (({"gain" + "lose" -} verb) (read-string n))]    
    {[name1 name2] delta}))

(defn read-input [lines]
  (->> (map parse-line lines)
       (reduce merge {})))

(defn score [m xs]
  (let [c (count xs)
        pairs (concat (take c (partition 2 1 (cycle xs)))
                      (take c (partition 2 1 (cycle (reverse xs)))))]
    (->> (map m pairs)
         (remove nil?)
         (reduce + 0))))

(defn names-in [m]
  (->> (keys m) (reduce into) set))

(defn part-1 [lines]
  (let [input (read-input lines)
        names (names-in input)]
    (->> (combs names)
         (map (partial score input))
         (reduce max 0))))

;; (= (part-1 example) 330)

;; takes ~1700ms
;; (time (part-1 (clojure.string/split-lines (slurp "resources/day13.txt"))))

(defn part-2 [lines]
  (let [input (read-input lines)
        names (conj (names-in input) :you)]
    (->> (combs names)
         (map (partial score input))
         (reduce max 0))))

;; takes ~16 secs
;; (time (part-2 (clojure.string/split-lines (slurp "resources/day13.txt"))))

