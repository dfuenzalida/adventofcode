(ns advent2015.day16)

(defn parse-pairs [xs]
  (->> xs
       (map (fn [s]
              (let [[a b] (clojure.string/split s #": ")]
                {a (clojure.edn/read-string b)})))
       (reduce into)))

(defn parse-line [s]
  (-> (re-find #": (.*)" s)
      last
      (clojure.string/split #", ")
      parse-pairs))

;; (parse-line "Sue 1: goldfish: 9, cars: 0, samoyeds: 9")

(defn read-input []
  (->> (slurp "resources/day16.txt")
       clojure.string/split-lines
       (map parse-line)))

;; (read-input)

(defn match-maps [a b] ;; every key-map in `b` match some key-map in `a`
  (let [keys-b (keys b)]
    (= b (select-keys a keys-b))))

(defn part-1 [m]
  (->> (read-input)
       (map-indexed vector)
       (filter (comp (partial match-maps m) second))
       ffirst
       inc)) ;; 1-based index

(def tape-input
  {"children" 3, "cats" 7, "samoyeds" 2, "pomeranians" 3, "akitas" 0,
   "vizslas" 0, "goldfish" 5, "trees" 3, "cars" 2, "perfumes" 1})

;; (part-1 tape-input)

;; * the cats and trees readings indicates that there are GREATER than that many
;; * the pomeranians and goldfish readings indicate that there are FEWER than that many

;; Given a comparator an a property, checks truth of `(comparator (get a property) (get b property))`
(defn compare-maps [property comparator a b]
  (if (and (get a property) (get b property))
    (comparator (get a property) (get b property))
    true)) ;; cannot drop if either map doesn't have the property

(def same-amount-keys ;; keys that can be compared for the same amount
  ["children" "samoyeds" "akitas" "vizslas" "cars" "perfumes"])

(def part-2-criteria
  (juxt (fn [b] (match-maps tape-input (select-keys b same-amount-keys)))
        (partial compare-maps "cats" < tape-input)
        (partial compare-maps "trees" < tape-input)
        (partial compare-maps "pomeranians" > tape-input)
        (partial compare-maps "goldfish" > tape-input)))

(defn part-2-filter [b]
  (every? true? (part-2-criteria b)))

(defn part-2 []
  (->> (read-input)
       (map-indexed vector)
       (filter (comp (partial part-2-filter) second))
       ffirst
       inc)) ;; 1-based index

;; (part-2)

(comment

  (let [a {:a 1 :b 2 :c 3}
        b {:a 1 :b 20 :c 0.3}
        agg (juxt (fn [b] (match-maps a (select-keys b [:a])))
                  (partial compare-maps :b < a)
                  (partial compare-maps :c > a))]
    (every? true? (agg b)))
  )
