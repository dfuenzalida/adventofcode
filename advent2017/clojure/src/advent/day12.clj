(ns advent.day12
  (:gen-class))

;; part 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def example ["0 <-> 2"
              "1 <-> 1"
              "2 <-> 0, 3, 4"
              "3 <-> 2, 4"
              "4 <-> 2, 3, 6"
              "5 <-> 6"
              "6 <-> 4, 5"])

(defn parse-line [s]
  (let [[_ from to] (first (re-seq #"(\d+) <-> (.*)" s))]
    (into #{from} (clojure.string/split to #", "))))

;; (parse-line (get example 2))

(defn grow-set [s ss]
  (reduce (fn [a b] (if (or (some b a) (some a b)) (into a b) a)) s ss))

(defn grow [s ss]
  (let [new-set (grow-set s ss)]
    (if (not= s new-set)
      (recur new-set ss)
      s)))

;; (count (grow #{"0"} (map parse-line example)))

(defn read-input []
  (-> (slurp "src/input12.txt")
      (clojure.string/split #"\n")))

(def problem-input (read-input))

(def zero-set
  (grow #{"0"} (map parse-line problem-input)))

;; (count zero-set)

;; part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn parse-from [s]
  (let [[_ from] (first (re-seq #"(\d+) .*" s))]
    #{from}))

(def froms
  (map parse-from problem-input))

(def all-sets
  (let [input-sets (map parse-line problem-input)]
    (loop [from-sets (reduce into froms)
           sets      #{}]
      (if (empty? from-sets)
        sets
        (let [ffrom (first from-sets)
              gfrom (grow #{ffrom} input-sets)]
          ;; now we remove the contents of gfrom from from-sets
          (recur
           (reduce disj from-sets gfrom)
           (conj sets gfrom)))))))

;; (count all-sets)
