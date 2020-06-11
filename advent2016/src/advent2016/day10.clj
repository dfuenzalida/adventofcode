(ns advent2016.day10)

(def example ["value 5 goes to bot 2"
              "bot 2 gives low to bot 1 and high to bot 0"
              "value 3 goes to bot 1"
              "bot 1 gives low to output 1 and high to bot 0"
              "bot 0 gives low to output 2 and high to output 0"
              "value 2 goes to bot 2"])

(def value-goes-re #"value (\d+) goes to (\w+) (\d+)")
(def bot-gives-re #"bot (\d+) gives low to (\w+) (\d+) and high to (\w+) (\d+)")

(defn assign-value [m value target index] ;; gives the `value` to the bot/io in the `target` key and `index`
  (if (= :bot target)
    (let [values (get-in m [target index :values] [])] ;; bots have a collection of values
      (update-in m [target index] merge {:values (into values [value])}))
    (assoc-in m [target index] value))) ;; i/o buckets hold a single value

(defn process-value-goes [m line]
  (if-let [[_ value target index] (re-matches value-goes-re line)]
    (let [target        (keyword target)
          [value index] (map read-string [value index])]
      (assign-value m value target index))
    m))

(defn process-bot-gives [m line]
  (if-let [[_match botn targetL indexL targetH indexH] (re-matches bot-gives-re line)]
    (let [[botn indexL indexH] (map read-string [botn indexL indexH])
          [targetL targetH]    (map keyword [targetL targetH])]
      (update-in m [:bot botn] merge {:low-to [targetL indexL] :high-to [targetH indexH]}))
    m))

(defn can-distribute? [{:keys [values low-to high-to]}]
  (and (= 2 (count values)) low-to high-to)) ;; has 2 values and both rules to distribute low/high values

;; if any bot has two values, updates `m` according to the rules that are loaded so far,
;; if any bot holds `low-chip` and `high-chip`, stops and returns [bot low-chip high-chip]
;; if no update was required, returns nil

(defn distribute [m low-chip high-chip]
  (let [[bot vals] (->> (:bot m) (filter (fn [[_k v]] (can-distribute? v))) first)]
    (when-let [{:keys [values low-to high-to]} vals]
      (let [[valL valH] (sort values)]
        (if (= [valL valH] [low-chip high-chip])
          [bot low-chip high-chip]
          (let [[targetL indexL] low-to
                [targetH indexH] high-to
                m' (-> (assoc-in m [:bot bot :values] [])   ;; remove the values under old path
                       (assign-value valL targetL indexL)   ;; update in low path adding low value
                       (assign-value valH targetH indexH))] ;; update in high path adding high value
            (when-not (= m m') (recur m' low-chip high-chip))))))))

(defn find-comparator-bot [m low-chip high-chip lines]
  (when-let [line (first lines)]
    (let [m'  (-> m (process-value-goes line) (process-bot-gives line))
          res (distribute m' low-chip high-chip)]
      (if res
        res
        (recur m' low-chip high-chip (rest lines))))))

;; (find-comparator {:bot {1 {:values [3]}, 2 {:values [2]}}} 2 5 example)
;; => [2 2 5] ;; bot #2 compares the chips #2 and #5

(defn part-1 []
  (->> (slurp "resources/day10.txt")
       clojure.string/split-lines
       (find-comparator-bot {} 17 61))) ;; returns [bot# 17 61]

;; (part-1)

(defn distribute2 [m]
  (let [[bot vals] (->> (:bot m) (filter (fn [[_k v]] (can-distribute? v))) first)]
    (if-let [{:keys [values low-to high-to]} vals]
      (let [[valL valH] (sort values)
            [targetL indexL] low-to
            [targetH indexH] high-to
            m' (-> (assoc-in m [:bot bot :values] [])   ;; remove the values under old path
                   (assign-value valL targetL indexL)   ;; update in low path adding low value
                   (assign-value valH targetH indexH))] ;; update in high path adding high value
        (if (= m m') m (recur m')))
      m)))

(defn find-outputs []
  (let [lines (->> (slurp "resources/day10.txt") clojure.string/split-lines)]
    (loop [m {}, lines lines]
      (if-let [line (first lines)]
        (let [m' (-> m (process-value-goes line) (process-bot-gives line) distribute2)]
          (recur m' (rest lines)))
        m))))

(defn part-2 []
  (let [outputs (:output (find-outputs))]
    (* (outputs 0) (outputs 1) (outputs 2))))

;; (part-2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lots of REPL tests below:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment

  ;; (distribute {:bot {2 {:values [5 6] :low-to [:bot 5] :high-to [:bot 3]}, 3 {:values [4]}}})
  
  (let [m {:bot {2 {:values [6 5] :low-to [:bot 5] :high-to [:bot 3]}, 3 {:values [4]}}}]
    (let [[bot vals] (->> (:bot m) (filter (fn [[k v]] (= 2 (count (get v :values []))))) first)]
      [bot vals]))
  
  (reduce process-value-goes {} ["value 5 goes to bot 2" "value 6 goes to bot 2"])

  (process-value-goes {} (first example))
  
  (process-bot-gives {} (second example))

  (->> (slurp "resources/day10.txt")
       clojure.string/split-lines
       #_(filter (partial re-matches value-goes-re)) ;; 21
       #_(filter (partial re-matches bot-gives-re)) ;; 210
       count)

  (assoc-in {} [:a 1 :b 2] :ok)
  (update-in {:a {1 {:b 2 :c 0}}} [:a 1] merge {:c 3})

  (re-matches #"value (\d+) goes to (\w+) (\d+)" "value 37 goes to bot 97")
  ;; => ["value 37 goes to bot 97" "37" "bot" "97"]

  (re-matches #"bot (\d+) gives low to (\w+) (\d+) and high to (\w+) (\d+)" "bot 169 gives low to output 19 and high to output 11")
  ;; => ["bot 169 gives low to output 19 and high to output 11" "169" "output" "19" "output" "11"]

  ;; problem state shape:
  {:bot {2 {:values [2 5] :low-to [:output 1] :high-to [:bot 0]}} ;; bot 2 has chips 2 and 5, sends low value to output 1, high value to bot #0
   :output {0 3} ;; chip 3 in output bin #0
   :input  {3 4} ;; chip 4 in input bin #3
   }

  )
