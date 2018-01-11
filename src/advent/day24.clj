(ns advent.day24
  (:gen-class))

(def example ["0/2" "2/2" "2/3" "3/4"
              "3/5" "0/1" "10/1" "9/10"])

(defn parse-line [s]
  (let [[_ a b] (first (re-seq #"(\d+)/(\d+)" s))
        pair    [(Integer. a) (Integer. b)]]
    pair))

(defn match-components [n xs]
  (->> xs
       (filter #(some #{n} %))
       (map (fn [[a b]] (if (= n a) [a b] [b a])))))

(defn remove-one [[a b] xs]
  (if (seq xs)
    (let [f (first xs)]
      (if (or (= [a b] f) (= [b a] f))
        (rest xs)
        (into [f] (remove-one [a b] (rest xs)))))
    []))

(defn strength [xs]
  (+ (reduce + 0 (map first xs))
     (reduce + 0 (map second xs))))

(defn iter [xs-atom bridge pieces]
  ;; check if any pieces are usable
  (let [last-pin (second (last bridge))
        usables  (match-components last-pin pieces)]
    (do
      (if (seq usables)
        ;; map over the usables, adding it to the bridge, removing it from the pieces
        (map 
         #(iter xs-atom (into bridge [%]) (remove-one % pieces))
         usables)
        ;; no usable piece? end here
        (if (>= (strength bridge) (strength @xs-atom))
          (do
            ;; (println "*" (strength bridge) bridge)
            (reset! xs-atom bridge))))
      xs-atom)))

(defn read-input []
  (-> (slurp "src/input24.txt")
      (clojure.string/split #"\n")))

#_(let [ex-atom        (atom [[0 0]])
        example-pieces (map parse-line example)]
    (do
      (iter ex-atom [[0 0]] example-pieces)
      (strength @ex-atom)))

;; (def pieces (map parse-line (read-input)))
;; (iter part-a [[0 0]] pieces)

(defn iter2 [xs-atom bridge pieces]
  ;; check if any pieces are usable
  (let [last-pin (second (last bridge))
        usables  (match-components last-pin pieces)]
    (if (seq usables)
      ;; map over the usables, adding it to the bridge, removing it from the pieces
      (map 
       #(iter2 xs-atom (into bridge [%]) (remove-one % pieces))
       usables)
      ;; no usable piece? end here
      (when (and (> (count bridge) (count @xs-atom))
                 (> (strength bridge) (strength @xs-atom)))
        (do
          (println "*" (count bridge) (strength bridge) bridge)
          (reset! xs-atom bridge))))))

;; (iter2 part-a [[0 0]] pieces)
