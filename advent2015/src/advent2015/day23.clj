(ns advent2015.day23
  (:require [clojure.string :refer [replace split split-lines]]))

(def example
  ["inc a" "jio a, +2" "tpl a" "inc a"])

(defn parse-line [s]
  (let [parts (-> (replace s #"," "") (split #"\s"))]
    (map #(or (keyword (read-string %)) (read-string %)) parts)))

(defn execute
  ([code]
   (execute code {:a 0 :b 0}))
  ([code regs-map]
   (let [length (count code)]
     (loop [ip 0 regs regs-map]
       (if (or (neg? ip) (>= ip length))
         regs
         (let [[a b c] (code ip)]
           (condp = a
             :hlf (recur (inc ip) (update-in regs [b] #(quot % 2)))
             :tpl (recur (inc ip) (update-in regs [b] (partial * 3)))
             :inc (recur (inc ip) (update-in regs [b] inc))
             :jmp (recur (+ ip b) regs)
             :jie (if (even? (regs b))
                    (recur (+ ip c) regs)
                    (recur (inc ip) regs))
             :jio (if (= 1 (regs b))
                    (recur (+ ip c) regs)
                    (recur (inc ip) regs)))))))))

(defn read-input []
  (->> (slurp "resources/day23.txt") split-lines (mapv parse-line)))

(defn part-1 []
  (-> (read-input) execute :b))

;; (part-1)

(defn part-2 []
  (-> (read-input) (execute {:a 1 :b 0}) :b))

;; (part-2)

