(ns advent2016.day12
  (:require [clojure.string :refer [split-lines]]))

(defn parse-line [line]
  (let [xs (re-seq #"[\w-\d]+" line)]
    (mapv
     #(try (Long/parseLong %) (catch Exception _ (keyword %))) xs)))

(defn read-input []
  (->> (slurp "resources/day12.txt")
       split-lines
       (mapv parse-line)))

;; (read-input)

(def example-input
  [[:cpy 41 :a]
   [:inc :a]
   [:inc :a]
   [:dec :a]
   [:jnz :a 2]
   [:dec :a]])

(defn run-assembunny
  ([code] (run-assembunny code {:a 0, :b 0, :c 0, :d 0}))
  ([code regs]
   (let [progsize (count code)]
     (loop [ip 0, regs regs]
       ;; (println ip regs)
       (if (or (neg? ip) (>= ip progsize))
         [ip regs]
         (let [[opcode arg1 arg2] (code ip)]
           (condp = opcode
             :cpy (let [val (if (keyword? arg1) (get regs arg1) arg1)]
                    (recur (inc ip) (assoc regs arg2 val)))

             :inc (let [val (+ (get regs arg1) 1)]
                    (recur (inc ip) (assoc regs arg1 val)))

             :dec (let [val (- (get regs arg1) 1)]
                    (recur (inc ip) (assoc regs arg1 val)))

             #_:jnz (let [val (if (keyword? arg1) (get regs arg1) arg1)
                          ip' (if (zero? val) (+ ip 1) (+ ip arg2))]
                      (recur ip' regs)))))))))

;; (= 42 (->> example-input run-assembunny second :a))

(defn part1 []
  (->> (read-input)
       run-assembunny
       second :a))

;; (part1)

(defn part2 []
  (->> (run-assembunny (read-input) {:a 0, :b 0, :c 1, :d 0})
       second :a))

;; (part2)

