(ns advent2015.day07
  (:require [clojure.string :refer [lower-case split split-lines]]))

(def example
  ["123 -> x"
   "456 -> y"
   "x AND y -> d"
   "x OR y -> e"
   "x LSHIFT 2 -> f"
   "y RSHIFT 2 -> g"
   "NOT x -> h"
   "NOT y -> i"
   "i -> z"])

(defn parse-line [s]
  (->> (split s #"\s+")
       (map read-string)
       (remove #{'->})
       ((juxt last butlast))))

(defn read-input [lines]
  (->> (map parse-line lines)
       (reduce merge {})))

(declare eval-wire-m)

(defn eval-wire [m x]
  (let [[a b c] (get m x)]
    ;; (println [x a b c]) ;; debug
    (bit-and
     0xffff
     (cond
       ;; numbers
       (number? x) x
       (and (number? a) (nil? b) (nil? c)) a

       ;; when there's only a symbol, look it up recusively
       (and (symbol? a) (nil? b) (nil? c) (= (name a) (lower-case (name a)))) (eval-wire-m m a)
       
       (= 'NOT a) (bit-not (eval-wire-m m b))
       (= 'AND b) (bit-and (eval-wire-m m a) (eval-wire-m m c))
       (= 'OR b) (bit-or (eval-wire-m m a) (eval-wire-m m c))
       (= 'LSHIFT b) (bit-shift-left (eval-wire-m m a) (eval-wire-m m c))
       (= 'RSHIFT b) (bit-shift-right (eval-wire-m m a) (eval-wire-m m c))
       :else (throw (Exception. (str "unknown arg" {:x x})))))))

(def eval-wire-m
  (memoize eval-wire))

(defn part-1 []
  (-> (slurp "resources/day07.txt")
      clojure.string/split-lines
      read-input
      (eval-wire-m 'a)))

;; (time (part-1)) ;; ~25 msecs

(defn part-2 []
  (let [signal-a (part-1)]
    (-> (slurp "resources/day07.txt")
        clojure.string/split-lines
        read-input
        (assoc 'b [signal-a nil nil])
        (eval-wire-m 'a))))

;; (time (part-2)) ;; ~75 msecs

(comment

  (do
    (println)
    (-> (read-input example)
        (eval-wire-m 'g)))
  
  )
