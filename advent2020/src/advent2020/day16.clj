(ns advent2020.day16
  (:require [clojure.string :refer [blank? split-lines starts-with?]]))

(declare example)

;; (->> example split-lines (partition-by blank?) (remove (comp empty? first)))

(defn parse-range [s]
  (let [[_ name ranges] (first (re-seq #"(.+): (.+)" s))
        ranges (->> (re-seq #"(\d+)-(\d+)" ranges) (map rest) (mapv (partial mapv read-string)))]
    [name ranges]))

;; (parse-range "class: 1-3 or 5-7")

(defn parse-ticket [s]
  (->> (str \[ s \]) read-string))

;; (parse-ticket "7,1,14")

(defn parse-input [s]
  (let [blocks  (->> s split-lines (partition-by blank?) (remove (comp empty? first)))
        [ranges [_ & myticket] [_ & tickets]] blocks
        ranges  (->> (map parse-range ranges) (reduce merge {}))]
    [ranges (parse-ticket (first myticket)) (mapv parse-ticket tickets)]))

(defn not-in-any-range? [xs y] ;; y is not in any range [[y1 y2] ...]
  (->> (map (fn [[a b]] (<= a y b)) xs)
       (every? false?)))

(defn part-1 [input]
  (let [[ranges _ tickets] (parse-input input)
        ranges   (reduce concat (vals ranges))
        tickvals (reduce concat tickets)
        invalid  (filter (partial not-in-any-range? ranges) tickvals)]
    (reduce + invalid)))

;; (= 71 (part-1 example))
;; (part-1 (slurp "resources/day16.txt"))

(defn within-ranges? [xs y]
  (->> (map (fn [[a b]] (<= a y b)) xs)
       (some true?)))

(defn cycle1 [items] ;; [a b c] => [b c a]
  (into (subvec items 1) [(first items)]))

;; Builds a map from an attribute name (eg. "seat") to a column `i` such that
;; the i-th element all tickets is within the valid ranges for the attribute

(defn build-index-map [ranges tickets]
  (loop [ranges (vec ranges), indexes (range (count ranges)), m {}]
    (if (empty? ranges)
      m
      (let [[rangek rangevs] (first ranges)
            matchidxs (for [n indexes
                            :let [nths (map #(nth % n) tickets)
                                  nthws (map (partial within-ranges? rangevs) nths)]
                            :when (every? true? nthws)]
                        n)]
        (if (= 1 (count matchidxs))
          (let [matchidx (first matchidxs)]
            (recur (subvec ranges 1) (remove #{matchidx} indexes) (assoc m rangek matchidx)))
          (recur (cycle1 ranges) indexes m)))))) ;; no unique match in ranges, cycle them

(defn invalid-ticket? [ranges ts] ;; one of the numbers in the ticket is not in any range
  (some (partial not-in-any-range? ranges) ts))

(defn part-2 []
  (let [[ranges mytix tix] (parse-input (slurp "resources/day16.txt"))
        tickets (remove (partial invalid-ticket? (mapcat second ranges)) tix)
        index-map (build-index-map ranges (into tickets [mytix]))
        departures (->> (filter (comp #(starts-with? % "departure") first) index-map)
                        (map second))
        depvalues (map #(nth mytix %) departures)]
    (reduce * depvalues)))

;; (part-2)

(comment

  (let [[ranges mytix tix] (parse-input example2)]     
    (build-index-map ranges (into tix [mytix])))

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def example "class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12")

(def example2 "class: 0-1 or 4-19
row: 0-5 or 8-19
seat: 0-13 or 16-19

your ticket:
11,12,13

nearby tickets:
3,9,18
15,1,5
5,14,9")
