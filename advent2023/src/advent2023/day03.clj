(ns advent2023.day03
  (:require [clojure.string :as s]
            [clojure.edn :as edn]))

(declare example)

;; IDEA: capture locations in two maps: one for numbers {"467" [0 0], "114" [5 0], ... }
;; another for signs: {"*" [3 1], ... } and use them for adjacency check

(defn find-occurrences-line [re s] ;; -> [["467" 0], ["114" 5] ... ]
  (let [matches    (mapcat rest (re-seq re s))
        [pairs _i] (reduce (fn [[occs i] match]
                             (let [i (s/index-of s match i)]
                               [(conj occs [match i]) (+ i (count match))]))
                           [[] 0]
                           matches)]
    pairs))

;; (find-occurrences-line #"(\d+)" "467..114..")
;; (find-occurrences-line #"([^\d\.])" "..$..+.58.")

(defn find-occurrences [re ss]
  (loop [ss     ss
         result {}
         j      0]
    (if-let [s (first ss)]
      (let [line-occs (find-occurrences-line re s)
            pairs     (map (fn [[s i]] [[i j] s]) line-occs)]
        (recur (rest ss) (into result pairs) (inc j)))
      result)))

;; (->> example s/split-lines (find-occurrences #"(\d+)"))
;; (->> example s/split-lines (find-occurrences #"([^\d\.])"))

(defn adjacent? [[[i1 j1] s1] [[i2 j2] s2]]
  (and
   (<= (Math/abs (- j1 j2)) 1)
   (some true?
         (for [x1 (range i1 (+ i1 (count s1)))
               x2 (range i2 (+ i2 (count s2)))]
           (<= (Math/abs (- x1 x2)) 1)))))

;; (adjacent? [[0 0] "467"] [[3 1] "*"])
;; (adjacent? [[5 0] "114"] [[3 1] "*"])

;; Let S1 be the 1-character string:
;; Are j1-j2 within 1 unit of distance?
;; Is i1 between (i2-1) and (i2+len(s2)+1), inclusive?

(defn adjacent-fast? [[[i1 j1] s1] [[i2 j2] s2]]
  (and (<= (Math/abs (- j1 j2)) 1)
       (<= (dec i2) i1 (+ i2 (count s2)))))

;; (adjacent-fast? [[3 1] "*"] [[0 0] "467"])
;; (adjacent-fast? [[3 1] "*"] [[5 0] "114"])

(defn adjacent-words [syms nums]
  (filter (fn [num]
            (some true?
                    (map #(adjacent-fast? % num) syms)))
          nums))

(defn part-1 [s]
  (->> (let [lines (s/split-lines s)
             syms  (find-occurrences #"([^\d\.])" lines)
             nums  (find-occurrences #"(\d+)" lines)]
         (adjacent-words syms nums))
       (map second)
       (map edn/read-string)
       (reduce +)))

;; (= 4361 (part-1 example))
;; (->> "resources/day03.txt" slurp part-1)

(defn part-2 [s]
  (let [lines (s/split-lines s)
        nums  (find-occurrences #"(\d+)" lines)
        gears (->> (find-occurrences #"([^\d\.])" lines)
                   (filter (fn [[_coords c]] (= c "*"))))]

    (->>
     ;; groups of numbers adjacent to some gear
     (for [gear gears]
       (for [num nums
             :when (adjacent-fast? gear num)]
         num))
     (filter #(= 2 (count %))) ;; having exactly 2 adjacent numbers
     (map (partial map (comp edn/read-string second))) ;; drop coords, parse to actual number
     (map (partial reduce *))
     (reduce +))))

;; (= 467835 (part-2 example))
;; (->> "resources/day03.txt" slurp part-2)

(comment

  (->> example s/split-lines)
  
  (->> (s/replace example #"\." " ")
       s/split-lines
       (map #(format "[%s]" %))
       #_(map edn/read-string)
       )
  
  (map
   (comp edn/read-string second)
   (re-seq #"(\d+)" "467..114.."))

  (mapcat rest (re-seq #"(\d+)" "467..114.."))
  
  (mapcat rest (re-seq #"([^\d\.])" "..$..+.58."))
  
  (s/index-of "467..114" "114" 3)

  )

(def example
  "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..")
