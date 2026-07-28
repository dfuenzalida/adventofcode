(ns advent2025.day08
  (:require [clojure.string :as s]
            [clojure.edn :as edn]))

(declare example)

(defn read-input [s]
  (->> (format "[%s]" s)
       edn/read-string
       (partition 3)
       (mapv vec)))

(defn distance2 [[x1 y1 z1] [x2 y2 z2]]
  (+ (Math/pow (- x2 x1) 2) (Math/pow (- y2 y1) 2) (Math/pow (- z2 z1) 2)))

(defn pairs-by-distance [points]
  (let [pairs (for [i (range (count points))
                    j (range (inc i) (count points))
                    :let [x (nth points i) y (nth points j)]]
                [x y])]
    (sort
     (fn [[p1 p2] [p3 p4]]
       (compare (distance2 p1 p2) (distance2 p3 p4)))
     pairs)))

;; (= (-> example read-input pairs-by-distance (nth 0)) [[162,817,812] [425,690,689]])
;; (= (-> example read-input pairs-by-distance (nth 1)) [[162,817,812] [431,825,988]])
;; (= (-> example read-input pairs-by-distance (nth 2)) [[906,360,560] [805, 96,715]])
;; (= (-> example read-input pairs-by-distance (nth 3)) [[431,825,988] [425,690,689]])

;; If the set #{box1 box2} has an element in common with any existing sets, merge them,
;; otherwise the set is added to the box-sets as a set of 2 elements

(defn reduce-boxes [box-sets [box1 box2]]
  (if-let [matching-sets  (filter #(some % [box1 box2]) box-sets)]
    (let [box-sets-without-matches (reduce disj box-sets matching-sets)]
      (->> (reduce into #{box1 box2} matching-sets)
           (conj box-sets-without-matches)))
    (conj box-sets #{box1 box2})))

#_(= (->> example read-input pairs-by-distance (take 2) (reduce reduce-boxes #{}))
   #{#{[162,817,812] [425,690,689] [431,825,988]}})

#_(= (->> example read-input pairs-by-distance (take 3) (reduce reduce-boxes #{}))
   #{#{[162,817,812] [425,690,689] [431,825,988]} #{[906,360,560] [805,96,715]}})

;; The 4th pair doesn't change the result, boxes already in the same circuit
#_(= (->> example read-input pairs-by-distance (take 4) (reduce reduce-boxes #{}))
   #{#{[162,817,812] [425,690,689] [431,825,988]} #{[906,360,560] [805,96,715]}})

#_(->> example read-input pairs-by-distance (take 10) (reduce reduce-boxes #{}) (map count) sort reverse (take 3))

(defn part-1 [s n]
  (->> (read-input s)
       pairs-by-distance
       (take n)
       (reduce reduce-boxes #{})
       (map count)
       (sort (fn [a b] (compare b a))) ;; sort by descending order
       (take 3)
       (reduce *)))

;; (= 40 (part-1 example 10))
;; (-> (slurp "resources/day08.txt") (part-1 1000))

(defn reduce-boxes2 [total box-sets [box1 box2]]
  (if-let [matching-sets  (filter #(some % [box1 box2]) box-sets)]
    (let [box-sets-without-matches (reduce disj box-sets matching-sets)
          box-sets' (->> (reduce into #{box1 box2} matching-sets)
                         (conj box-sets-without-matches))]
      (if (and (= 1 (count box-sets')) (= total (count (first box-sets'))))
        (reduced [box1 box2])
        box-sets'))
    (conj box-sets #{box1 box2})))

#_(->> example read-input pairs-by-distance (reduce reduce-boxes2 #{}))

(defn part-2 [s]
  (let [input (read-input s)]
    (->> (pairs-by-distance input)
         (reduce (partial reduce-boxes2 (count input)) #{})
         (map first)
         (reduce *))))

;; (= (part-2 example) 25272)
;; (part-2 (slurp "resources/day08.txt"))

(comment
  (disj #{#{:a :e} #{:b :c}} #{:e :a})
  (into #{:a :e} [:i :o :u])

  (let [[box1 box2] #{(425 690 689) (162 817 812)}]
    (filter #{box1 box2} #{#{(431 825 988) (162 817 812)}}))
  
  (let [pairs (->> example read-input pairs-by-distance (take 10))]
    (->> (reduce reduce-boxes #{} pairs)
         (map count)
         )
    )

  ;; (->> example read-input pairs-by-distance (map set) set count)
  )

(def example
  "162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689")
