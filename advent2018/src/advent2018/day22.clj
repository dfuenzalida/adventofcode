(ns advent2017.day22)

(declare erosion-level)
(declare geological-index-memo)
(declare erosion-level-memo)

(defn geological-index [[^long x ^long y] [^long xtarget ^long ytarget] ^long depth]
  (cond
    (= [0 0] [x y]) 0             ;; [0 0] has a geologic index of 0
    (= [x y] [xtarget ytarget]) 0 ;; the target has a geologic index of 0
    (= y 0)  (* x 16807)
    (= x 0)  (* y 48271)
    :else    (* (erosion-level-memo [(dec x) y] [xtarget ytarget] depth)
                (erosion-level-memo [x (dec y)] [xtarget ytarget] depth))))

(defn erosion-level [coords target-coords ^long depth]
  (mod (+ (geological-index-memo coords target-coords depth) depth)
       20183))

(def geological-index-memo (memoize geological-index))
(def erosion-level-memo (memoize erosion-level))
(def region-type [:rocky :wet :narrow])

(defn region-index [erosion-lvl]
  (mod erosion-lvl 3))

(defn risk-level [^long xtarget ^long ytarget ^long depth]
  (reduce + (for [x (range (inc xtarget))
                  y (range (inc ytarget))]
              (region-index (erosion-level-memo [x y] [xtarget ytarget] depth)))))

;; (time (risk-level 10 10 510))

(defn read-input []
  (->> (slurp "resources/day22.txt") (re-seq #"\d+") (map read-string)))

(defn part1 []
  (let [[depth xtarget ytarget] (read-input)]
    (risk-level xtarget ytarget depth)))

;; (time (part1))
