(ns advent.day22
  (:gen-class))

(def left  {:up :left, :left :down, :down :right, :right :up})
(def right {:up :right, :right :down, :down :left, :left :up})
(def speed {:up [0 -1] :down [0 1] :left [-1 0] :right [1 0]})

(defn replace-at [xs x y c]
  (if (= \. c)
    (disj xs [x y])
    (conj xs [x y])))

(defn char-at [xs x y]
  (if (xs [x y]) \# \.))

(defn iter [xs x y dir n n-infec]
  (if (zero? n)
    n-infec
    (let [node (char-at xs x y)]
      (if (= \. node)
        (let [dir2    (left dir)
              [sx sy] (speed dir2)]
          (recur (replace-at xs x y \#) ;; infect
                 (+ x sx) (+ y sy) dir2
                 (dec n) (inc n-infec)))
        (let [dir2    (right dir)
              [sx sy] (speed dir2)]
          (recur (replace-at xs x y \.) ;; clean
                 (+ x sx) (+ y sy) dir2
                 (dec n) n-infec))))))

;; example
;; (iter #{[5 3] [3 4]} 4 4 :up 10000 0) => 5587

(defn read-input []
  (-> (slurp "src/input22.txt")
      (clojure.string/split #"\n")))

(defn load-map []
  (loop [cell-map #{}
         row      0
         lines    (read-input)]
    (if (seq lines)
      (let [line (first lines)
            idxs (->> (map-indexed vector line)
                      (filter (comp #{\#} second))
                      (map first))]
        (recur (into cell-map (map vector idxs (repeat row)))
               (inc row)
               (rest lines)))
      cell-map)))

;; part 1
;; (iter (load-map) 12 12 :up 10000 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn replace-at2 [m x y c]
  (if (= \. c)
    (dissoc m [x y]) ;; don't store blanks
    (assoc m [x y] c)))

(defn char-at2 [m x y]
  (get m [x y] \.))

(defn iter2 [xs x y dir n n-infec]
  (if (zero? n)
    n-infec
    (let [node (char-at2 xs x y)]
      (condp = node
        \. (let [dir2    (left dir)
                 [sx sy] (speed dir2)]
             (recur (replace-at2 xs x y \W) ;; weaken
                    (+ x sx) (+ y sy) dir2
                    (dec n) n-infec))

        \W (let [[sx sy] (speed dir)]
             (recur (replace-at2 xs x y \#) ;; infect
                    (+ x sx) (+ y sy) dir
                    (dec n) (inc n-infec)))

        \# (let [dir2    (right dir)
                 [sx sy] (speed dir2)]
             (recur (replace-at2 xs x y \F) ;; flagged
                    (+ x sx) (+ y sy) dir2
                    (dec n) n-infec))

        \F (let [dir2    (right (right dir)) ;; turn 180 deg
                 [sx sy] (speed dir2)]
             (recur (replace-at2 xs x y \.) ;; cleaned
                    (+ x sx) (+ y sy) dir2
                    (dec n) n-infec))))))

;; example
;; (iter2 {[5 3] \#, [3 4] \#} 4 4 :up 100 0) => 26

;; part 2
;; (iter2 (zipmap (load-map) (repeat \#)) 12 12 :up 10000000 0)
