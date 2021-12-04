(ns advent2021.day02)

(declare example)

(defn read-input [s]
  (->> s clojure.string/split-lines
       (mapv (comp read-string (partial format "[%s]")))))

(defn move-submarine [[dist depth] [cmd amnt]]
  (condp = cmd
    'up   [dist (- depth amnt)]
    'down [dist (+ depth amnt)]
    [(+ dist amnt) depth]))

(defn part-1 [s]
  (->> s read-input (reduce move-submarine [0 0]) (apply *)))

;; (= [15 10] (reduce move-submarine [0 0] (read-input example)))
;; (= 150 (part-1 example))
;; (part-1 (slurp "resources/day02.txt"))

(defn move-submarine2 [[dist depth aim] [cmd amnt]]
  (condp = cmd
    'down [dist depth (+ aim amnt)]
    'up   [dist depth (- aim amnt)]
    [(+ dist amnt) (+ depth (* aim amnt)) aim]))

;; (= [15 60 10] (reduce move-submarine2 [0 0 0] (read-input example)))

(defn part-2 [s]
  (->> s read-input (reduce move-submarine2 [0 0 0]) (take 2) (apply *)))

;; (= 900 (part-2 example))
;; (part-2 (slurp "resources/day02.txt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def example "forward 5
down 5
forward 8
up 3
down 8
forward 2")
