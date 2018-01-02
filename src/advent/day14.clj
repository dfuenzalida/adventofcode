(ns advent.day14
  (require [advent.day10 :as day10])
  (:gen-class))

(def hex-to-bin
  {\0 "0000" \1 "0001" \2 "0010" \3 "0011" \4 "0100" \5 "0101" \6 "0110"
   \7 "0111" \8 "1000" \9 "1001" \a "1010" \b "1011" \c "1100" \d "1101"
   \e "1110" \f "1111"})

(defn hex-to-binary [s]
  (apply str (map hex-to-bin s)))

(defn count-used-squares [input]
  (->> (map #(str input "-" %) (range 128))
       (map day10/knot-hash)
       (map hex-to-binary)
       (apply str)
       (filter #{\1})
       count))

;; part 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (count-used-squares "flqrgnkx")

(defn render-map [input]
  (->> (map #(str input "-" %) (range 128))
       (map day10/knot-hash)
       (map hex-to-binary)))

(def example-map ["10100"
                  "11100"
                  "10101"])

(defn map-to-set [xs]
  (into #{}
        (for [x (range (count (first xs)))
              y (range (count xs))
              :when (= \1 (.charAt (get xs y) x))]
          [x y])))

(defn remove-neighbors [map-atom [x y]]
  (when (@map-atom [x y])
    (swap! map-atom disj [x y])
    (doall
     (for [[dx dy] [[1 0] [-1 0] [0 1] [0 -1]]
           :when (and (>= (+ x dx) 0)
                      (>= (+ y dy) 0)
                      (@map-atom [(+ x dx) (+ y dy)]))]
       (remove-neighbors map-atom [(+ x dx) (+ y dy)])))))

(defn count-islands [bit-map]
  (let [map-atom (atom (map-to-set bit-map))]
    (for [x (range (count (first bit-map)))
          y (range (count bit-map))
          :when (remove-neighbors map-atom [x y])]
      [x y])))

;; (count (count-islands example-map)) => 2
;; (count (count-islands (into [] (render-map "flqrgnkx")))) => 1242

;; part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (count (count-islands (into [] (render-map "vbqugkhl"))))
