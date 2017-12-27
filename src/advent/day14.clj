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

;; (map println (render-map "flqrgnkx"))

(def example-map ["110101"
                  "010101"
                  "111010"]) ;; 4 islands

(defn map-to-set [xs]
  (into #{}
        (for [x (range (count (first xs)))
              y (range (count xs))
              :when (= \1 (.charAt (get xs y) x))]
          [x y])))
; (map-to-set example-map)

(defn contiguous-pts [point-set cont-set x y]
  ;; (println cont-set [x y])
  (if (point-set [x y])
    (let [cont-set' (conj cont-set [x y])
          neighbors (for [dx [-1 0 1]
                          dy [-1 0 1]
                          :when (and (not= 0 dx dy)
                                     (zero? (* dx dy))
                                     (point-set [(+ x dx) (+ y dy)])
                                     (nil? (cont-set' [(+ x dx) (+ y dy)])))]
                      (contiguous-pts point-set cont-set' (+ x dx) (+ y dy))
                      )]
      (reduce into cont-set' neighbors))
    #{}))

;; ((map-to-set example-map) [4 2])
;; (contiguous-pts (map-to-set example-map) #{} 1 1)
