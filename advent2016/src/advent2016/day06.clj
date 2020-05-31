(ns advent2016.day06)

(def example (mapv vec ["eedadn"
                        "drvtee"
                        "eandsr"
                        "raavrd"
                        "atevrs"
                        "tsrnev"
                        "sdttsa"
                        "rasrtv"
                        "nssdts"
                        "ntnada"
                        "svetve"
                        "tesnvt"
                        "vntsnd"
                        "vrdear"
                        "dvrsen"
                        "enarar"]))

(defn sel-freq [lines sel-func index]
  (->> (map #(% index) lines)
       frequencies
       (sort-by second)
       sel-func
       first))

(defn part-1 [input]
  (->> (map (partial sel-freq input last) (range (count (first input))))
       (apply str)))

;; (= "easter" (part-1 example))

(defn read-input []
  (->> (slurp "resources/day06.txt")
       clojure.string/split-lines
       (mapv vec)))

;; (part-1 (read-input))

(defn part-2 [input]
  (->> (map (partial sel-freq input first) (range (count (first input))))
       (apply str)))

;; (part-2 (read-input))

