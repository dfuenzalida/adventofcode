(ns advent2022.day06)

(defn find-start-of-packet [s n]
  (->> (partition n 1 s)
       (map (partial into #{}))
       (take-while #(> n (count %)))
       count
       (+ n)))

(defn part-1 [s]
  (find-start-of-packet s 4))

;; (= [7 5 6 10 11] (map part-1 examples))
;; (part-1 (slurp "resources/day06.txt"))

(defn part-2 [s]
  (find-start-of-packet s 14))

;; (= [19 23 23 29 26] (map part-2 examples))
;; (part-2 (slurp "resources/day06.txt"))

(comment

  (->> (nth examples 3)
       (partition 4 1)
       (map (partial into #{}))
       (take-while #(> 4 (count %)))
       count
       (+ 4))
  )

(def examples
  ["mjqjpqmgbljsphdztnvjfqwrcgsmlb"
   "bvwbjplbgvbhsrlpgdmjqwftvncz"
   "nppdvjthqldpwncqszvftbrmjlhg"
   "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
   "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"])
