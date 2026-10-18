(ns advent2023.day02
  (:require [clojure.string :as s]
            [clojure.edn :as edn]))

(declare example)

;; "1 red, 2 green, 6 blue" -> {:red 1, :green 2, :blue 6}
(defn cubeset->map [cs]
  (->> (re-seq #"(\d+) (\w+)" cs)
       (mapcat rest) ;; keep only the matching components
       (partition 2)
       (map (fn [[v k]] [(keyword k) (edn/read-string v)]))
       (into {})))

;; (cubeset->map "1 red, 2 green, 6 blue")

(defn read-game [line]
  (->> (s/split line #"; ")
       (map cubeset->map)))

;;(= (->> example s/split-lines first read-game) [{:blue 3, :red 4} {:red 1, :green 2, :blue 6} {:green 2}])

(defn game-possible? [config cubesetmaps]
  (->> (map (fn [csm] (merge-with - config csm)) cubesetmaps)
       (map (fn [m] (->> m vals (every? #(<= 0 %)))))
       (every? true?)))

;; (game-possible? {:red 12, :green 13, :blue 14} [{:blue 3, :red 4} {:red 1, :green 2, :blue 6} {:green 2}])

(defn part-1 [config s]
  (->> s s/split-lines
       (map #(re-matches #"Game (.*): (.*)" %))
       (map rest)
       (filter (fn [[n g]] (game-possible? config (read-game g))))
       (map (comp edn/read-string first))
       (reduce +)))

;; (= 8 (part-1 {:red 12, :green 13, :blue 14} example))
;; (->> "resources/day02.txt" slurp (part-1 {:red 12, :green 13, :blue 14}))

(defn min-possible [cubesetmaps]
  (reduce (partial merge-with max) cubesetmaps))

;; (= {:red 4, :green 2, :blue 6} (min-possible [{:blue 3, :red 4} {:red 1, :green 2, :blue 6} {:green 2}]))

(defn cubeset-power [cubeset]
  (reduce * (vals cubeset)))

(defn part-2 [s]
  (->> s s/split-lines
       (map #(re-matches #"Game (.*): (.*)" %))
       (map rest)
       (map (fn [[_n g]] (min-possible (read-game g))))
       (map cubeset-power)
       (reduce +)))

;; (= 2286 (part-2 example))
;; (->> "resources/day02.txt" slurp part-2)

(comment

  (let [csms (->> example s/split-lines
                  (map #(re-matches #".*: (.*)" %))
                  (map second)
                  (map read-game))
          ]
      #_csms
      (->> csms (map (partial game-possible? {:red 12, :green 13, :blue 14}))))

  (let [subsets (->> example s/split-lines first
                     (re-matches #".*: (.*)")
                     second)
        subsets (->> (s/split subsets #"; ")
                     (map #(->> (re-seq #"(\d+) (\w+)" %) (mapcat rest) #_(map edn/read-string))))
        ]
    subsets
    )

  (re-seq #"(\d+) (\w+)" "1 red, 2 green, 6 blue")
  )


(def example
  "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")
