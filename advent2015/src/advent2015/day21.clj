(ns advent2015.day21)

(def weapons ;; map from :name to [cost damage armor]
  {:Dagger      [ 8 4 0]
   :Shortsword  [10 5 0]
   :Warhammer   [25 6 0]
   :Longsword   [40 7 0]
   :Greataxe    [74 8 0]})

(def armor
  {:Leather    [ 13 0 1]
   :Chainmail  [ 31 0 2]
   :Splintmail [ 53 0 3]
   :Bandedmail [ 75 0 4]
   :Platemail  [102 0 5]
   :none       [  0 0 0]}) ;; armor is optional

(def rings
  {:Damage+1  [ 25 1 0]
   :Damage+2  [ 50 2 0]
   :Damage+3  [100 3 0]
   :Defense+1 [ 20 0 1]
   :Defense+2 [ 40 0 2]
   :Defense+3 [ 80 0 3]
   :none1     [  0 0 0]
   :none2     [  0 0 0] ;; 0 to 2 rings
   })

;; Player wins if they beat the boss in the same or less rounds
(defn player-wins? [[p-hp p-dmg p-arm] [b-hp b-dmg b-arm]]
  (let [p-deals  (max 1 (- p-dmg b-arm))
        b-deals  (max 1 (- b-dmg p-arm))
        p-rounds (+ (quot b-hp p-deals)
                    (if (zero? (rem b-hp p-deals)) 0 1))
        b-rounds (+ (quot p-hp b-deals)
                    (if (zero? (rem p-hp b-deals)) 0 1))]
    (<= p-rounds b-rounds)))

;; (player-wins? [8 5 5] [12 7 2])

(defn lowest-price [[boss-hp boss-dmg boss-arm]]
  (first
   (for [price (range)
         weapon (filter #(<= (first %) price) (vals weapons))
         :let   [[w-cost w-dmg _] weapon]
         armor  (filter #(<= (first %) (- price w-cost)) (vals armor))
         :let   [[a-cost _ a-arm] armor]
         ring1  (filter #(<= (first %) (- price w-cost a-cost)) (vals rings))
         :let   [[r1-cost r1-dmg r1-arm] ring1]
         ring2  (filter #(<= (first %) (- price w-cost a-cost r1-cost)) (vals rings))
         :let   [[r2-cost r2-dmg r2-arm] ring2]
         :when  (and (not= ring1 ring2)
                     (>= price (+ w-cost a-cost r1-cost r2-cost))
                     (player-wins? [100
                                    (+ w-dmg r1-dmg r2-dmg)
                                    (+ a-arm r1-arm r2-arm)]
                                   [boss-hp boss-dmg boss-arm]))
         ]
     price
     #_[price weapon armor ring1 ring2])))

(defn read-input []
  (->> (slurp "resources/day21.txt")
       (re-seq #"\d+")
       (map read-string)))

(defn part-1 []
  (lowest-price (read-input)))

;; (part-1)

(defn highest-price [[boss-hp boss-dmg boss-arm]]
  (let [maxprice (+ (->> (vals weapons) (map first) (reduce max))
                    (->> (vals armor) (map first) (reduce max))
                    (->> (vals rings) (map first) sort reverse (take 2) (apply +)))]
    (reduce max 0
            (for [price  (range (inc maxprice))
                  weapon (filter #(<= (first %) price) (vals weapons))
                  :let   [[w-cost w-dmg _] weapon]
                  armor  (filter #(<= (first %) (- price w-cost)) (vals armor))
                  :let   [[a-cost _ a-arm] armor]
                  ring1  (filter #(<= (first %) (- price w-cost a-cost)) (vals rings))
                  :let   [[r1-cost r1-dmg r1-arm] ring1]
                  ring2  (filter #(<= (first %) (- price w-cost a-cost r1-cost)) (vals rings))
                  :let   [[r2-cost r2-dmg r2-arm] ring2]
                  :when  (and (not= ring1 ring2)
                              (= price (+ w-cost a-cost r1-cost r2-cost)) ;; SPEND EXACTLY ALL THE MONEY
                              (false? (player-wins? [100 ;; DO NOT WIN
                                                     (+ w-dmg r1-dmg r2-dmg)
                                                     (+ a-arm r1-arm r2-arm)]
                                                    [boss-hp boss-dmg boss-arm])))
                  ]
              price))))

(defn part-2 []
  (highest-price (read-input)))

;; (part-2)

