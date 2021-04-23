(ns advent2016.day21)

(defn parse-line [s]
  (->> (re-seq #"\S+" s)
       (mapv #(try (Long. %)
                   (catch Exception ex
                     (let [x (str %)]
                       (if (= 1 (count x)) (first x) (keyword x))))))))                   

;; (parse-line "this is a 3 level dungeon")

(defn read-input []
  (->> (slurp "resources/day21.txt")
       clojure.string/split-lines
       (mapv parse-line)))

;; (first (read-input))

(defn swap-position [xs i j]
  (let [m {i (xs j), j (xs i)}]
    (mapv #(get m % (xs %)) (range (count xs)))))

;; (swap-position (into [] "abcde") 4 0)

(defn swap-letter [xs a b]
  (let [m {a b, b a}]
    (mapv #(get m % %) xs)))

;; (swap-letter (into [] "abcde") \d \b)

(defn rotate-left [s i]
  (->> (cycle s) (drop i) (take (count s)) vec))

;; (rotate-left "abcde" 1)

(defn rotate-right [s i]
  (let [n (count s)]
    (->> (cycle s) (drop (- (* 2 n) i)) (take n) vec)))

(defn index-of [xs x]
  (->> (map-indexed vector xs) (filter (comp #{x} second)) ffirst))

(defn rotate-based [xs x]
  (let [index (index-of xs x)]
    (rotate-right xs (+ 1 index (if (>= index 4) 1 0)))))

;; (rotate-based [\a \b \d \e \c] \b)
;; (rotate-based [\e \c \a \b \d] \d)

(defn reverse-positions [xs i j]
  (let [m (zipmap (range i (inc j)) (range j (dec i) -1))]
    (mapv #(xs (get m % %)) (range (count xs)))))

;; (reverse-positions [\e \d \c \b \a] 0 4)

(defn move-position [xs i j]
  (let [xs' (concat (take i xs) (drop (inc i) xs))
        xs' (concat (take j xs') [(nth xs i)] (drop j xs'))]
    (vec xs')))

;; (= (vec "abdec") (move-position (vec "bdeac") 3 0))
;; (= (vec "bdeac") (move-position (vec "bcdea") 1 4))

(defn scramble [xs command]
  (let [pre (take 2 command)]
    (condp = pre
      [:swap :position] (let [[_ _ x _ _ y] command]
                          (swap-position xs x y))

      [:swap :letter] (let [[_ _ x _ _ y] command]
                        (swap-letter xs x y))

      [:rotate :left] (let [[_ _ x _] command]
                        (rotate-left xs x))

      [:rotate :right] (let [[_ _ x _] command]
                         (rotate-right xs x))

      [:rotate :based] (let [[_ _ _ _ _ _ x] command]
                         (rotate-based xs x))

      [:reverse :positions] (let [[_ _ x _ y] command]
                              (reverse-positions xs x y))

      [:move :position] (let [[_ _ x _ _ y] command]
                          (move-position xs x y))

      xs)))

(defn part-1 [s]
  (->> (reduce scramble (vec s) (read-input))
       (apply str)))

(comment
  ;; Problem statement example
  (-> (vec "abcde")
      (swap-position 4 0)
      (swap-letter \d \b)
      (reverse-positions 0 4)
      (rotate-left 1)
      (move-position 1 4)
      (move-position 3 0)
      (rotate-based \b)
      (rotate-based \d)
      (= (vec "decab")))
  )

(defn unrotate-based [xs x]
  (let [index (- (count xs) (index-of xs x) 1)]
    (rotate-left xs (+ 1 index (if (>= index 4) 1 0)))))

(defn unscramble [xs command]
  (let [pre (take 2 command)]
    (condp = pre
      [:swap :position] (let [[_ _ x _ _ y] command]
                          (swap-position xs y x))

      [:swap :letter] (let [[_ _ x _ _ y] command]
                        (swap-letter xs x y))

      [:rotate :left] (let [[_ _ x _] command]
                        (rotate-right xs x))

      [:rotate :right] (let [[_ _ x _] command]
                         (rotate-left xs x))

      [:rotate :based] (let [[_ _ _ _ _ _ x] command]
                         (unrotate-based xs x))

      [:reverse :positions] (let [[_ _ x _ y] command]
                              (reverse-positions xs x y))

      [:move :position] (let [[_ _ x _ _ y] command]
                          (move-position xs y x))

      xs)))

(defn part-2 [s]
  (let [steps (->> (read-input) reverse vec)]
    (->> (reduce unscramble (vec s) steps)
         (apply str))))

;; (part-2 "fbgdceah")

(comment
  ;; Testing individual steps
  (= (unscramble (vec "ebcda") [:swap :position 4 :with :position 0]) (vec "abcde"))
  (= (unscramble (vec "edcba") [:swap :letter \d :with :letter \b]) (vec "ebcda"))
  (= (unscramble (vec "abcde") [:reverse :positions 0 :through 4]) (vec "edcba"))
  (= (unscramble (vec "bcdea") [:rotate :left 1 :step]) (vec "abcde"))
  (= (unscramble (vec "bdeac") [:move :position 1 :to :position 4]) (vec "bcdea"))
  (= (unscramble (vec "abdec") [:move :position 3 :to :position 0]) (vec "bdeac"))
  (= (unscramble (vec "ecabd") [:rotate :based :on :position :of :letter \b]) (vec "abdec"))
  (= (unscramble (vec "decab") [:rotate :based :on :position :of :letter \d]) (vec "ecabd"))

  ;; Testing the pipeline
  (-> (vec "decab")
      (unrotate-based \d)
      (unrotate-based \b)
      (move-position 0 3)
      (move-position 4 1)
      (rotate-right 1)
      (reverse-positions 0 4)
      (swap-letter \d \b)
      (swap-position 0 4)
      (= (vec "abcde")))

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The part-2 implementation above is buggy, so cracking the password
;; is also possible (8! combinations = 40320 iterations)

(defn crack [s target]
  (let [xs    (set s)
        steps (read-input)]
    (for [a xs
          :let [xs-a (disj xs a)]
          b xs-a
          :let [xs-b (disj xs-a b)]
          c xs-b
          :let [xs-c (disj xs-b c)]
          d xs-c
          :let [xs-d (disj xs-c d)]
          e xs-d
          :let [xs-e (disj xs-d e)]
          f xs-e
          :let [xs-f (disj xs-e f)]
          g xs-f
          :let [xs-g (disj xs-f g)]
          h xs-g
          :when (= (reduce scramble [a b c d e f g h] steps) target)
          ]
      [a b c d e f g h])))

;; (time (->> (crack "abcdefgh" (vec "fbgdceah")) first (reduce str)))
;; => "Elapsed time: 22285.833112 msecs"
