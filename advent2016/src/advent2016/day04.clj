(ns advent2016.day04
  (:require [clojure.string :refer [split-lines]]))

(defn freq-then-alpha [[k1 v1] [k2 v2]] ;; compare by frequency then alphabetically
  (let [cv2v1 (compare v2 v1)]
    (if (zero? cv2v1)
      (compare k1 k2)
      cv2v1)))

(defn checksum [name]
  (->> name
       (remove #{\-})
       frequencies
       (sort-by identity freq-then-alpha)
       (map first)
       (take 5)
       (apply str)))

(defn room-parts [s]
  (let [[_ name roomid check] (re-find #"([a-z-]+)-(\d+)\[(\w+)\]" s)]
    [name (read-string roomid) check]))

(defn valid-room? [[name _ check]]
  (= (checksum name) check))

(defn read-input []
  (slurp "resources/day04.txt"))

;; (= true (->> "aaaaa-bbb-z-y-x-123[abxyz]" room-parts valid-room?))
;; (= true (->> "a-b-c-d-e-f-g-h-987[abcde]" room-parts valid-room?))
;; (= true (->> "not-a-real-room-404[oarel]" room-parts valid-room?))
;; (= false (->> "totally-real-room-200[decoy]" room-parts valid-room?))

(defn part-1 []
  (->> (read-input)
       split-lines
       (map room-parts)
       (filter valid-room?)
       (map second)
       (reduce +)))

;; (part-1)

(defn rotate-char [n c]
  (if (= \space c)
    c
    (-> c int (- (int \a)) (+ n) (mod 26) (+ (int \a)) char)))

(defn decrypt [[name turns _]]
  (->> (replace {\- \space} name)
       (map (partial rotate-char turns))
       (apply str)))

(defn storage-room? [[name turns _]]
  (= "northpole object storage" (decrypt [name turns _])))

;; (= "very encrypted name" (->> "qzmt-zixmtkozy-ivhz-343[xyz]" room-parts decrypt))

(defn part-2 []
  (->> (read-input)
       split-lines
       (map room-parts)
       (filter valid-room?)
       (filter storage-room?)
       first
       second))

;; (part-2)

