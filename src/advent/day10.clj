(ns advent.day10
  (:gen-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; part 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn update-vec [v v2 offset i]
  (if (seq v2)
    (recur (assoc-in v [(mod (+ i offset) (count v))] (first v2))
           (rest v2) offset (inc i))
    v))

(defn iter [elems lengths pos skip]
  (if (seq lengths)
    (let [fstlen (first lengths)
          l      (count elems)
          revd   (->> elems cycle (drop pos) (take fstlen) reverse vec)
          uptd   (update-vec elems revd pos 0)]
      (recur uptd
             (rest lengths)
             (mod (+ pos fstlen skip) l)
             (inc skip)))
    elems))

;; (iter [0 1 2 3 4] [3 4 1 5] 0 0) ;; => [3 4 2 1 0]

;; (def nums
;;   (iter (into [] (range 256))
;;         [97,167,54,178,2,11,209,174,119,248,254,0,255,1,64,190]
;;         0 0))
;; (* (first nums) (second nums))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; part 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn str-to-len [s]
  (into (mapv int s)
          [17, 31, 73, 47, 23]))

(defn iter2 [elems lengths pos skip]
  (if (seq lengths)
    (let [fstlen (first lengths)
          l      (count elems)
          revd   (->> elems cycle (drop pos) (take fstlen) reverse vec)
          uptd   (update-vec elems revd pos 0)]
      (recur uptd
             (rest lengths)
             (mod (+ pos fstlen skip) l)
             (inc skip)))
    [elems pos skip]))

(defn run-rounds [elems lengths]
  (loop [e elems
         p 0
         s 0
         rounds 64]
    (if (pos? rounds)
      (let [[e2 p2 s2] (iter2 e lengths p s)]
        (recur e2 p2 s2 (dec rounds)))
      e)))

(defn dense-hash [xs]
  (let [seqs (partition 16 xs)]
    (map #(reduce bit-xor %) seqs)))

(defn to-hex [n]
  (Integer/toString (int n) 16))

(defn knot-hash [s]
  (let [lengths (str-to-len s)
        elems   (run-rounds (into [] (range 256)) lengths)
        densed  (dense-hash elems)]
    (apply str (map to-hex densed))))

;; (knot-hash "")
;; (knot-hash "97,167,54,178,2,11,209,174,119,248,254,0,255,1,64,190")

