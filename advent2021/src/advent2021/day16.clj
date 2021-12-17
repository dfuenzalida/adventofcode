(ns advent2021.day16)

(def from-hex
  (zipmap "0123456789ABCDEF" (range 16)))

(defn create-buffer [s]
  {:buffer 0, :num-bits 0, :offset 0, :source (seq s)})

(defn read-to-buffer [buf n] ;; update the buffer to have n bits available
  (let [{:keys [buffer num-bits offset source]} buf]
    (if (< num-bits n)
      (let [newbits (->> source first from-hex)
            buffer' (bit-or (bit-shift-left buffer 4) newbits)
            buf'    {:buffer buffer' :num-bits (+ 4 num-bits) :source (rest source)}]
        (recur (merge buf buf') n))
      buf)))

(defn read-from-buffer [buf n] ;; => [bits, buffer']
  (let [buf (read-to-buffer buf n)
        {:keys [buffer num-bits offset]} buf
        output (bit-shift-right buffer (- num-bits n))
        mask   (dec (bit-shift-left 1 (- num-bits n)))
        buf'   {:buffer (bit-and buffer mask) :num-bits (- num-bits n) :offset (+ offset n)}]
    [output (merge buf buf')]))

(defn read-literal [buf]
  (loop [lit 0, buf buf]
    (let [[start buf] (read-from-buffer buf 1)
          [group buf] (read-from-buffer buf 4)
          lit' (bit-or (bit-shift-left lit 4) group)]
      (if (zero? start)
        [lit' buf]
        (recur lit' buf)))))

(declare read-packet)

(defn read-operator [buf]
  (let [[ltype buf] (read-from-buffer buf 1)]
    (if (zero? ltype)
      (let [[plength buf] (read-from-buffer buf 15)
            offset-end    (+ (:offset buf) plength)]
        (loop [res [], buf buf] ;; read a seq of packets until the offset is plength bits ahead
          (if (= (:offset buf) offset-end)
            [res buf]
            (let [[p b] (read-packet buf)]
              (recur (conj res p) b)))))
      (let [[pcount buf] (read-from-buffer buf 11)]
        (loop [res [], buf buf, n pcount]
          (if (zero? n)
            [res buf]
            (let [[p b] (read-packet buf)]
              (recur (conj res p) b (dec n)))))))))

(defn read-packet [buf] ;; => [packet buf']
  (let [[pversion buf] (read-from-buffer buf 3)
        [ptype buf] (read-from-buffer buf 3)
        [pval buf]  (condp = ptype
                      4 (read-literal buf)
                      (read-operator buf))]
    [{:version pversion :type ptype :val pval} buf]))

;; (= 2021 (-> (create-buffer "D2FE28") read-packet second :val))

(defn versions [packet]
  (->> (tree-seq (comp vector? :val) :val packet)
       (map :version)
       (reduce + 0)))

(defn part-1 [s]
  (-> s create-buffer read-packet first versions))

;; (= 16 (part-1 "8A004A801A8002F478"))
;; (= 12 (part-1 "620080001611562C8802118E34"))
;; (= 23 (part-1 "C0015000016115A2E0802F182340"))
;; (= 31 (part-1 "A0016C880162017C3686B18A3D4780"))
;; (-> "resources/day16.txt" slurp part-1)

(defn eval-packet [p]
  (let [{:keys [type val]} p]
    (condp = type
      0 (reduce + (map eval-packet val))
      1 (reduce * (map eval-packet val))
      2 (reduce min (map eval-packet val))
      3 (reduce max (map eval-packet val))
      4 val
      5 (if (> (eval-packet (first val)) (eval-packet (second val))) 1 0)
      6 (if (< (eval-packet (first val)) (eval-packet (second val))) 1 0)
      7 (if (= (eval-packet (first val)) (eval-packet (second val))) 1 0)
      -1)))

(defn part-2 [s]
  (-> s create-buffer read-packet first eval-packet))

;; (= 3 (part-2 "C200B40A82"))
;; (= 54 (part-2 "04005AC33890"))
;; (= 7 (part-2 "880086C3E88112"))
;; (= 9 (part-2 "CE00C43D881120"))
;; (= 1 (part-2 "D8005AC2A8F0"))
;; (= 0 (part-2 "F600BC2D8F"))
;; (= 0 (part-2 "9C005AC2F8F0"))
;; (= 1 (part-2 "9C0141080250320F1802104A08"))
;; (-> "resources/day16.txt" slurp part-2)

