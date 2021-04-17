(ns advent2016.day17)

(defonce digester
  (java.security.MessageDigest/getInstance "MD5"))

(defn md5 [s]
  (->> (.digest digester (.getBytes s))
       (map (partial format "%02x"))
       (apply str)))

(def steps [[0 -1] [0 1] [-1 0] [1 0]]) ;; up-down-left-right

;; Given a password and current path, get the hash and possible exits
;; by picking the last characters and producing new valid paths.
;; We also keep the position of the head of the path so we don't fall off the map.
;; {:path "hijklU" :pos [1 2]}

(def open #{\b \c \d \e \f})

(defn next-paths [pass {:keys [path pos]}]
  (let [hash  (md5 (str pass path))
        dirs  (into [] (take 4 hash))
        paths (for [i (range 4)
                    :let [next-chr ([\U \D \L \R] i)
                          path'    (str path next-chr)
                          [x y]    pos
                          [dx dy]  (steps i)
                          [x' y']  [(+ x dx) (+ y dy)]]
                    :when (and (<= 0 x' 3)
                               (<= 0 y' 3)
                               (open (dirs i)))]
                {:path path' :pos [x' y']})]
    paths))

;; (next-paths "hijklD" {:path "" :pos [0 1]})

(defn walk-paths [pass]
  (loop [paths [{:path "" :pos [0 0]}]]
    (let [paths' (mapcat (partial next-paths pass) paths)
          goal   (->> paths (filter (comp #{[3 3]} :pos)) first)]
      (if goal
        (:path goal)
        (recur paths')))))

;; (= (walk-paths "ihgpwlah") "DDRRRD")
;; (= (walk-paths "kglvqrro") "DDUDRLRRUDRD")
;; (= (walk-paths "ulqzkmiv") "DRURDRUDDLLDLUURRDULRLDUUDDDRR")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; part 1
;; (walk-paths "qljzarfv")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 2
;; Keep track of 2 lists: paths being walked on and finished paths
;; On every iteration, move paths that reached the end to the finished list,
;; then stop when no paths are left to walk. Return the longest path.

(defn goal? [{:keys [pos]}]
  (= [3 3] pos))

(defn longest-path [pass]
  (loop [finished [], paths [{:path "" :pos [0 0]}]]
    (if (empty? paths)
      (->> finished (sort-by (comp count :path)) last :path count)
      (let [paths'  (mapcat (partial next-paths pass) paths)
            by-goal (group-by goal? paths')]
        (recur (into finished (get by-goal true)) (get by-goal false))))))

;; (= 370 (longest-path "ihgpwlah"))
;; (= 492 (longest-path "kglvqrro"))
;; (= 830 (longest-path "ulqzkmiv"))

;; Part 2
;; (longest-path "qljzarfv")
