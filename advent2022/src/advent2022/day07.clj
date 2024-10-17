(ns advent2022.day07
  (:require [clojure.string :as string]))

(defn parse-ls-output [lines]
  (->> lines
       (filter #(re-matches #"\d+ .+" %))
       (mapv #(let [[size name] (string/split % #" ")]
                [name (read-string size)]))
       (into {})))

(defn reduce-commands [fs path xss]
  (if-let [commands (first xss)]
    (cond
      (= commands ["cd /"])  (recur fs ["/"] (rest xss))
      (= commands ["cd .."]) (recur fs (vec (butlast path)) (rest xss))
      (= "ls" (first commands)) (let [output (rest commands)
                                      files  (parse-ls-output output)
                                      fs'    (assoc-in fs path files)]
                                  (recur fs' path (rest xss)))
      ;; the command is 'cd' into a directory
      :else (let [[_ dir] (string/split (first commands) #" ")
                  path' (into path [dir])]
              (recur fs path' (rest xss))))
    [fs path]))

(defn read-input [s]
  (->> (string/split s #"\$ ")
       rest
       (map string/split-lines)))

(defn dir-size [m]
  (->> (map (fn [[k v]]
              (if (map? v)
                (dir-size v)
                v)) m)
       (reduce + 0)))

(defn dirs-of [m]
  (let [direct (->> (filter (fn [[k v]] (map? v)) m) (map (comp vector first)))]
    (concat
     direct
     (mapcat (fn [k] (->> (dirs-of (get-in m k)) (map #(into k %)))) direct))))

(defn part-1 [s]
  (let [fs   (->> s read-input (reduce-commands {} ["/"]) first)
        dirs (dirs-of fs)]
    (->> (map #(dir-size (get-in fs %)) dirs)
         (filter #(>= 100000 %))
         (reduce + 0))))

;; (= (part-1 example) 95437)
;; (part-1 (slurp "resources/day07.txt"))

(defn part-2 [s]
  (let [fs (->> s read-input (reduce-commands {} ["/"]) first)
        free-space (- 70000000 (dir-size (get fs "/")))
        all-sizes  (->> fs dirs-of (map #(dir-size (get-in fs %))) sort)]
    (->> all-sizes
         (drop-while #(> 30000000 (+ free-space %)))
         first)))

;; (= (part-2 example) 24933642)
;; (part-2 (slurp "resources/day07.txt"))

(comment

  (assoc-in {"/" [["hello"]]} ["/" "a"] [[123 "a.txt" 456 "hello.c"]])
  
  (->> (read-input example)
       (reduce-commands {} ["/"])
       first
       #_((fn [m] (get-in m ["/"])))
       #_dir-size
       #_((fn [m] (dir-size m "e")))
       dirs-of
       #_clojure.pprint/pprint)

  (string/split "cd a" #" ")
  
  (->>
   (string/split example #"\$ ")
   rest
   (map string/split-lines)
   ;; cd /, ls, cd a, cd ..
   )
  
  (->> ["ls" "123 00.xt" "dir a" "dir b" "123 c.txt" "456 dd.dat"]
       (filter #(re-matches #"\d+ .+" %))
       (map #(let [[size name] (string/split % #" ")] [(read-string size) name]))
       )

    (->> ["ls" "123 00.xt" "dir a" "dir b" "123 c.txt" "456 dd.dat"]
         parse-ls-output)
  )

(def example
  "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k")
