(ns aoc22.day07
  (:require [clojure.string :as str]))

(def sample-input "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir eee
29116 f
2557 g
62596 h.lst
$ cd eee
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k
")

(defn parse-cmd-res [input]
  (let [cmd-res (rest (str/split input #"\$ "))
        cmd-res-pairs (map #(identity [(subs % 0 2) (subs % 2)]) cmd-res)]
    (map (fn [cmd]
           (condp = (first cmd)
      "ls" ["ls" (remove #(= "" %) (str/split (second cmd) #"\n"))]
      "cd" ["cd" (str/trim (second cmd))])) cmd-res-pairs
         )))

(defn calc-dir-size
  [dir]
  (apply + (map #(Integer/parseInt %)
                (map first (filter #(not= "dir" (first %))
                                   (map #(str/split % #" ") (second dir)))))))

(defn get-all-subdirs
  ""
  [dirs dir]
  (filter #(str/includes? (first %) dir)
          dirs))

(defn find-dir-size
  ""
  [dirs]
  (reduce (fn [acc dir]
            (assoc acc (first dir) (apply + (map calc-dir-size
             (get-all-subdirs (:dirs dirs) (first dir)))))
            )
          {} (:dirs dirs)))

(defn exec-and-calc
  [acc step]
  (condp = (first step)
    "cd" (if (= (second step) "..")
             (assoc acc :path (rest (:path acc)))
             (assoc acc :path (into (:path acc) [(second step)])))
    "ls" (assoc-in acc [:dirs
                        (subs (str/join "/" (reverse (:path acc))) 1)]
                   (second step)
    )
  ))

(defn parse-term-output
  [input]
  (reduce exec-and-calc
          {:path '() :dirs {}}
          (parse-cmd-res input)))

(defn p1
  [input]
  (apply + (map second (filter #(< (second %) 100000) (find-dir-size (parse-term-output input))))))

(defn p2
  [input]
  (let [total-used (second (last (sort-by second (find-dir-size (parse-term-output input)))))
        required-space (- 30000000 (- 70000000 total-used))]
    (second (first (filter #(< required-space (second %)) (sort-by second (find-dir-size (parse-term-output input))))))))
