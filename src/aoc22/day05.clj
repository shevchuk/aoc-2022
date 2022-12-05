(ns aoc22.day05
  (:require [clojure.string :as str]))

(def sample
  "    [D]
[N] [C]
[Z] [M] [P]
 1   2   3

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2")


(re-seq #"\d+" *1)

(defn transpose [m]
  (apply mapv vector m))

(defn make-move
  "Makes a move, returning new crates state"
  [crates move-line]
  (let [[num-of-moves from to] (mapv #(Integer/parseInt %) move-line)
        from-index (- from 1)
        to-index   (- to 1)
        initial-crates crates]
    (loop [qty (- num-of-moves 1)
           crates-t initial-crates]
      (let [from-crate   (filter #(not= " " %) (get crates-t from-index))
            to-crate     (filter #(not= " " %) (get crates-t to-index))
            new-to-stack (into [(first (take 1 from-crate))] to-crate)
            new-crates-t  (assoc crates-t from-index (vec (drop 1 from-crate)))
            new-crates-t2 (assoc new-crates-t to-index new-to-stack)

            ]
        (if (= 0 qty)
          new-crates-t2
          (recur (- qty 1) new-crates-t2))
        )
      )
    )
  )

(defn make-move-2
  "Makes a move, returning new crates state"
  [crates move-line]
  (let [[qty from to] (mapv #(Integer/parseInt %) move-line)
        from-index (- from 1)
        to-index   (- to 1)
        ]
    (let [from-crate   (filter #(not= " " %) (get crates from-index))
          to-crate     (filter #(not= " " %) (get crates to-index))
          new-to-stack (into (vec (take qty from-crate)) (vec to-crate))
          new-crates-t  (assoc crates from-index (vec (drop qty from-crate)))
          new-crates-t2 (assoc new-crates-t to-index new-to-stack)]
      new-crates-t2)))

(defn p1
  [data]
  (let [[crates-data moves-data] (str/split data #"\n\n")
        crates-data-lines (str/split crates-data #"\n")
        num-columns (Integer/parseInt
                     (last (re-seq #"\d+" (last crates-data-lines))))
        sub-positions (mapv #(+ (* % 4) 1) (range num-columns))
        crate-vec (fn [line]
                    (mapv #(subs (str line "                                          ")
                                 % (+ 1 %))
                          sub-positions))
        crates (mapv crate-vec (drop-last crates-data-lines))
        moves (mapv #(re-seq #"\d+" %) (str/split moves-data #"\n"))]
    (->> (loop [mv moves
               crates-t (transpose crates)]
          (if (empty? mv)
            crates-t
            (recur (rest mv) (make-move crates-t (first mv)))))
         (map first)
         (apply str)
        )
    ))

(defn p2
  [data]
  (let [[crates-data moves-data] (str/split data #"\n\n")
        crates-data-lines (str/split crates-data #"\n")
        num-columns (Integer/parseInt
                     (last (re-seq #"\d+" (last crates-data-lines))))
        sub-positions (mapv #(+ (* % 4) 1) (range num-columns))
        crate-vec (fn [line]
                    (mapv #(subs (str line "                                          ")
                                 % (+ 1 %))
                          sub-positions))
        crates (mapv crate-vec (drop-last crates-data-lines))
        moves (mapv #(re-seq #"\d+" %) (str/split moves-data #"\n"))]
    (->> (loop [mv moves
               crates-t (transpose crates)]
          (if (empty? mv)
            crates-t
            (recur (rest mv) (make-move-2 crates-t (first mv)))))
         (map first)
         (apply str)
        )
    ))
