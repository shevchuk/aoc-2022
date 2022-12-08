(ns aoc22.day08
  (:require [clojure.string :as str]))

(def sample-input "30373
25512
65332
33549
35390")

(defn el
  [matrix i j]
  (nth (nth matrix j) i))

(defn build-matrix
  [input]
  (map #(map (fn [c](Integer/parseInt c)) (str/split % #""))
       (str/split input #"\n")))


(defn get-vertical-top [matrix i j] (map #(el matrix i %) (range 0 j)))
(defn get-vertical-bottom [matrix i j] (map #(el matrix i %) (range (+ 1 j) (count matrix))))
(defn get-horizontal-left [matrix i j] (map #(el matrix % j) (range 0 i)))
(defn get-horizontal-right [matrix i j] (map #(el matrix % j) (range (+ 1 i) (count (nth matrix 0)) )))

(defn max-safe [v]
  (if (empty? v)
    0
    (apply max v)))

(defn edge?
  ""
  [matrix i j]
  (or (= i 0)
      (= j 0)
      (= i (- (count (nth matrix 0)) 1))
      (= j (- (count matrix) 1))))

(defn el-before
  [target v]
  (let [visible-trees (count (take-while #(< % target) (reverse v)))]
    (if (= visible-trees (count v))
      visible-trees
      (+ 1 visible-trees)
      ))
  )

(defn el-after
  [target v]
  (let [visible-trees (count (take-while #(< % target) v))]
    (if (= visible-trees (count v))
      visible-trees
      (+ 1 visible-trees)
      )))


(defn calc-score
  [matrix x y]
  (let [target (el matrix x y)]
    (if (edge? matrix x y)
      0
      (apply * [(el-before target (get-vertical-top matrix x y))
                (el-after target (get-vertical-bottom matrix x y))
                (el-before target (get-horizontal-left matrix x y))
                (el-after target (get-horizontal-right matrix x y))]))))


(defn p1
  [matrix]
  (count (filter #(= true %)
                 (for [x (range 0 (count (nth matrix 0)))
                       y (range 0 (count matrix))]
                   (let [target (el matrix x y)]
                     (or (edge? matrix x y)
                         (> target (apply min [(max-safe (get-vertical-top matrix x y))
                                           (max-safe (get-vertical-bottom matrix x y))
                                           (max-safe (get-horizontal-left matrix x y))
                                           (max-safe (get-horizontal-right matrix x y))]))
                         )
                     )))))


(defn p2
  [matrix]
  (apply max
   (for [x (range 0 (count (nth matrix 0)))
         y (range 0 (count matrix))]
       (calc-score matrix x y))))
