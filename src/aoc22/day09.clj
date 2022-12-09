(ns aoc22.day09
  (:require [clojure.string :as str]))

(def sample-input "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2")


(def input (slurp "./src/aoc22/day09.data"))

(defn calc-tail-coords [h-new h-old t]
  (let [h-new-x (first h-new)
        h-new-y (second h-new)
        t-x (first t)
        t-y (second t)
        diff-x (abs (- h-new-x t-x))
        diff-y (abs (- h-new-y t-y))]
    (cond
      (or
       (or (and (= 0 diff-x) (= 1 diff-y))
           (and (= 0 diff-y) (= 1 diff-x)))
       (= 1 diff-x diff-y)
       (= 0 diff-x diff-y)) [t-x t-y]
      :else [(first h-old) (second h-old)])))

(defn p1 [input]
  (let [parsed-input (map #(str/split % #" ") (str/split input #"\n"))
        unfolded (flatten (map #(take (read-string (last %)) (repeat (first %))) parsed-input))]
    (count
     (into #{}
           (loop [steps unfolded
                  h [0 0]
                  t [0 0]
                  acc [[0 0]]]
             (if (empty? steps)
               acc
               (let [h-new (condp = (first steps)
                             "R" [(inc (first h)) (second h)]
                             "L" [(dec (first h)) (second h)]
                             "U" [(first h) (inc (second h))]
                             "D" [(first h) (dec (second h))])
                     t-new (calc-tail-coords h-new h t)]
                 (recur (rest steps)
                        h-new
                        t-new
                        (conj acc t-new)))))))))
