(ns aoc22.day02
  (:require [clojure.string :as str]))

(def op-moves {:A :rock
               :B :paper
               :C :scissors})

(def my-moves {:X :rock
               :Y :paper
               :Z :scissors})

(def shape-point {:rock 1
                  :paper 2
                  :scissors 3})

(def round-points {:loss 0
                   :draw 3
                   :win 6})


(defn calc-round [line]
  (let [[op my] (map keyword (str/split line #" "))
        op-move (op op-moves)
        my-move (my my-moves)
        move-point (my-move shape-point)]
    (+ move-point
       (if (= op-move my-move)
         (:draw round-points)
         (condp = [op-move my-move]
           [:scissors :rock] (:win round-points)
           [:paper :scissors] (:win round-points)
           [:rock :paper] (:win round-points)
           (:loss round-points))))))

(defn calculate-score [data]
  (->>
      (str/split data #"\n")
      (map calc-round)
      (apply +)))

;(def data (slurp "./src/aoc22/day02.data"))
