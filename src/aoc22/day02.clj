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

(def res-moves {:X :loss
                :Y :draw
                :Z :win})

(defn calc-round-kw [op-move my-move]
  (let [move-point (my-move shape-point)]
    (+ move-point
       (if (= op-move my-move)
         (:draw round-points)
         (condp = [op-move my-move]
           [:scissors :rock] (:win round-points)
           [:paper :scissors] (:win round-points)
           [:rock :paper] (:win round-points)
           (:loss round-points))))))

(defn calc-round [line]
  (let [[op my] (map keyword (str/split line #" "))
        op-move (op op-moves)
        my-move (my my-moves)
        move-point (my-move shape-point)]
    (calc-round-kw op-move my-move)))

(def win-lines
  [[:scissors :rock]
   [:paper :scissors]
   [:rock :paper]])

(defn find-win-move
  [op-move]
  (second (first (filter #(= (first %) op-move) win-lines))))

(defn find-loss-move
  [op-move]
  (first (first (filter #(= (second %) op-move) win-lines))))

(defn calc-round-line-2
  "Finds my-move for line for A X, where X - round result, returning result move line: op move, my line"
  [line]
  (let [[op round-res] (map keyword (str/split line #" "))
        op-move (op op-moves)
        res-kw (res-moves round-res)]
    (apply calc-round-kw
           (condp = res-kw
             :draw [op-move op-move]
             :loss [op-move (find-loss-move op-move)]
             :win [op-move (find-win-move op-move)]))))

(defn calculate-score [data]
  (->>
      (str/split data #"\n")
      (map calc-round)
      (apply +)))

(defn calculate-score-2
  ""
  [data]
  (->>
      (str/split data #"\n")
      (map calc-round-line-2)
      (apply +))
  )

;(def data (slurp "./src/aoc22/day02.data"))
