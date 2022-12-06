(ns aoc22.day06
  (:require [clojure.string :as str]))

(def sample-1 "bvwbjplbgvbhsrlpgdmjqwftvncz")

(defn find-pos
  ""
  [len s]
  (loop [i 0]
    (if (= len (count (into #{} (subs s i (+ i len)))))
      (+ len i)
      (recur (+ 1 i)))))

(defn p1 [s] (find-pos 4 s))

(defn p2 [s] (find-pos 14 s))
