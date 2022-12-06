(ns aoc22.day06
  (:require [clojure.string :as str]))

(def sample-1 "bvwbjplbgvbhsrlpgdmjqwftvncz")

(defn p1
  [s]
  (loop [i 0]
    (if (= 4 (count (into #{} (subs s i (+ i 4)))))
      (+ 4 i)
      (recur (+ 1 i)))
    )
  )

(defn p2
  [s]
  (loop [i 0]
    (if (= 14 (count (into #{} (subs s i (+ i 14)))))
      (+ 14 i)
      (recur (+ 1 i)))
    )
  )
