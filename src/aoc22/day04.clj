(ns aoc22.day04
  (:require [clojure.string :as str]))

(def sample-data
  "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8")


(defn gen-range
  [range-str]
  (let [r (mapv #(Integer/parseInt %) (str/split range-str #"-"))]
    (range (first r) (+ 1 (second r)))
      )
  )

(defn find-overlaps [ranges]
  (count (filter true? (map #(or (empty? (clojure.set/difference
                     (into #{} (first %))
                     (into #{} (second %))))
            (empty? (clojure.set/difference
                     (into #{} (second %))
                     (into #{} (first %)))))
       ranges))))

(defn find-overlaps-2 [ranges]
  (count (filter true? (map #(not (empty? (clojure.set/intersection
                     (into #{} (first %))
                     (into #{} (second %)))))
                            ranges))))

(defn p1
  ""
  [data]
  (find-overlaps (map #(map gen-range (str/split % #","))
     (str/split data #"\n")))
  )

(defn p2
  ""
  [data]
  (find-overlaps-2 (map #(map gen-range (str/split % #","))
     (str/split data #"\n")))
  )
