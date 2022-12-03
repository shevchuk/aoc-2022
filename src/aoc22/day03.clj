(ns aoc22.day03
  (:require [clojure.string :as str]))

(def sample-contents "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw")

(def priorities
  (merge (zipmap (seq "abcdefghijklmnopqrstuvwxyz") (range 1 27))
         (zipmap (seq "ABCDEFGHIJKLMNOPQRSTUVWXYZ") (range 27 53))))

(defn find-common-item-in-rucksack
  ""
  [contents]
  (let [comp-len (/ (count contents) 2)
        comp1 (into #{} (take      comp-len contents))
        comp2 (into #{} (take-last comp-len contents))]
      (first (clojure.set/intersection comp1 comp2)))
  )

(defn p1 [data]
  (let [contents (str/split data #"\n")]
    (reduce +
            (remove nil?
                    (->> contents
                 (map find-common-item-in-rucksack)
                 (map #(get priorities %)))))))
