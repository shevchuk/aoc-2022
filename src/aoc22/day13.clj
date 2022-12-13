(ns aoc22.day13
  (:require [clojure.string :as str]))

(def sample-input "[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]")

(def input (slurp "src/aoc22/day13.dat"))

(defn check-order-old [a b]
  (cond
    (and (int? a)
         (int? b)) (<= a b)
    (and (vector? a)
         (vector? b)) (boolean
                       (or
                        (and (empty? a) (not (empty? b)))
                        (if (and (not (some false? (map int? a)))
                                 (not (some false? (map int? b))))
                          (or (not (some false? (map < a b)))
                              (and (not (some false? (map check-order a b)))
                                   (<= (count a) (count b))))
                          (and (not (some false? (map check-order a b)))
                                   (<= (count a) (count b)))
                          )))
    (and (vector? a)
         (int? b)) (check-order a [b])
    (and (int? a)
         (vector? b)) (check-order [a] b)
    :else false))

(defn fill-missing [a b]
  (let [m (max (count a) (count b))]
    [(if (> m (count a))
      (into a (take (- m (count a)) (repeat -1)))
      a)
     (if (> m (count b))
      (into b (take (- m (count b)) (repeat -1)))
      b)
     ]
  ))

(defn check-order
  ""
  [a b]
  (>= 0 (check-order-int a b))
  )

(defn check-order-int [a b]
  (cond
    (and (int? a)
         (int? b)) (compare a b)
    (and (vector? a)
         (vector? b)) (let [[a1 b1] (fill-missing a b)
                            res (map check-order-int a1 b1)
                            items-correct (take-while #(>= 0 %) res)]
                        (if (some neg? items-correct)
                          -1
                          (if (some pos? res)
                            1
                            (if (= (count a) (count items-correct))
                              -1
                              1))))
    (and (vector? a)
         (int? b)) (check-order-int a [b])
    (and (int? a)
         (vector? b)) (check-order-int [a] b)))


(defn parse-input
  [input]
  (map #(identity [(read-string (first (str/split % #"\n")))
                   (read-string (second (str/split % #"\n")))])
       (str/split input #"\n\n"))
  )

(defn p1
  ""
  [input-raw]
  (let [inp (parse-input input-raw)
        indices (map inc (range (count inp)))
        zipped (zipmap indices inp)]
    (reduce + (map first(filter #(apply check-order (second %)) zipped)))))
