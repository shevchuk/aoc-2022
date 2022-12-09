(ns aoc22.day09p2
  (:require [clojure.string :as str]))

(def sample-input "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2")

(def sample-input-2 "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20")


(def input (slurp "./src/aoc22/day09.data"))

(defn render-state
  ""
  [rope]
  (let [width 30
        height 30
        ]
    (doseq [i (range -30 30)]
      (doseq [k (range -30 30)]
        (if (some #(= [k i] %) rope)
          (print "x")
          (print "."))
        )
      (print "\n")
        ;tallest-col (reduce #(max (count %)) crates)]
    )
    (flush)
    ))

(defn gen-rope []
  (take 10 (repeat [0 0])))

; this is definetely can be improved
(defn calc-tail-coords [h-new t]
  (let [h-new-x (first h-new)
        h-new-y (second h-new)
        t-x (first t)
        t-y (second t)
        diff-x (abs (- h-new-x t-x))
        diff-y (abs (- h-new-y t-y))]
    (if (or (> diff-x 1)
            (> diff-y 1))
      [(+ t-x (if (= 0 diff-x)
               0
               (if (> h-new-x t-x)
                 1 -1)))
       (+ t-y (if (= 0 diff-y)
               0
               (if (> h-new-y t-y)
                 1 -1)))]
      [t-x t-y]
      )
    ))

(defn calc-new-rope [rope h-new]
  (loop [r rope
         head-new h-new
         acc []]
    (if (empty? r)
      acc
      (let [next-head (calc-tail-coords head-new (first r))
            ]
        (recur (rest r)
               next-head
               (conj acc next-head))))))

(defn p2 [input]
  ""
  (count
   (into #{}
         (map #(nth % 9)
              (let [parsed-input (map #(str/split % #" ") (str/split input #"\n"))
                    unfolded (flatten (map #(take (read-string (last %)) (repeat (first %))) parsed-input))
                    rope (gen-rope)]
                (loop [steps unfolded
                       r rope
                       acc [rope]]
                  (if (empty? steps)
                    acc
                    (let [h (first r)
                          h-new (condp = (first steps)
                                  "R" [(inc (first h)) (second h)]
                                  "L" [(dec (first h)) (second h)]
                                  "U" [(first h) (inc (second h))]
                                  "D" [(first h) (dec (second h))])
                          rope-new (into [h-new] (calc-new-rope (rest r) h-new))
                          ;_ (println (render-state rope-new))
                          ]
                      (recur (rest steps)
                             rope-new
                             (conj acc rope-new))))))))))
