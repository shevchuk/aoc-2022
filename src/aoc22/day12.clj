(ns aoc22.day12
  (:require [clojure.string :as str]))

(def sample-input "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi")


(def input (slurp "./src/aoc22/day12.dat"))

(defn el [input [x y]]
  (if (and (>= x 0)
           (>= y 0)
           (< x (count (first input)))
           (< y (count input)))
     (let [height (first (nth (nth input y) x))]
      (if (nil? height)
        nil
        (condp = (int height)
          69 (int (first "z"))
          83 (int (first "a"))
         (int height)
              )
        ))
    999)
  )


(defn move-possible [c1 c2]
  (if (= 999 c2)
    false
    (if (> c1 c2)
      true
      (>= 1 (abs (- c1 c2))))
    )
  )

(defn move-possible-2 [c1 c2]
  (move-possible c2 c1)
  )

(defn find-next-steps [move-rule-fn input pos]
  (let [x (first pos)
        y (second pos)
        cur-height (el input pos)
        up-coord [x (dec y)]
        down-coord [x (inc y)]
        left-coord [(dec x) y]
        right-coord [(inc x) y]]
    (if (nil? cur-height)
      [nil nil nil nil]
      (list (when (move-rule-fn cur-height (el input up-coord)) up-coord)
            (when (move-rule-fn cur-height (el input down-coord)) down-coord)
            (when (move-rule-fn cur-height (el input left-coord)) left-coord)
            (when (move-rule-fn cur-height (el input right-coord)) right-coord)))))


(defn find-coords-all [input c]
  ""
  (let [inp (map #(str/split % #"")
                 (str/split input #"\n"))]
    (remove nil? (for [y (range (count inp))
          x (range (count (first inp)))]
      (if (= c (nth (nth inp y) x))
        [x y]
        nil
      )
    )))
  )


(defn find-coords [input c]
  (first (let [inp (map #(str/split % #"")
                 (str/split input #"\n"))]
    (remove nil? (for [y (range (count inp))
          x (range (count (first inp)))]
      (if (= c (nth (nth inp y) x))
        [x y]
        nil
      )
    )))))



(defn calc-weights [move-rule-fn m inp s]
  inp
  (let [cur-weight (get m s)
        next (remove nil? (find-next-steps move-rule-fn inp s))
        ]
    (loop [pts next
           m1 m]
      (if (empty? pts)
        m1
        (recur (rest pts)
               (assoc m1 (first pts) (inc cur-weight)))
      )

  )))

(defn p1 [input]
  (let [inp (map #(str/split % #"")
                 (str/split input #"\n"))
        s (find-coords input "S")
        e (find-coords input "E")
        weights (calc-weights move-possible {s 0} inp s)]
    (loop [w weights]
      (if (get w e)
        (get w e)
        (recur (apply merge (for [i (keys w)]
                              (calc-weights move-possible w inp i))))))))

(defn p1-reverse [input]
  (let [inp (map #(str/split % #"")
                 (str/split input #"\n"))
        s (find-coords input "S")
        e (find-coords input "E")
        weights (calc-weights move-possible-2 {e 0} inp e)]
    (loop [w weights]
      (if (get w s)
        (get w s)
        (recur (apply merge (for [i (keys w)]
                              (calc-weights move-possible-2 w inp i))))))))



(defn p2-reverse [input]
  (let [inp (map #(str/split % #"")
                 (str/split input #"\n"))
        e (find-coords input "E")
        a-vec (find-coords-all input "a")
        weights (calc-weights move-possible-2 {e 0} inp e)]
    (loop [w weights]
      (if (some #(get w %) a-vec)
        (get w (first (filter #(get w %) a-vec)))
        (recur (apply merge (for [i (keys w)]
                              (calc-weights move-possible-2 w inp i))))))))

(defn render [m]
  (doseq [y (range 0 41)]
    (do (println "")
        (doseq [x (range 0 77)]
          (if (get m [x y])
            (print "x")
            (print " "))))))
