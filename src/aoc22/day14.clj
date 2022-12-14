(ns aoc22.day14
  (:require [clojure.string :as str]))


(def sample-input "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9")

(def input (slurp "./src/aoc22/day14.dat"))

(defn add-points-between [pt1 pt2]
  (let [[x1 y1] [(first pt1) (second pt1)]
        [x2 y2] [(first pt2) (second pt2)]]
    (if (= x1 x2)
      (map #(identity [x1 %]) (if (< y1 y2)
                       (range y1 (inc y2))
                       (range y2 (inc y1))))
      (map #(identity [% y1]) (if (< x1 x2)
                       (range x1 (inc x2))
                       (range x2 (inc x1)))))))

(defn parse-rock-line [s]
  (let [anchor-pts (map #(identity [(read-string (first (str/split % #",")))
                                    (read-string (second (str/split % #",")))])
                        (str/split s #" -> "))]
    (loop [pts anchor-pts
           acc {}]
      (if (< (count pts) 2)
        acc
        (recur (rest pts)
               (conj acc (reduce #(assoc %1 %2 :rock) {} (add-points-between (first pts) (second pts)))))))))

(defn possible-move [m sand min-x max-x max-y]
  (let [[x y] sand
        down-coord [x (inc y)]
        leftdown-coord [(dec x) (inc y)]
        rightdown-coord [(inc x) (inc y)]
        next-coord (first (filter #(nil? (get m %))
                                  [down-coord leftdown-coord rightdown-coord]))]
    (if (nil? next-coord)
      [:rest sand]
      (if (and (<= min-x (first next-coord) max-x)
               (<= (second next-coord) max-y))
        [:next next-coord]
        [:out next-coord]))))

(defn possible-move-2 [m sand floor-y]
  (let [[x y] sand
        down-coord [x (inc y)]
        leftdown-coord [(dec x) (inc y)]
        rightdown-coord [(inc x) (inc y)]
        next-coord (first (filter #(nil? (get m %))
                                  [down-coord leftdown-coord rightdown-coord]))]
    (if (or (= (second next-coord) floor-y) (nil? next-coord))
      [:rest sand]
      [:next next-coord])))

(defn render [m]
  (let [min-x (apply min (map first (keys m)))
        max-x (apply max (map first (keys m)))
        max-y (apply max (map second (keys m)))]
      (doseq [y (range 0 (inc max-y))]
    (do (println "")
        (doseq [x (range min-x (inc max-x))]
          (condp = (get m [x y])
            :rock (print "#")
            :o    (print "o")
            (print " ")))))))

(defn p1 [input]
  (let [initial-map (apply merge (map parse-rock-line (str/split input #"\n")))
        min-x (apply min (map first (keys initial-map)))
        max-x (apply max (map first (keys initial-map)))
        max-y (apply max (map second (keys initial-map)))]
    ;(render initial-map min-x max-x max-y)
    (loop [m1 initial-map]
      (render m1)
      (let [map-round (loop [m m1
                             s [500 0]]
                        (let [next (possible-move m s min-x max-x max-y)]
                          (condp = (first next)
                            :rest (merge m {(second next) :o})
                            :out {:stopped m :final-sand (second next)}
                            :next (recur m (second next)))))]
        (if (:stopped map-round)
          (:stopped map-round)
          (recur map-round))
        ))))

(defn p2 [input]
  (let [initial-map (apply merge (map parse-rock-line (str/split input #"\n")))
        max-y (apply max (map second (keys initial-map)))
        floor-y (+ 2 max-y)]
    ;(render initial-map min-x max-x max-y)
    (loop [m1 initial-map]
      ;(render m1)
      (let [map-round (loop [m m1
                             s [500 0]]
                        (let [next (possible-move-2 m s floor-y)]
                          (if (= (second next) [500 0])
                            {:stopped (merge m {(second next) :o}) :final-sand (second next)}
                            (condp = (first next)
                              :rest (merge m {(second next) :o})
                              :next (recur m (second next))))))]
        (if (:stopped map-round)
          (:stopped map-round)
          (recur map-round))))))

(comment
  (count (filter #(= % :o) (vals (p1 input))))
  (count (filter #(= % :o) (vals (p2 input)))))
