(ns aoc22.day10
  (:require [clojure.string :as str]))

(def inp1
  "noop
addx 3
addx -5")

(def inp2 "addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop")

(def input (slurp "./src/aoc22/day10.dat"))

(def cmd-cycles {:addx 2
                 :noop 1})

(def cycles-points (range 20 221 40))

(def crt-cycles (range 0 241 40))

(defn parse-cmd [cmd-str]
  (if (= cmd-str "noop")
    [:noop]
    [:addx (read-string (second (str/split cmd-str #" "))
                          )]))

(defn calc-registers [cmd-str x]
  (let [cmd (parse-cmd cmd-str)
        new-x (if (= (first cmd) :noop)
                x
                (+ x (second cmd)))]
    [new-x (take ((first cmd) cmd-cycles) (repeat x)
  )]))

(defn calc-cycles [input]
  (loop [x 1
         steps (str/split input #"\n")
         acc []]
    (if (empty? steps)
      (into acc [x])
      (let [res (calc-registers (first steps) x)]
        (recur (first res)
               (rest steps)
               (into acc (second res)))))))

(defn calc-x-during-cycle
  [input cycle]
  (nth (calc-cycles input) (dec cycle))
  )

(defn p1
  [input]
  (reduce + (map #(* % (calc-x-during-cycle input %)) cycles-points)
  ))

(defn p2
  [input]
  (let [cycles (calc-cycles input)
        rows (partition 40 cycles)]
    (doseq [row rows]
      (loop [c (rest row)
             pos 0
             spr-pos 1]
        (if (empty? c)
          nil
          (let [x (first c)]
            (if (some #(= pos %) [(dec spr-pos) spr-pos (inc spr-pos)])
              (print "█")
              (print "."))
            (recur (rest c)
                   (inc pos)
                   x))))
      (print "\n"))))
