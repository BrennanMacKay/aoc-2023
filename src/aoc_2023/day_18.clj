(ns aoc-2023.day-18
  (:require [aoc-2023.common :as common]))

(def test-input "problems/day-18-1-t.txt")
(def p1-input "problems/day-18-1.txt")

(defn shoelace-formula
  ([points]
   (shoelace-formula (conj points (first points)) 0))
  ([[curr & rest] sum]
   (let [next (first rest)]
     (if (nil? next)
       (abs (/ sum 2))
       (recur rest (+ sum
                      (- (* (first curr)
                            (second next))
                         (* (second curr)
                            (first next)))))))))

(defn vertices
  ([instructions]
   (vertices instructions [0 0] []))
  ([[curr & rest] [x y] points]
   (if (nil? curr)
     points
     (let [[[dx dy] dist] curr
           next-x (+ x (* dist dx))
           next-y (+ y (* dist dy))]
       (recur rest [next-x next-y] (conj points [next-x next-y]))))))

(defn parse-line [line]
  (let [[_ dir dist _] (re-matches #"(\w) (\d+) \((#\w+)\)" line)
        vector (case dir
                 "U" [0 1]
                 "D" [0 -1]
                 "L" [-1 0]
                 "R" [1 0])]
    [vector (Integer/parseInt dist)]))

(defn parse-line-p2 [line]
  (let [[_ dist-hex dir] (re-matches #"\w \d+ \(#(.....)(\d)\)" line)
        vector (case dir
                 "0" [1 0]
                 "1" [0 -1]
                 "2" [-1 0]
                 "3" [0 1])
        dist (Integer/parseInt dist-hex 16)]
    [vector dist]))

(defn load-data [filename f]
  (->> filename
       common/read-input
       (map f)))

(defn run [filename f]
  (let [instructions (load-data filename f)
        vertices (vertices instructions)
        area (shoelace-formula vertices)
        perimeter (reduce + (map second instructions))
        interior (+ area (/ perimeter 2) 1)]
    [area perimeter interior]))

(defn p1 [filename]
  (run filename parse-line))

(defn p2 [filename]
  (run filename parse-line-p2))