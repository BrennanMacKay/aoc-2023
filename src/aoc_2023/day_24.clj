(ns aoc-2023.day-24
  (:require [aoc-2023.common :as common]))

(def test-input "problems/day-24-1-t.txt")
(def p1-input "problems/day-24-1.txt")

(defn parse-line [idx line]
  (let [[x y z dx dy dz] (map #(Long/parseLong %) (re-seq #"\-?\d+" line))
        x2 (+ x dx)
        y2 (+ y dy)
        z2 (+ z dz)
        m (/ (- y2 y) (- x2 x))]
    {:id idx
     :p1 [x y z]
     :p2 [x2 y2 z2]
     :v  [dx dy dz]
     :m m
     :c (- y (* m x))}))

(defn load-data [filename]
  (->> filename
       common/read-input
       (map-indexed parse-line)))

(defn point-ahead? [point origin vector]
  (let [result (cond
                 (pos-int? vector) (<= origin point)
                 (neg-int? vector) (<= point origin)
                 (= 0 vector) (= origin point))]
    ;(prn (float point) (float origin) vector result)
    result))

(defn intersect [l1 l2]
  (let [m1 (:m l1)
        m2 (:m l2)
        c1 (:c l1)
        c2 (:c l2)
        denom (- m1 m2)
        num (- c2 c1)]
    (if (= 0 denom)
      nil
      (let [x (/ num denom)
            y (+ (* m1 (/ num denom)) c1)
            [dx1 dy1 _] (:v l1)
            [dx2 dy2 _] (:v l2)
            [x1 y1 _] (:p1 l1)
            [x3 y3 _] (:p1 l2)
            x-ahead? (and (point-ahead? x x1 dx1)
                          (point-ahead? x x3 dx2))
            y-ahead? (and (point-ahead? y y1 dy1)
                          (point-ahead? y y3 dy2))]
        (if (and x-ahead? y-ahead?)
          {:a l1 :b l2 :intersect [x y]}
          nil)))))

(defn intersections [lines]
  (for [a lines
        b lines]
    (if (= a b)
      nil
      (intersect a b))))

(defn p1 [filename min max]
  (let [input (load-data filename)
        crosses (remove nil? (intersections input))
        in-range (filter #(let [[x y] (:intersect %)]
                            (and (<= min x max)
                                 (<= min y max))) crosses)]
    (prn (count crosses))
    (/ (count in-range) 2)))

(defn p2 [filename]
  (let [input (load-data filename)
        [s1 s2 s3] (take 3 input)
        [x1 y1 z1] (:p1 s1)
        [x2 y2 z2] (:p1 s2)
        [x3 y3 z3] (:p1 s3)
        [dx1 dy1 dz1] (:v s1)
        [dx2 dy2 dz2] (:v s2)
        [dx3 dy3 dz3] (:v s3)
        output "src/aoc_2023/day-24-sage"]
    ;; clear the file?
    (spit output "")
    (spit output "var('x y z dx dy dz t1 t2 t3')\n" :append true)
    (spit output (str "x1 = x + (dx * t1) == " x1 " + " dx1 " * t1\n") :append true)
    (spit output (str "y1 = y + (dy * t1) == " y1 " + " dy1 " * t1\n") :append true)
    (spit output (str "z1 = z + (dz * t1) == " z1 " + " dz1 " * t1\n") :append true)
    (spit output (str "x2 = x + (dx * t2) == " x2 " + " dx2 " * t2\n") :append true)
    (spit output (str "y2 = y + (dy * t2) == " y2 " + " dy2 " * t2\n") :append true)
    (spit output (str "z2 = z + (dz * t2) == " z2 " + " dz2 " * t2\n") :append true)
    (spit output (str "x3 = x + (dx * t3) == " x3 " + " dx3 " * t3\n") :append true)
    (spit output (str "y3 = y + (dy * t3) == " y3 " + " dy3 " * t3\n") :append true)
    (spit output (str "z3 = z + (dz * t3) == " z3 " + " dz3 " * t3\n") :append true)
    (spit output "solve([x1, y1, z1, x2, y2, z2, x3, y3, z3], x, y, z, dx, dy, dz, t1, t2, t3)\n" :append true)))


;; [[x == 131246724405205, y == 399310844858926, z == 277550172142625, dx == 279, dy == -184, dz == 16, t1 == 130621773037, t2 == 423178590960, t3 == 631793973864]]
;; (+ 131246724405205 399310844858926 277550172142625)