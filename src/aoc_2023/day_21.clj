(ns aoc-2023.day-21
  (:require [aoc-2023.common :as common]))

(def test-input "problems/day-21-1-t.txt")
(def test-input-2 "problems/day-21-1-t-2.txt")
(def p1-input "problems/day-21-1.txt")

(defn get-inf [grid x y]
  (let [max (alength grid)
        x (mod x max)
        y (mod y max)]
    (aget grid y x)))

(def adjacent
  (memoize (fn [grid [x y] part]
             (let [max (dec (alength grid))
                   adj [[(inc x) y]
                        [(dec x) y]
                        [x (inc y)]
                        [x (dec y)]]
                   gardens (filter (fn [[x y]]
                                     (if (= :part-2 part)
                                       (not= \# (get-inf grid x y))
                                       (and (<= 0 x max)
                                            (<= 0 y max)
                                            (not= \# (aget grid y x))))) adj)]
               gardens))))

(defn take-step
  ([grid locations part]
   (take-step grid locations part #{}))
  ([grid [curr & rest] part next-locations]
   (if (nil? curr)
     next-locations
     (recur grid rest part (apply (partial conj next-locations) (adjacent grid curr part))))))

(defn load-grid [lines]
  (to-array-2d lines))

(defn load-data [filename]
  (->> filename
       common/read-input
       load-grid))

;  01234567890
;0 ...........
;1 .....###.#.
;2 .###.##..#.
;3 ..#.#...#..
;4 ....#.#....
;5 .##..S####.
;6 .##..#...#.
;7 .......##..
;8 .##.#.####.
;9 .##..##.##.
;10...........

(defn p1 [filename steps part]
  (let [grid (load-data filename)
        size (alength grid)
        center (int (/ size 2))
        start-pos [center center]
        gardens (reduce (fn [locations step]
                          (let [result (take-step grid locations part)]
                            ;(when (= 0 (mod step 1)) (prn (str step \, (count result))))
                            result)) #{start-pos} (range 1 (inc steps)))]
    (count gardens)))

;; P2
;; 65 3821
;; 196 34234
;; 327 94963
;; https://www.dcode.fr/lagrange-interpolating-polynomial
;; 620348631910321
