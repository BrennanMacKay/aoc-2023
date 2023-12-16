(ns aoc-2023.day-16
  (:require [aoc-2023.common :as common]))

(def test-input "problems/day-16-1-t.txt")
(def p1-input "problems/day-16-1.txt")


(defn in-bounds? [x y xb yb]
  (and (<= 0 x xb)
       (<= 0 y yb)))

(defn redirect [mirror dx dy]
  (cond
    (and (= \\ mirror) (= 0 dx)) [dy 0]
    (and (= \\ mirror) (= 0 dy)) [0 dx]
    (and (= \/ mirror) (= 0 dx)) [(* -1 dy) 0]
    (and (= \/ mirror) (= 0 dy)) [0 (* -1 dx)]))

(defn pathing
  ([grid x y xb yb dx dy]
   ;(prn [x y] [dx dy])
   (pathing grid x y xb yb dx dy #{} (common/queue)))
  ([grid x y xb yb dx dy visited next]
   (let [curr (or (grid [x y]) \.)
         already-visited? (contains? visited [x y dx dy])
         visited (if (in-bounds? x y xb yb) (conj visited [x y dx dy]) visited)]
     ;(prn [x y] [dx dy] curr next visited)
     (cond
       (or (not (in-bounds? x y xb yb))
           already-visited?)
       (do
         ;(prn "out of bounds")
         (if (peek next)
           (let [[next-x next-y next-dx next-dy] (peek next)]
             ;(prn "NEXT" [next-x next-y next-dx next-dy])
             (recur grid next-x next-y xb yb next-dx next-dy visited (pop next)))
           (set (map (fn [[x y _ _]] [x y]) visited))))

       (= \. curr)
       (do
         ;(prn "dot")
         (recur grid (+ x dx) (+ y dy) xb yb dx dy visited next))

       (or (= \\ curr) (= \/ curr))
       (do
         ;(prn "mirror")
         (let [[next-dx next-dy] (redirect curr dx dy)]
           (recur grid (+ x next-dx) (+ y next-dy) xb yb next-dx next-dy visited next)))

       (= \- curr)
       (do
         ;(prn "split -")
         (if (= 0 dy)
           (recur grid (+ x dx) (+ y dy) xb yb dx dy visited next)
           (recur grid (+ 1 x) y xb yb 1 0 visited (conj next [(- x 1) y -1 0]))))

       (= \| curr)
       (do
         ;(prn "split |")
         (if (= 0 dx)
           (recur grid (+ x dx) (+ y dy) xb yb dx dy visited next)
           (recur grid x (+ 1 y) xb yb 0 1 visited (conj next [x (- y 1) 0 -1]))))))))

(defn parse-line
  ([[curr & rest] x y grid]
   (cond
     (nil? curr) grid
     (= \. curr) (recur rest (inc x) y grid)
     :else (recur rest (inc x) y (assoc grid [x y] curr)))))

(defn parse-lines
  ([lines]
   (parse-lines lines 0 {}))
  ([[curr & rest] y grid]
   (if (nil? curr)
     grid
     (recur rest (inc y) (parse-line curr 0 y grid)))))

(defn load-data [filename]
  (->> filename
       common/read-input))

(defn p1 [filename]
  (let [lines (load-data filename)
        xb (dec (count (first lines)))
        yb (dec (count lines))
        grid (parse-lines lines)
        visited (pathing grid 0 0 xb yb 1 0)]
    (common/draw-grid visited xb yb)
    (count visited)))

(defn p2 [filename]
  (let [lines (load-data filename)
        xb (dec (count (first lines)))
        yb (dec (count lines))
        x-range (range (inc xb))
        y-range (range (inc yb))
        grid (parse-lines lines)]
    (apply max (flatten [(map #(count (pathing grid % 0 xb yb 0 1)) x-range)
                         (map #(count (pathing grid % yb xb yb 0 -1)) x-range)
                         (map #(count (pathing grid 0 % xb yb 1 0)) y-range)
                         (map #(count (pathing grid xb % xb yb -1 0)) y-range)]))))
