(ns aoc-2023.day-11
  (:require [aoc-2023.common :as common]))

(def test-input "problems/day-11-1-t.txt")
(def p1-input "problems/day-11-1.txt")

(defn parse-galaxy
  ([[curr & rem]]
   (parse-galaxy 0 0 1 #{} #{} {} curr rem))
  ([x y n xs ys positions [curr & rem] lines]
   (cond
     (and (nil? curr) (empty? lines)) {:xs xs :ys ys :galaxies positions}
     (nil? curr) (recur 0 (inc y) n xs ys positions (first lines) (rest lines))
     (= \. curr) (recur (inc x) y n xs ys positions rem lines)
     :else (recur (inc x) y (inc n) (conj xs x) (conj ys y)
                  (assoc positions [x y] n) rem lines))))

(defn empty-line [max non-empty]
  (into #{} (filter #(not (contains? non-empty %)) (range (inc max)))))

(defn load-data [filename]
  (let [data (common/read-input filename)
        max-x (dec (count (first data)))
        max-y (dec (count data))
        universe (assoc (parse-galaxy data) :max-x max-x :max-y max-y)
        empty-x (empty-line max-x (:xs universe))
        empty-y (empty-line max-y (:ys universe))
        universe (-> universe
                   (assoc :empty-x empty-x :empty-y empty-y)
                   (dissoc :xs :xy))]
    universe))

(defn within-range [a b v]
  (if (< a b)
    (< a v b)
    (< b v a)))

(defn distance-line [a b expands factor]
  (let [raw-distance (abs (- b a))
        expansions (count (filter (partial within-range a b) expands))]
    (+ raw-distance (* factor expansions))))

(defn distance [[x1 y1] [x2 y2] x-expands y-expands factor]
  (+ (distance-line x1 x2 x-expands factor)
     (distance-line y1 y2 y-expands factor)))

(defn pairs [galaxies]
  (into #{} (remove nil?)
                (for [a (keys galaxies) b (keys galaxies)]
                  (when (not (= a b)) #{a b}))))

;; use 1 for expansion for p1 and 999999 for p2
(defn p1 [filename expansion]
  (let [{:keys [galaxies empty-x empty-y] :as universe} (load-data filename)
        pairs (pairs galaxies)]
    (->> pairs
         (map (fn [pair] (distance (first pair) (second pair) empty-x empty-y expansion)))
         (reduce +))))
