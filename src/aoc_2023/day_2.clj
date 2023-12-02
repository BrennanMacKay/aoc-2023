(ns aoc-2023.day-2
  (:require [aoc-2023.common :as common]
            [clojure.string :as string]))

(def cube-counts {"red" 12 "green" 13 "blue" 14})
(def test-file "problems/day-2-1-t.txt")
(def p1-file "problems/day-2-1.txt")

(defn max-counts [sets]
  (apply (partial merge-with max) sets))

(defn power [sets]
  ;; do we need to deal with zero counts?
  (apply * (vals (max-counts sets))))

(defn set-possible? [counts set]
  (every? identity (map (fn [[color count]]
                          (<= count (counts color)))
                        set)))

(defn game-possible? [counts game]
  (every? identity (map (partial set-possible? counts) game)))

(defn parse-set [set-string]
  (into {} (->> set-string
                (re-seq #"((\d+) ([a-z]+))")
                (map (fn [[_ _ num color]] [color (Integer/parseInt num)])))))

(defn parse-game [line]
  (let [[game-s sets-s] (string/split line #":")
        id (Integer/parseInt (re-find #"\d+" game-s))
        sets (map parse-set (string/split sets-s #"\;"))]
    [id sets]))

(defn load-data
  ([]
   (load-data test-file))
  ([filename]
   (into {} (->> filename
                 common/read-input
                 (map parse-game)))))


(defn run-1 [filename]
  (->> filename
       load-data
       (map (fn [[k v]] [k (game-possible? cube-counts v)]))
       (filter #(last %))
       (map first)
       (apply +)))

(defn run-2 [filename]
  (->> filename
       load-data
       (map (fn [[k v]] (power v)))
       (apply +)))
