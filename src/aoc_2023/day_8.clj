(ns aoc-2023.day-8
  (:require [aoc-2023.common :as common]
            [clojure.string :as string]
            [clojure.math.numeric-tower :as nt]))

(def test-input-1 "problems/day-8-1-t-1.txt")
(def test-input-2 "problems/day-8-1-t-2.txt")
(def test-input-3 "problems/day-8-1-t-3.txt")
(def p1-input "problems/day-8-1.txt")

(def start "AAA")
(def end "ZZZ")

(defn follow-inst-p1
  ([{:keys [inst entries]} ends-at starts-at]
   (follow-inst-p1 inst entries inst 0 starts-at ends-at))
  ([[curr & rest] entries original-inst steps position ends-at]
   (cond
     (nil? curr) (recur original-inst entries original-inst steps position ends-at)
     (string/ends-with? position ends-at) steps
     :else (recur rest entries original-inst (inc steps) ((entries position) curr) ends-at))))

(defn next-positions [entries positions inst]
  (map #((entries %) inst) positions))

(defn parse-input [lines]
  (let [inst (first lines)
        entries (into {} (map #(let [[pos l r] (re-seq #"\w+" %)] [pos {\L l \R r}])
                              (drop 2 lines)))
        last-letters (group-by :last (map #(let [[pos l r] (re-seq #"\w+" %)]
                                             {:last (last pos) :pos pos \L l \R r})
                                          (drop 2 lines)))]
    {:inst inst :entries entries :last-letter last-letters}))


(defn load-data [filename]
  (->> filename
       common/read-input
       parse-input))

(defn p1 [filename]
  (let [data (->> filename load-data)]
    (follow-inst-p1 data end start)
    ))

(defn p2 [filename]
  (let [data
        (->> filename
             load-data)
        steps (map #(follow-inst-p1 data "Z" %) (map :pos ((:last-letter data) \A)))]
    (reduce nt/lcm steps)))
