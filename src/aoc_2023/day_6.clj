(ns aoc-2023.day-6
  (:require [aoc-2023.common :as common]
            [clojure.string :as string]))

(def test-input "problems/day-6-1-t.txt")
(def p1-input "problems/day-6-1.txt")

(defn distance-for-time [time race-time]
  (let [move-time (- race-time time)]
    (* move-time time)))

(defn first-win [time race-time required-distance f]
  (cond
    (or (= time race-time) (= time 0))
    (throw (ex-info "No winner" {:race race-time :distance required-distance}))

    (< required-distance (distance-for-time time race-time))
    time

    :else
    (recur (f time) race-time required-distance f)))

(defn win-count [[race-time distance]]
  (let [lowest-win (first-win 1 race-time distance inc)
        highest-win (first-win (dec race-time) race-time distance dec)]
    (clojure.pprint/pprint [lowest-win highest-win])
    (if (= lowest-win highest-win)
      1
      (+ 1 (- highest-win lowest-win)))))

(defn parse-lines [lines]
  (let [times (map #(Long/parseLong %) (re-seq #"\d+" (first lines)))
        distances (map #(Long/parseLong %) (re-seq #"\d+" (last lines)))]
    (map vector times distances)))

(defn load-data [filename]
  (->> filename
       common/read-input))

(defn p1 [filename]
  (->> filename
       load-data
       parse-lines
       (map win-count)
       (reduce *)))

(defn p2 [filename]
  (->> filename
       load-data
       (map #(string/replace % " " ""))
       parse-lines
       (map win-count)))