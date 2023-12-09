(ns aoc-2023.day-9
  (:require [aoc-2023.common :as common]
            [clojure.string :as string]))

(def test-input "problems/day-9-1-t.txt")
(def p1-input "problems/day-9-1.txt")

(defn predict
  ([[curr & rem]]
   (predict curr rem [] [(last rem)]))
  ([curr [next & rem] differences last-vals]
   ;(clojure.pprint/pprint [(str curr " " next) rem "diffs" differences "lasts" last-vals])
   (cond
     ;; we need to figure out if we need to go deeper or start calculating
     (nil? next)
     (cond
       ;; need to go deeper
       (some #(not (= 0 %)) differences)
       (recur (first differences) (rest differences) [] (conj last-vals (last differences)))

       ;; we're done!
       :else
       (do
         (reduce + (conj last-vals (or (last differences) 0)))))

     :else
     (recur next rem (conj differences (- next curr)) last-vals))))

(defn parse-line [line]
  (mapv #(Integer/parseInt %) (string/split line #"\s")))

(defn load-data [filename]
  (->> filename
       common/read-input
       (map parse-line)))

(defn p1 [filename]
  (->> filename
       load-data
       (map predict)
       (reduce +)))

(defn p2 [filename]
  (->> filename
       load-data
       (map reverse)
       (map predict)
       (reduce +)))
