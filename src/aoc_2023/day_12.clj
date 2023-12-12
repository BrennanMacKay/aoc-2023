(ns aoc-2023.day-12
  (:require [aoc-2023.common :as common]
            [clojure.string :as string]))

(def test-input "problems/day-12-1-t.txt")
(def p1-input "problems/day-12-1.txt")

(defn contains-empty? [pattern length]
  (some #{\.} (take length pattern)))

(defn replace-first [seq char]
  (conj (drop 1 seq) char))

;; not sure if I'm dealing with the memoized function correctly but it works :shrug:
(defn fits [f pattern sequence]
  ;(clojure.pprint/pprint [pattern sequence])
  (let [pattern (drop-while #(= \. %) pattern)
        curr-seq (first sequence)]
    (cond
      (and (empty? pattern) (empty? sequence))
      (do
        ;(prn "Empty Empty")
        1)

      (empty? pattern)
      (do
        ;(prn "Empty Pattern")
        0)

      (nil? curr-seq)
      (if (some #{\#} pattern)
        (do
          ;(prn "out of sequences but still #")
          0)
        (do
          ;(prn "out of sequences and out of #")
          1))

      (= \# (first pattern))
      (cond
        (< (count pattern) curr-seq)
        (do
          ;(prn "Can't fit Seq")
          0)

        (contains-empty? pattern curr-seq)
        (do
          ;(prn "Can't fit due to empty")
          0)

        (= curr-seq (count pattern))
        (if (= 1 (count sequence))
          (do
            ;(prn "Perfect fit")
            1)
          (do
            ;(prn "Can't fit")
            0))

        (= \# (nth pattern curr-seq))
        (do
          ;(prn "Req Sep")
          0)

        :else (do
                ;(prn "Drop Spring")
                (f f (drop (inc curr-seq) pattern) (rest sequence))))

      ;; we must be on a \?
      :else (do
              ;(prn "Testing options")
              (+ (f f (replace-first pattern \#) sequence)
                 (f f (replace-first pattern \.) sequence))))))

(def fits-mem (memoize fits))

(defn parse-line [line]
  (let [[pattern seq-string] (string/split line #"\s")
        sequence (mapv #(Integer/parseInt %) (string/split seq-string #","))]
    [pattern sequence]))

(defn load-data [filename]
  (->> filename
       common/read-input
       (map parse-line)))

(defn expand-line [[pattern sequence]]
  [(string/join \? (repeat 5 pattern)) (vec (flatten (repeat 5 sequence)))])

(defn p1 [filename]
  (->> filename
       load-data
       (map (fn [[pattern sequence]] (fits-mem fits-mem pattern sequence)))
       (reduce +)))

(defn p2 [filename]
  (->> filename
       load-data
       (map expand-line)
       (map (fn [[pattern sequence]] (fits-mem fits-mem pattern sequence)))
       (reduce +)))

