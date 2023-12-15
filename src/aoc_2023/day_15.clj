(ns aoc-2023.day-15
  (:require [aoc-2023.common :as common]
            [clojure.string :as string]))

(def test-input "problems/day-15-1-t.txt")
(def p1-input "problems/day-15-1.txt")

(defn string-hash
  ([s]
   (string-hash s 0))
  ([[curr-char & rest] curr-val]
   (if (nil? curr-char)
     curr-val
     (recur rest (-> curr-char
                     int
                     (+ curr-val)
                     (* 17)
                     (mod 256))))))

(defn gen-dict [size]
  (vec (repeat size [])))

(defn add-label [dict idx inst]
  (let [[label value] (string/split inst #"=")
        update-fn (fn [row]
                    (if (not (some #(#{label} (first %)) row))
                      (conj row [label (Integer/parseInt value)])
                      (assoc row (first (keep-indexed #(when (= label (first %2)) %1) row)) [label (Integer/parseInt value)])))]
    (update dict idx update-fn)))

(defn remove-label [dict idx inst]
  (let [label (first (string/split inst #"-"))]
    (update dict idx (fn [row] (vec (remove #(= label (first %)) row))))))

(defn handle-instruction [dict inst]
  (let [set-inst (re-matches #"(\w+)=(\d+)" inst)
        rem-inst (re-matches #"(\w+)-" inst)]
    (cond
      set-inst (add-label dict (string-hash (second set-inst)) inst)
      rem-inst (remove-label dict (string-hash (second rem-inst)) inst))))

(defn handle-instructions
  ([dict [curr & rest]]
   (if (nil? curr)
     dict
     (recur (handle-instruction dict curr) rest))))

(defn score-dict [dict]
  (map-indexed (fn [box-num row]
                 (map-indexed (fn [row-num [_ length]]
                                (* (inc box-num)
                                   (inc row-num)
                                   length)) row)) dict))

(defn load-data [filename]
  (-> filename
      common/read-input
      first
      (string/split #",")))

(defn p1 [filename]
  (->> filename
       load-data
       (map string-hash)
       (reduce +)))

(defn p2 [filename]
  (let [dict (->> filename
                  load-data
                  (handle-instructions (gen-dict 256)))
        scores (score-dict dict)]
    (reduce + (flatten scores))))