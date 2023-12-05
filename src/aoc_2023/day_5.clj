(ns aoc-2023.day-5
  (:require [aoc-2023.common :as common]
            [clojure.string :as string]))

(def test-input "problems/day-5-1-t.txt")
(def p1-input "problems/day-5-1.txt")

(defn in-range?
  "inclusive range check"
  [[start end] v]
  (<= start v end))

;; 98 98 | 50 51
;; 98 - 50 = 48
;; 98 - 48 = 50
(defn destination [[curr & rest] v]
  (cond
    (nil? curr) v
    (in-range? (:source curr) v) (- v (:dist curr))
    :else (recur rest v)))

(defn find-location [[curr & rest] original v]
  (if (nil? curr)
    {:seed original :location v}
    (let [next (destination (:mapping curr) v)]
      ;(clojure.pprint/pprint (str "v " v " next " next " curr " curr))
      (recur rest original next))))

(defn parse-ranges [[curr & rest] mapping]
  (if (or (nil? curr) (empty? curr))
    [mapping rest]
    (let [[dest source length] (map #(Long/parseLong %) (string/split curr #" "))]
      (recur rest (conj mapping {:dest [dest (+ dest (dec length))]
                                 :source [source (+ source (dec length))]
                                 :dist (- source dest)})))))

(defn parse-range-maps [[curr & rest] mappings]
  (if (nil? curr)
    mappings
    (let [[mapping rest] (parse-ranges rest [])]
      (recur rest (conj mappings {:name curr :mapping mapping})))))


(defn parse-seeds [seed-line]
  (map #(Long/parseLong %) (re-seq #"\d+" seed-line)))

(defn load-data [filename]
  (let [raw-data (common/read-input filename)
        seeds (parse-seeds (first raw-data))
        mapping (parse-range-maps (drop 2 raw-data) [])]
    {:seeds seeds :mappings mapping}))

(defn p1 [filename]
  (let [{:keys [seeds mappings]} (load-data filename)]
    ;(clojure.pprint/pprint [seeds mappings])
    (->> seeds
         (map (fn [seed] (find-location mappings seed seed)))
         (map :location)
         (apply min))))

(defn expand-seeds [seeds new-seeds]
  (let [[start length] (take 2 seeds)]
    (if (nil? start)
      new-seeds
      (recur (drop 2 seeds) (conj new-seeds (range start (+ start length)))))))

(defn p2 [filename]
  (let [{:keys [seeds mappings]} (load-data filename)
        seeds (flatten (expand-seeds seeds []))]
    (->> seeds
         (map (fn [seed] (find-location mappings seed seed)))
         (map :location)
         (apply min))))
