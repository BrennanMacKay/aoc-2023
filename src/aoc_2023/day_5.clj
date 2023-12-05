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

(defn find-destination [[curr & rest] original v]
  (if (nil? curr)
    {:source original :destination v}
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
         (map (fn [seed] (find-destination mappings seed seed)))
         (map :destination)
         (apply min))))

(defn seed-ranges [seeds ranges]
  (let [[start length] (take 2 seeds)]
    (if (nil? start)
      ranges
      (recur (drop 2 seeds) (conj ranges [start (+ start (dec length))])))))

(defn test-locations [location seed-ranges mappings]
  (let [maybe-seed (find-destination mappings location location)
        seed? (some #(in-range? % (:destination maybe-seed)) seed-ranges)]
    (when (= 0 (mod location 10000000)) (prn location))
    (if seed?
      maybe-seed
      (recur (inc location) seed-ranges mappings))))

(defn reverse-mapping [m]
  (let [updated-mapping (mapv (fn [m] {:dest (:source m)
                                      :source (:dest m)
                                      :dist (* -1 (:dist m))})
                             (:mapping m))]
    (assoc m :mapping updated-mapping)))

(defn p2 [filename]
  (let [{:keys [seeds mappings]} (load-data filename)
        ranges (seed-ranges seeds [])
        mappings (reverse mappings)
        mappings (mapv reverse-mapping mappings)]
    (test-locations 1 ranges mappings)))
