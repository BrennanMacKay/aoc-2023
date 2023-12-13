(ns aoc-2023.day-13
  (:require [aoc-2023.common :as common]))

(def test-input "problems/day-13-1-t.txt")
(def p1-input "problems/day-13-1.txt")

(defn difference-max
  ([a b max] (difference-max a b max 0))
  ([[curr-a & rest-a] [curr-b & rest-b] max diff]
   (cond
     (< max diff)
     diff

     (or (nil? curr-a) (nil? curr-b))
     diff

     (not= curr-a curr-b)
     (recur rest-a rest-b max (inc diff))

     :else
     (recur rest-a rest-b max diff))))

(defn test-reflection
  ([pattern required-smudge fold ]
   (test-reflection pattern required-smudge fold 0 0))
  ([pattern required-smudge fold offset smudge-count]
   (let [length (count pattern)
         a (- fold offset)
         b (+ (inc fold) offset)
         line-a (get pattern a)
         line-b (get pattern b)
         diff (difference-max line-a line-b required-smudge)]
     ;(clojure.pprint/pprint ["cur" fold offset "d" diff smudge-count length a b line-a line-b])

     (cond
       (< required-smudge smudge-count)
       (do
         ;(prn "over smudge allowance")
         nil)

       (or (< a 0)
           (<= length b))
       (do
         ;(prn "End of pattern")
         (if (= required-smudge smudge-count)
           (do
             ;(prn "success")
             fold)
           (do
             ;(prn "failure")
             nil)))

       (< required-smudge diff)
       (do
         ;(prn "Failed Fold")
         nil)

       :else
       (recur pattern required-smudge fold (inc offset) (+ diff smudge-count))))))

(defn test-view-folds [{:keys [rows columns row-folds column-folds]} required-smudge]
  (let [row-reflects (remove nil? (map (partial test-reflection rows required-smudge) row-folds))
        column-reflects (remove nil? (map (partial test-reflection columns required-smudge) column-folds))]
    {:rows rows :columns columns
     :row-folds row-folds :column-folds column-folds
     :row-reflects row-reflects :column-reflects column-reflects}))

(defn find-folds
  ([pattern smudge] (find-folds pattern smudge 0 []))
  ([[curr & rest] smudge i folds]
   (cond
     (or (nil? curr) (nil? (first rest))) folds
     (>= smudge (difference-max curr (first rest) smudge)) (recur rest smudge (inc i) (conj folds i))
     :else (recur rest smudge (inc i) folds))))

(defn find-view-folds [{:keys [rows columns]} smudge]
  (let [row-folds (find-folds rows smudge)
        column-folds (find-folds columns smudge)]
    {:rows rows :columns columns
     :row-folds row-folds :column-folds column-folds}))

(defn views [data]
  {:rows (mapv vec data) :columns (apply mapv vector data)})

(defn group-lines
  ([lines]
   (group-lines lines []))
  ([lines patterns]
   (if (empty? lines)
     patterns
     (recur (rest (drop-while not-empty lines))
            (conj patterns (vec (take-while not-empty lines)))))))

(defn score-pattern [{:keys [row-reflects column-reflects]}]
  (+ (reduce + (map inc column-reflects))
     (reduce + (map #(* 100 (inc %)) row-reflects))))

(defn load-data [filename]
  (->> filename
       common/read-input
       group-lines
       (map views)))


(defn p1 [filename smudges]
  (let [views (load-data filename)
        folds (map #(find-view-folds % smudges) views)
        reflections (map #(test-view-folds % smudges) folds)]
    reflections
    (map-indexed (fn [idx m] [idx (+ (count (:row-reflects m))
                          (count (:column-reflects m)))]) reflections)
    (reduce + (map score-pattern reflections))))
