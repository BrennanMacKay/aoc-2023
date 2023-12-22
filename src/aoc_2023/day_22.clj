(ns aoc-2023.day-22
  (:require [aoc-2023.common :as common]))


(def test-input "problems/day-22-1-t.txt")
(def p1-input "problems/day-22-1.txt")

; [id {:points [[x1 y1 z1]] :min-z z}]
; supports [id [id id id]]
; supported-by [id [id id id]]
; world [[x y z] id]

(defn expand-points [x1 y1 z1 x2 y2 z2]
  (cond
    (not= x1 x2) (mapv #(vector % y1 z1) (range (min x1 x2) (inc (max x1 x2))))
    (not= y1 y2) (mapv #(vector x1 % z1) (range (min y1 y2) (inc (max y1 y2))))
    (not= z1 z2) (mapv #(vector x1 y1 %) (range (min z1 z2) (inc (max z1 z2))))
    :else [[x1 y1 z1]]))

(defn parse-line [id line]
  (let [[x1 y1 z1 x2 y2 z2] (map #(Integer/parseInt %) (re-seq #"\d+" line))
        points (expand-points x1 y1 z1 x2 y2 z2)
        min-z (apply min (map last points))]
    [id {:points points :min-z min-z}]))

(defn load-data [filename]
  (->> filename
       common/read-input
       (map-indexed parse-line)
       (sort-by #(:min-z (second %)))))

(defn settle [world blocks supporting supported-by id points]
  (let [supports (distinct (remove nil? (map (fn [[x y z]]
                                               (or
                                                 (when (= 0 (dec z)) :ground)
                                                 (world [x y (dec z)])))
                                             points)))]
    (if (not-empty supports)
      {:world        (reduce #(assoc %1 %2 id) world points)
       :blocks       (assoc blocks id points)
       :supporting   (reduce #(update %1 %2 conj id) supporting supports)
       :supported-by (reduce #(update %1 id conj %2) supported-by supports)}
      (recur world blocks supporting supported-by id (mapv (fn [[x y z]] [x y (dec z)]) points)))))

(defn settle-blocks
  ([blocks]
   (settle-blocks {} {} {} {} blocks))
  ([world settled-blocks supporting supported-by [curr & rest]]
   (if (nil? curr)
     {:world        world
      :blocks       settled-blocks
      :supporting   supporting
      :supported-by supported-by}
     (let [[id {:keys [points]}] curr
           {:keys [world blocks supporting supported-by]}
           (settle world settled-blocks supporting supported-by id points)]
       (recur world blocks supporting supported-by rest)))))

(defn disintegrate? [id supporting supported-by]
  (let [supports (supporting id)
        unsupported (count (filter #(= 0 %)
                                   (map (fn [supported]
                                          (count
                                            (filter #(not= id %)
                                                    (supported-by supported))))
                                        supports)))]
    (= 0 unsupported)))

(defn disintegrate-count
  ([id supporting supported-by]
   (disintegrate-count id supporting supported-by #{} (common/queue)))
  ([id supporting supported-by falling to-check]
   (let [falling (conj falling id)
         supports (supporting id)
         no-supports (remove nil? (map (fn [brick]
                                         (when (= 0 (count (filter #(not (contains? falling %))
                                                                   (supported-by brick))))
                                           brick))
                                       supports))
         falling (reduce conj falling no-supports)
         to-check (reduce conj to-check no-supports)
         next (peek to-check)]
     (if (nil? next)
       (dec (count falling))
       (recur next supporting supported-by falling (pop to-check))))))


(defn p1 [filename]
  (let [input (load-data filename)
        {:keys [world blocks supporting supported-by]} (settle-blocks input)]
    (count (remove false? (map #(disintegrate? % supporting supported-by) (keys blocks))))))

(defn p2 [filename]
  (let [input (load-data filename)
        {:keys [world blocks supporting supported-by]} (settle-blocks input)]
    (reduce + (map #(disintegrate-count % supporting supported-by) (keys blocks)))))
