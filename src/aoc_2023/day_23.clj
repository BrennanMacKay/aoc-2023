(ns aoc-2023.day-23
  (:require [aoc-2023.common :as common]))

(def test-input "problems/day-23-1-t.txt")
(def p1-input "problems/day-23-1.txt")

(defn parse-line
  ([y line]
   (parse-line 0 y line {}))
  ([x y [curr & rest] paths]
   (if (nil? curr)
     paths
     (let [paths (if (= \# curr)
                   paths
                   (assoc paths [x y] curr))]
       (recur (inc x) y rest paths)))))

(defn adj [x y]
  [[(inc x) y]
   [(dec x) y]
   [x (inc y)]
   [x (dec y)]])

(defn locate-forks
  ([paths max]
   (locate-forks paths max paths []))
  ([paths max [curr & rest] forks]
   (if (nil? curr)
     forks
     (let [[[x y] _] curr
           neighbors (count (remove nil? (map #(paths %) (adj x y))))
           forks (if (or (< 2 neighbors)
                         (= 0 y)
                         (= max y))
                   (conj forks [x y])
                   forks)]
       (recur paths max rest forks)))))

(defn can-pass? [path dx dy part]
  (if (= :part-2 part)
    true
    (case path
      \. true
      \> (pos-int? dx)
      \< (neg-int? dx)
      \v (pos-int? dy)
      \^ (neg-int? dy))))

(defn follow-path-to-fork
  ([paths forks x y dx dy part]
   (follow-path-to-fork paths forks x y dx dy 1 part))
  ([paths forks x y dx dy distance part]
   ;(prn [x y] [dx dy] distance)
   (let [maybe-fork (forks [x y])]
     (if maybe-fork
       [maybe-fork distance]
       (let [[prev-x prev-y] [(- x dx) (- y dy)]
             ;_ (prn "PREV" [prev-x prev-y])
             [next-x next-y] (first (filter #(contains? paths %) (remove #(= [prev-x prev-y] %) (adj x y))))
             ;_ (prn "NEXT" [next-x next-y])
             [next-dx next-dy] [(- next-x x) (- next-y y)]
             next (paths [next-x next-y])]
         (if (can-pass? next next-dx next-dy part)
           (recur paths forks next-x next-y next-dx next-dy (inc distance) part)
           nil))))))

(defn paths-for-fork [paths forks [x y] part]
  (let [paths-to-check (filter #(contains? paths %) (adj x y))
        neighbors (into {} (map (fn [[next-x next-y]]
                                  (follow-path-to-fork paths forks next-x next-y (- next-x x) (- next-y y) part))
                                paths-to-check))]
    [(forks [x y]) neighbors]))

(defn build-graph [paths forks part]
  (into {} (map (fn [[pos _]] (paths-for-fork paths forks pos part)) forks)))

(defn load-data [filename part]
  (let [raw-input (common/read-input filename)
        paths (apply merge (map-indexed parse-line raw-input))
        max-pos (dec (count raw-input))
        raw-forks (locate-forks paths max-pos)
        forks (into {} (map-indexed #(vector %2 %1) raw-forks))
        graph (build-graph paths forks part)]
    {:start (forks [1 0])
     :end   (forks [(dec max-pos) max-pos])
     :forks forks
     :graph graph}))

(defn traverse
  ([graph start end]
   (traverse graph start end '() 0 [] #{}))
  ([graph curr end stack distance distances visited]
   ;(prn curr end stack distance distances visited)
   (if (= curr end)
     (let [distances (conj distances distance)
           next (peek stack)]
       (if (nil? next)
         distances
         (recur graph (first next) end (pop stack) (second next) distances (last next))))
     (let [neighbors (graph curr)
           ;_ (prn neighbors)
           unvisited-neighbors (remove (fn [[k _]] (contains? visited k)) neighbors)
           stack (reduce #(conj %1 [(first %2)
                                    (+ distance (second %2))
                                    (conj visited (first %2))]) stack unvisited-neighbors)
           next (peek stack)]
       (if (nil? next)
         distances
         (recur graph (first next) end (pop stack) (second next) distances (last next)))))))

(defn p1 [filename part]
  (let [{:keys [start end forks graph] :as data} (load-data filename part)]
    (apply max (traverse graph start end))))
