(ns aoc-2023.day-10
  (:require [aoc-2023.common :as common]
            [clojure.string :as string]))

(def test-input-1 "problems/day-10-1-t-1.txt")
(def test-input-2 "problems/day-10-1-t-2.txt")
(def test-input-3 "problems/day-10-1-t-3.txt")
(def test-input-4 "problems/day-10-1-t-4.txt")
(def test-input-5 "problems/day-10-1-t-5.txt")

(def p1-input "problems/day-10-1.txt")


;| is a vertical pipe connecting north and south.
;- is a horizontal pipe connecting east and west.
;L is a 90-degree bend connecting north and east.
;J is a 90-degree bend connecting north and west.
;7 is a 90-degree bend connecting south and west.
;F is a 90-degree bend connecting south and east.
(def actions {\| [[0 1] [0 -1]]
              \- [[1 0] [-1 0]]
              \L [[1 0] [0 -1]]
              \J [[-1 0] [0 -1]]
              \7 [[-1 0] [0 1]]
              \F [[1 0] [0 1]]})

(def vertical-pipes #{\| \L \J #_\7 #_\F \S})

(defn char-at [{:keys [mx my world]} [x y]]
  (if (and (<= 0 x mx)
           (<= 0 y my))
    (aget world y x)
    \.))

(defn add [[x y] [xv yv]]
  [(+ x xv) (+ y yv)])

(defn pipe? [v] (contains? actions v))

(defn connected? [world curr next]
  (let [next-pipe (char-at world next)]
    (if (pipe? next-pipe)
      (contains? (into #{} (map (partial add next) (actions next-pipe)))
                 curr)
      false)))


(defn adjacent [[x y]]
  [[x (inc y)]
   [(inc x) y]
   [x (dec y)]
   [(dec x) y]])

(defn update-visited [visited pipe [x y]]
  (update visited y #(conj % {:x x :pipe pipe})))

(defn follow [world curr-pos pipe-pos step visited]
  (let [pipe (char-at world pipe-pos)
        visited (update-visited visited pipe pipe-pos)
        possible (map (partial add pipe-pos) (actions pipe))
        next-pos (first (remove #(= curr-pos %) possible))
        next-pipe (char-at world next-pos)]
    (cond
      (= \S next-pipe) {:steps (inc step) :visited visited}
      (not (pipe? next-pipe)) nil
      :else (recur world pipe-pos next-pos (inc step) visited))))

(defn init-visited [y]
  (into {} (map (fn [i] [i []]) (range y))))

(defn follow-from-start [world start-pos [curr-test & rest] visited]
  (if (nil? curr-test)
    nil
    (let [steps (follow world start-pos curr-test 1 (update-visited visited \S start-pos))]
      (cond
        (nil? steps) (recur world start-pos rest visited)
        :else steps))))


(defn find-start [input]
  (->> input
       (map-indexed (fn [y v]
                      (let [x (string/index-of v \S)]
                        (when x [x y]))))
       (remove nil?)
       first))

(defn load-data [filename]
  (->> filename
       common/read-input))

(defn p1 [filename]
  (let [input (load-data filename)
        max-x (dec (count (first input)))
        max-y (dec (count input))
        start (find-start input)
        world-array {:mx max-x :my max-y :world (to-array-2d input)}
        adjs (filter #(connected? world-array start %) (adjacent start))
        visited (init-visited (count input))]
    (/ (:steps (follow-from-start world-array start adjs visited)) 2)))

(defn enclosed-count [min max exclude]
  (count (filter #(not (contains? exclude %)) (range (inc min) max))))

(defn contains
  ([[y visited-line]]
   (let [exclude (or (set (map :x (get visited-line false))) #{})
         vals (or (sort (map :x (get visited-line true))) [])]
     (when (not (= 0 (mod (count vals) 2)))
       (throw (ex-info "vals not even" {:vals vals})))
     (contains vals exclude 0)))
  ([[a b & rest] exclude enclosed]
   (cond
     (and (nil? a) (nil? b)) enclosed
     :else
     (let [enclosed-in-pair (enclosed-count a b exclude)
           enclosed (+ enclosed enclosed-in-pair)]
       (recur rest exclude enclosed)))))

(defn p2 [filename]
  (let [input (load-data filename)
        max-x (dec (count (first input)))
        max-y (dec (count input))
        start (find-start input)
        world-array {:mx max-x :my max-y :world (to-array-2d input)}
        adjs (filter #(connected? world-array start %) (adjacent start))
        visited (init-visited (count input))
        visited (:visited (follow-from-start world-array start adjs visited))
        verticals (sort (map (fn [[k v]] [k (group-by #(contains? vertical-pipes (:pipe %)) v)]) visited))]
    (reduce + (map contains verticals))))