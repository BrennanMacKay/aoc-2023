(ns aoc-2023.day-17
  (:require [aoc-2023.common :as common]
            [clojure.data.priority-map :as pmap]))

(def test-input "problems/day-17-1-t.txt")
(def small-input "problems/day-17-1-t-1.txt")
(def p2-test-input "problems/day-17-1-t-2.txt")
(def p1-input "problems/day-17-1.txt")


(defn neighbors-p1 [x y dx dy bx by steps]
  (let [x1 (+ x dx)
        y1 (+ y dy)
        xr (+ x dy)
        yr (+ y dx)
        xl (+ x (* -1 dy))
        yl (+ y (* -1 dx))]

    (if (= dx dy)
      ;; special case for starting
      [{:x 1 :y 0 :dx 1 :dy 0 :steps (inc steps)}
       {:x 0 :y 1 :dx 0 :dy 1 :steps (inc steps)}]
      (as-> [] nbs
            (if (< steps 3)
              (conj nbs {:x x1 :y y1 :dx dx :dy dy :steps (inc steps) :past []})
              nbs)
            ;; turn left or turn right
            (conj nbs {:x xr :y yr :dx dy :dy dx :steps 1 :past []}
                  {:x xl :y yl :dx (* -1 dy) :dy (* -1 dx) :steps 1 :past []})
            ;; filter out of bound neighbors
            (filter (fn [{:keys [x y]}]
                      (and (<= 0 x bx)
                           (<= 0 y by))) nbs)))))

(defn neighbors-p2 [x y dx dy bx by steps]
  (let [x1 (+ x (* 1 dx))
        y1 (+ y (* 1 dy))
        x2 (+ x (* 2 dx))
        y2 (+ y (* 2 dy))
        x3 (+ x (* 3 dx))
        y3 (+ y (* 3 dy))
        x4 (+ x (* 4 dx))
        y4 (+ y (* 4 dy))
        xr (+ x dy)
        yr (+ y dx)
        xr2 (+ xr dy)
        yr2 (+ yr dx)
        xr3 (+ xr2 dy)
        yr3 (+ yr2 dx)
        xr4 (+ xr3 dy)
        yr4 (+ yr3 dx)
        xl (+ x (* -1 dy))
        yl (+ y (* -1 dx))
        xl2 (+ xl (* -1 dy))
        yl2 (+ yl (* -1 dx))
        xl3 (+ xl2 (* -1 dy))
        yl3 (+ yl2 (* -1 dx))
        xl4 (+ xl3 (* -1 dy))
        yl4 (+ yl3 (* -1 dx))
        nbs (if (= dx dy)
              [{:x 4 :y 0 :dx 1 :dy 0 :steps 4 :past [[1 0] [2 0] [3 0]]}
               {:x 0 :y 4 :dx 0 :dy 1 :steps 4 :past [[0 1] [0 2] [0 3]]}]
              (cond
                (< steps 4) [{:x x4 :y y4 :dx dx :dy dy :steps 4 :past [[x1 y1] [x2 y2] [x3 y3]]}]
                (< steps 10) [{:x x1 :y y1 :dx dx :dy dy :steps (inc steps) :past []}
                              {:x xr4 :y yr4 :dx dy :dy dx :steps 4 :past [[xr yr] [xr2 yr2] [xr3 yr3]]}
                              {:x xl4 :y yl4 :dx (* -1 dy) :dy (* -1 dx) :steps 4 :past [[xl yl] [xl2 yl2] [xl3 yl3]]}]
                (= steps 10) [{:x xr4 :y yr4 :dx dy :dy dx :steps 4 :past [[xr yr] [xr2 yr2] [xr3 yr3]]}
                              {:x xl4 :y yl4 :dx (* -1 dy) :dy (* -1 dx) :steps 4 :past [[xl yl] [xl2 yl2] [xl3 yl3]]}]))]
    (filter (fn [{:keys [x y]}]
              (and (<= 0 x bx)
                   (<= 0 y by))) nbs)))

(defn entry [grid curr-dist {:keys [x y dx dy steps past]}]
  (let [past-distance (reduce + (map (fn [[x y]] (aget grid x y)) past))
        distance (+ curr-dist (aget grid x y) past-distance)]
    [[x y dx dy steps] distance]))

(defn add-or-update [queue [curr & rest]]
  (if (nil? curr)
    queue
    (let [[k distance] curr
          curr-distance (queue k)
          next-queue (if (and curr-distance
                              (< curr-distance distance))
                       queue
                       (do
                         ;(prn "UPDATE" k distance)
                         (assoc queue k distance)))]
      (recur next-queue rest))))

(defn filter-visited [visited entries]
  (filter (fn [{:keys [x y dx dy steps]}]
            (not (contains? visited [x y dx dy steps]))) entries))

(defn dijkstras [grid x y dist dx dy bx by queue visited steps dest-x dest-y nbs-f]
  (let [nbs (nbs-f x y dx dy bx by steps)
        filtered (filter-visited visited nbs)
        entries (mapv #(entry grid dist %) filtered)
        queue (-> queue
                  (add-or-update entries)
                  (dissoc [x y dx dy]))
        [[next-x next-y next-dx next-dy next-steps] next-dist] (peek queue)]
    (cond
      (and (= x dest-x) (= y dest-y)) dist
      (contains? visited [x y dx dy steps]) (throw (ex-info "We hit a visited?" {:x x :y y :visited visited}))
      :else (recur grid next-x next-y next-dist next-dx next-dy bx by (pop queue) (conj visited [x y dx dy steps]) next-steps dest-x dest-y nbs-f)))
  )

(defn load-data [filename]
  (let [input (common/read-input filename)
        xb (dec (count (first input)))
        yb (dec (count input))]
    {:xb   xb
     :yb   yb
     :grid (->> input
                (map (fn [row] (map #(- (int %) 48) row)))
                (apply map list)
                to-array-2d)}
    ))

(defn p1 [filename]
  (let [{:keys [xb yb grid]} (load-data filename)]
    (dijkstras grid 0 0 0 0 0 xb yb (pmap/priority-map) #{} 0 xb yb neighbors-p1)))

(defn p2 [filename]
  (let [{:keys [xb yb grid]} (load-data filename)]
    (dijkstras grid 0 0 0 0 0 xb yb (pmap/priority-map) #{} 0 xb yb neighbors-p2)))
