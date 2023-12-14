(ns aoc-2023.day-14
  (:require [aoc-2023.common :as common]))

(def test-input "problems/day-14-1-t.txt")
(def p1-input "problems/day-14-1.txt")

(defn add-obstruction [state x y]
  (-> state
      (update-in [:obstructions-column x] #(set (conj % y)))
      (update-in [:obstructions-row y] #(set (conj % x)))))

(defn add-rock [state x y id]
  (-> state
      #_(update-in [:rocks-x x] #(set (conj % y)))
      #_(update-in [:rocks-y y] #(set (conj % x)))
      (update :rocks #(assoc % id [x y]))))

#_(defn remove-rock [state id]
    (let [[x y] (get-in state [:rocks id])]
      (-> state
          (update-in [:rocks-x x] #(disj % y))
          (update-in [:rocks-y y] #(disj % x)))))

(def calc-pos (memoize (fn [obstructions a direction size]
                         ;(clojure.pprint/pprint [obstructions a])
                         (let [result (if (contains? obstructions a)
                                        ;; move back a step if our current position is obstructed
                                        (cond
                                          (= -1 direction) (calc-pos obstructions (inc a) direction size)
                                          (= 1 direction) (calc-pos obstructions (dec a) direction size))
                                        (cond
                                          (= -1 direction) (inc (apply max (conj (filter #(contains? obstructions %) (range 0 a)) -1)))
                                          (= 1 direction) (dec (apply min (conj (filter #(contains? obstructions %) (range a size)) size)))))]
                           ;(clojure.pprint/pprint ["dest" result])
                           result))))

(defn move-rock [state id x-dir y-dir]

  (let [[x y] (get-in state [:rocks id])
        #_#__ (clojure.pprint/pprint ["move" id x y (get-in state [:obstructions-column x])])
        [new-x new-y] (cond
                        (= 0 y-dir)
                        [(calc-pos (get-in state [:obstructions-row y])
                                   x x-dir (:size state)) y]

                        (= 0 x-dir)
                        [x (calc-pos (get-in state [:obstructions-column x])
                                     y y-dir (:size state))])]
    (-> state
        (add-rock new-x new-y id)
        (add-obstruction new-x new-y))))

(defn move-rocks
  ([state x-dir y-dir]
   ;(clojure.pprint/pprint state)
   (let [original-state state
         final-state (move-rocks state (keys (:rocks state)) x-dir y-dir)]
     (assoc final-state :obstructions-row (:obstructions-row original-state)
                        :obstructions-column (:obstructions-column original-state))))
  ([state [curr & rest] x-dir y-dir]
   (if (nil? curr)
     state
     (recur (move-rock state curr x-dir y-dir) rest x-dir y-dir))))

(defn score-rock [[x y] size]
  (- size y))

(defn score-rocks [{:keys [rocks size]}]
  (map #(score-rock % size) (vals rocks)))

(defn parse-line
  ([line y id state]
   (parse-line line 0 y id state))
  ([[curr & rest] x y id state]
   (cond
     (nil? curr) state
     (= \. curr) (recur rest (inc x) y id state)
     (= \# curr) (recur rest (inc x) y id (add-obstruction state x y))
     (= \O curr) (recur rest (inc x) y (inc id) (add-rock state x y id)))))

(defn parse-state
  ([lines]
   (parse-state lines 0 0 {:obstructions-column {}
                           :obstructions-row {}
                           #_#_:rocks-x        {}
                           #_#_:rocks-y        {}
                           :rocks          {}
                           :size           (count lines)}))
  ([[curr & rest] y id state]
   (if (nil? curr)
     state
     (let [state (parse-line curr y id state)]
       (recur rest (inc y) (count (:rocks state)) state)))))

(defn load-data [filename]
  (->> filename
       common/read-input
       parse-state))

(defn p1 [filename]
  (reduce + (-> filename
                load-data
                (move-rocks 0 -1)
                score-rocks)))

(def rotate (memoize (fn [state]
                       (-> state
                           (move-rocks 0 -1)
                           (move-rocks -1 0)
                           (move-rocks 0 1)
                           (move-rocks 1 0)))))

(defn do-x-times [f state times]
  (prn times (reduce + (score-rocks state)))
  (if (<= times 0)
    state
    (recur f (f state) (dec times))))

(defn p2 [filename iters]
  (let [initial-state (load-data filename)
        after-x (do-x-times rotate initial-state iters)]
    (reduce + (score-rocks after-x))))
