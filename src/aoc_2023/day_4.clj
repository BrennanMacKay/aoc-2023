(ns aoc-2023.day-4
  (:require [aoc-2023.common :as common]
            [clojure.string :as string]
            [clojure.math :as math]))

(def test-input "problems/day-4-1-t.txt")
(def p1-input "problems/day-4-1.txt")

(def exp #"Card\s+(\d+):\s+(\d+\s+)+\|\s+(\d+\s+)+")

(defn update-game-count [games game-id count]
  (if (contains? games game-id)
    (update-in games [game-id :count] + count)
    games))

(defn update-game-counts [games [curr & rest] count]
  (if (nil? curr)
    games
    (recur (update-game-count games curr count) rest count)))

(defn propagate-wins
  ([games]
   (propagate-wins games 1))
  ([games curr-id]
   (let [game (games curr-id)]
     (if (or (nil? game))
       games
       (let [wins (:wins game)
             count (:count game)
             dest (range (inc curr-id) (+ (inc curr-id) wins))
             updated-games (update-game-counts games dest count)]
         (recur updated-games (inc curr-id)))))))

(defn score [{:keys [wins]}]
  (cond
    (= 0 wins) 0
    (= 1 wins) 1
    :else (math/pow 2 (- wins 1))))

(defn win-count [{:keys [winners played] :as game}]
  (assoc game :wins (count (filter (partial contains? winners) played))))

(defn parse-line [line]
  (let [[game-s win-s played-s] (string/split line #":|\|")
        game-id (Integer/parseInt (re-find #"\d+" game-s))
        win (set (map #(Integer/parseInt %) (re-seq #"\d+" win-s)))
        played (map #(Integer/parseInt %) (re-seq #"\d+" played-s))]
    [game-id {:game-id game-id :count 1 :winners win :played played}]))

(defn load-data [filename]
  (common/read-input filename))

(defn p1 [filename]
  (->> (load-data filename)
       (map parse-line)
       (map last)
       (map win-count)
       (map score)
       (apply +)))

(defn p2 [filename]
  (->> (load-data filename)
       (map parse-line)
       (map (fn [[k v]] [k (win-count v)]))
       (into {})
       propagate-wins
       vals
       (map :count)
       (reduce +)))
