(ns aoc-2023.day-7
  (:require [aoc-2023.common :as common]))

(def test-input "problems/day-7-1-t.txt")
(def test-all-hands "problems/day-7-1-t-all-hands.txt")
(def test-jokers "problems/day-7-1-t-jokers.txt")
(def test-jokers2 "problems/day-7-1-t-jokers2.txt")
(def p1-input "problems/day-7-1.txt")


;; A, K, Q, J, T, 9, 8, 7, 6, 5, 4, 3, 2
(defn to-int [part ^Character c]
  (cond
    (= \A c) 14
    (= \K c) 13
    (= \Q c) 12
    (= \J c) (if (= "p1" part) 11 1)
    (= \T c) 10
    :else (Character/digit c 10)))

(defn identify-hand-by-counts [card-counts]
  (cond
    (contains? card-counts 5)
    {:type :five-of-a-kind :type-val 7}

    (contains? card-counts 4)
    {:type :four-of-a-kind :type-val 6}

    (or (and (contains? card-counts 3)
             (contains? card-counts 2)
             ;; weird special case for jokers
             (= 1 (count (card-counts 2))))
        ;; special case for jokers
        (= 2 (count (card-counts 3))))
    {:type :full-house :type-val 5}

    (contains? card-counts 3)
    {:type :three-of-a-kind :type-val 4}

    (= 2 (count (card-counts 2)))
    {:type :two-pair :type-val 3}

    ;; not equals because of jokers
    (or (< 0 (count (card-counts 2))))
    {:type :one-pair :type-val 2}

    :else
    {:type :high-card :type-val 1}))

(defn identify-hand-jokers [hand]
  (let [jokers (count (filter #(= 1 %) (:cards hand)))
        card-counts (group-by count (vals (dissoc (group-by identity (:cards hand)) 1)))
        with-jokers (into {} (map (fn [[k v]] {(+ jokers k) v}) card-counts))]
    (identify-hand-by-counts with-jokers)))

(defn identify-hand-natural [hand]
  (let [card-counts (group-by count (vals (group-by identity (:cards hand))))]
    (identify-hand-by-counts card-counts)))

(defn rank-hands [hands]
  (->> hands
       (sort-by :cards)
       (sort-by :type-val)))

(defn parse-line [part line]
  (let [[cards bid] (re-seq #"\w+" line)
        hand {:cards (mapv (partial to-int part) cards) :bid (Integer/parseInt bid)}
        natural-identity (identify-hand-natural hand)
        joker-identity (identify-hand-jokers hand)]
    (merge hand (max-key :type-val natural-identity joker-identity))))

(defn load-data [part filename]
  (->> filename
       common/read-input
       (map (partial parse-line part))))

(defn p1 [filename]
  (->> filename
       (load-data "p1")
       rank-hands
       (map-indexed (fn [i hand] (* (inc i) (:bid hand))))
       (reduce +)))

(defn p2 [filename]
  (->> filename
       (load-data "p2")
       rank-hands
       (map-indexed (fn [i hand] (* (inc i) (:bid hand))))
       (reduce +)))
