(ns aoc-2023.common
  (:require [clojure.java.io :as io]
            [clojure.string :as string])
  (:import (clojure.lang PersistentQueue)
           (java.io Writer)))

(defn read-input [file-name]
  (-> file-name
      (io/resource)
      (slurp)
      (string/split-lines)))

(defn queue
  ([] (PersistentQueue/EMPTY))
  ([coll]
   (reduce conj PersistentQueue/EMPTY coll)))

(defmethod print-method PersistentQueue
  [q ^Writer w]
  (.write w "#queue ")
  (print-method (sequence q) w))


(defn draw-grid [grid xmin ymin xmax ymax]
  (let [grid (set grid)]
    (mapv (fn [y]
            (prn (apply str
                        (map (fn [x]
                               (if (contains? grid [x y]) \# \.))
                             (range xmin (inc xmax))))))
          (range ymin (inc ymax)))))