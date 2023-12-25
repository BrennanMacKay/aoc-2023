(ns aoc-2023.day-25
  (:require [aoc-2023.common :as common])
  (:import org.jgrapht.graph.DefaultUndirectedGraph
           org.jgrapht.graph.DefaultEdge
           org.jgrapht.alg.StoerWagnerMinimumCut))

(def test-input "problems/day-25-1-t.txt")
(def p1-input "problems/day-25-1.txt")

(defn parse-lines [lines]
  (reduce (fn [graph line]
            (let [[node & edges] (re-seq #"\w+" line)
                  graph (reduce (fn [graph [node edge]]
                                  (update graph node conj edge))
                                graph (map #(vector % node) edges))
                  graph (assoc graph node edges)]
              graph)) {} lines))

(defn load-data [filename]
  (-> filename
      common/read-input
      parse-lines))

(defn p1 [filename]
  (let [graph (load-data filename)
        jgraph (DefaultUndirectedGraph. DefaultEdge)]
    (doseq [node (keys graph)]
      (.addVertex jgraph node))
    (doseq [[node edges] graph]
      (doseq [edge edges]
        (.addEdge jgraph node edge)))
    (let [alg (StoerWagnerMinimumCut. jgraph)
          cut (.minCut alg)
          _ (.removeAllVertices jgraph cut)
          s1 (.. jgraph vertexSet size)
          s2 (count cut)]
      (* s1 s2))))


