(ns aoc-2023.day-20
  (:require [aoc-2023.common :as common]
            [clojure.math.numeric-tower :as nt]
            [clojure.string :as string]))


(def test-input-1 "problems/day-20-1-t-1.txt")
(def test-input-2 "problems/day-20-1-t-2.txt")
(def p1-input "problems/day-20-1.txt")

(defn parse-module [line]
  (let [[module & dests] (string/split line #"( -> )|(, )")
        type-char (first module)
        type (case type-char
               \% :flip
               \& :conj
               \b :broad
               (throw (ex-info "Unknown type" {:module module :type type-char})))
        module-id (if (= type :broad) module (apply str (rest module)))]
    [module-id {:type  type
                :dests (vec dests)}]))

(defn build-callers [modules]
  (apply merge-with into (map (fn [[k v]]
                      (into {} (map #(vector % #{k}) (:dests v)))) modules)))

(defn init-state [id {:keys [type]} callers]
  [id (case type
        :flip false
        :conj (into {} (map #(vector % :low) (callers id)))
        {})])

(defn load-data [filename]
  (let [modules (->> filename
                     common/read-input
                     (mapv parse-module)
                     (into {}))
        callers (build-callers modules)
        states (into {} (map (fn [[k v]] (init-state k v callers)) modules))]
    [modules {:low-count 0 :high-count 0 :output {} :states states}]))

(defn handle-flip [state module-id signal]
  (let [on? (get-in state [:states module-id])]
    (cond
      (= signal :high)
      [state :off]

      (and (= signal :low)
           (not on?))
      [(assoc-in state [:states module-id] true) :high]

      (and (= signal :low)
           on?)
      [(assoc-in state [:states module-id] false) :low])))

(defn handle-conj [state module-id signal caller-id]
  (let [state (assoc-in state [:states module-id caller-id] signal)
        all-high? (not (some #{:low} (vals (get-in state [:states module-id]))))]
    (if all-high?
      [state :low]
      [state :high])))

(defn press-button
  ([modules state part looking-for]
   (press-button modules state "broadcaster" :low nil (common/queue) part looking-for))
  ([modules state module-id signal caller-id queue part looking-for]
   (let [module (modules module-id)
         next (peek queue)
         state (if (= signal :low)
                 (update state :low-count inc)
                 (update state :high-count inc))]
     (if (nil? module)
       (let [state (update-in state [:output module-id] #(inc (or % 0)))]
         (if (nil? next)
           [state false]
           (recur modules state (:id next) (:signal next) (:caller-id next) (pop queue) part looking-for)))

       (let [{:keys [type dests]} module
             [state queue-adds]
             (case type
               :broad [state (map #(hash-map :id % :signal :low :caller-id module-id) dests)]
               :flip (let [[state signal] (handle-flip state module-id signal)]
                       [state
                        (if (= signal :off)
                          []
                          (map #(hash-map :id % :signal signal :caller-id module-id) dests))])
               :conj (let [[state signal] (handle-conj state module-id signal caller-id)]
                       [state (map #(hash-map :id % :signal signal :caller-id module-id) dests)]))
             queue (reduce conj queue queue-adds)
             next (peek queue)]
         (if (nil? next)
           [state false]
           (if (= part :part-2)
             (if (and (= looking-for (:caller-id next)) (= :high (:signal next)))
               (do (prn "FOUND!" next)
                 [state true])
               (recur modules state (:id next) (:signal next) (:caller-id next) (pop queue) part looking-for))
             (recur modules state (:id next) (:signal next) (:caller-id next) (pop queue) part looking-for))))))))

(defn p1 [filename]
  (let [[modules state] (load-data filename)
        result (reduce (fn [state _] (first (press-button modules state :part-1 nil))) state (range 1000))]
    [(* (:low-count result) (:high-count result)) result]))

;rx is called by df which is called by &xl &ln &xp &gp
; so we need the LCM of when each of these first sends a high (or maybe their cycle)
(defn p2 [filename]
  (let [[modules state] (load-data filename)
        xl (reduce (fn [state step] (let [[state done?] (press-button modules state :part-2 "xl")]
                                      (if done?
                                        (reduced (inc step))
                                        state))) state (range))
        ln (reduce (fn [state step] (let [[state done?] (press-button modules state :part-2 "ln")]
                                      (if done?
                                        (reduced (inc step))
                                        state))) state (range))
        xp (reduce (fn [state step] (let [[state done?] (press-button modules state :part-2 "xp")]
                                      (if done?
                                        (reduced (inc step))
                                        state))) state (range))
        gp (reduce (fn [state step] (let [[state done?] (press-button modules state :part-2 "gp")]
                                      (if done?
                                        (reduced (inc step))
                                        state))) state (range))]
    [xl ln xp gp (reduce nt/lcm [xl ln xp gp])]))
