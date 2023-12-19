(ns aoc-2023.day-19
  (:require [aoc-2023.common :as common]
            [clojure.string :as string]))

(def test-input "problems/day-19-1-t.txt")
(def p1-input "problems/day-19-1.txt")

(defn parse-parts [[curr & rest] parts]
  (if (nil? curr)
    parts
    (let [[x m a s] (map #(Integer/parseInt %) (re-seq #"\d+" curr))]
      (recur rest (conj parts {:x x :m m :a a :s s})))))

(defn parse-rules
  ([rule-string]
   (parse-rules (string/split rule-string #",") []))
  ([[curr & rest] rules]
   (if (nil? curr)
     rules
     (let [stmt (re-matches #"(\w+)(<|>)(\d+):(\w+)" curr)]
       (if (nil? stmt)
         (recur rest (conj rules {:type :pass :dest curr}))
         (let [[_ var cond val dest] stmt
               cond (case cond "<" <
                               ">" >)
               val (Integer/parseInt val)]
           (recur rest (conj rules {:type :cond :var (keyword var)
                                    :cond cond :val val :dest dest}))))))))

(defn parse-lines
  ([lines]
   (parse-lines lines []))
  ([[curr & rest] workflows]
   (if (empty? curr)
     [(into {} workflows) (parse-parts rest [])]
     (let [[_ workflow-id rule-string] (re-matches #"(\w+)\{(.*)\}" curr)]
       (recur rest (conj workflows [workflow-id (parse-rules rule-string)]))))))

(defn load-data [filename]
  (-> filename
      common/read-input
      parse-lines))

(defn process-rules [[{:keys [type dest var cond val]} & rest] part]
  (if (= :pass type)
    dest
    (if (cond (part var) val)
      dest
      (recur rest part))))

(defn part-destination
  ([workflows part]
   (part-destination workflows "in" part))
  ([workflows workflow-id part]
   (case workflow-id
     "A" part
     "R" nil
     (let [rules (workflows workflow-id)
           next (process-rules rules part)]
       (recur workflows next part)))))

(defn p1 [filename]
  (let [[workflows parts] (load-data filename)
        processed (remove nil? (map (partial part-destination workflows) parts))]
    (reduce + (flatten (map vals processed)))))

(defn new-low-high [[l h] cond val]
  (let [dest-hl (if (= < cond)
                  [l (min h (dec val))]
                  [(max l (inc val)) h])
        next-hl (if (= < cond)
                  [(max l val) h]
                  [l (min h val)])]
    [dest-hl next-hl]))

(defn process-rules-p2 [[{:keys [type dest var cond val]} & rest] part dests]
  (case type
    :pass
    (conj dests [dest part])

    :cond
    (let [[[dl dh] [ol oh]] (new-low-high (part var) cond val)
          part-for-dest (assoc part var [dl dh])
          part-for-next (assoc part var [ol oh])
          dests (if (< dl dh) (conj dests [dest part-for-dest]) dests)]
      (if (< ol oh)
        (recur rest part-for-next dests)
        dests))))

(defn part-destination-p2
  ([workflows part]
   (part-destination-p2 workflows "in" part))
  ([workflows workflow-id {:keys [x m a s] :as part}]
   (case workflow-id
     "A" (* (inc (- (second x) (first x))) (inc (- (second m) (first m)))
            (inc (- (second a) (first a))) (inc (- (second s) (first s))))
     "R" 0
     (let [rules (workflows workflow-id)
           next-dests (process-rules-p2 rules part [])]
       (reduce + (map (fn [[dest part]] (part-destination-p2 workflows dest part)) next-dests))))))

(defn p2 [filename]
  (let [[workflows _] (load-data filename)
        comb (part-destination-p2 workflows {:x [1 4000] :m [1 4000]
                                             :a [1 4000] :s [1 4000]})]
    comb))