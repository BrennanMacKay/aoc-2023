(ns aoc-2023.day-3
  (:require [aoc-2023.common :as common]))

(def test-input "problems/day-3-1-t.txt")
(def p1-input "problems/day-3-1.txt")

(defn digit? [c]
  (<= 48 (int c) 57))

(defn adjacent-pos [[x y]]
  [[(inc x) y]
   [(inc x) (inc y)]
   [x (inc y)]
   [(dec x) y]
   [(dec x) (dec y)]
   [x (dec y)]
   [(inc x) (dec y)]
   [(dec x) (inc y)]])

(defn sym-adj [pos symbols]
  ;(clojure.pprint/pprint [pos symbols])
  (let [adjs (->> (adjacent-pos pos)
                  (map (fn [adj-pos] (when (contains? symbols adj-pos) adj-pos)))
                  (remove nil?))]
    ;(clojure.pprint/pprint ["adj" pos adjs])
    adjs))

(defn read-symbols
  ([lines]
   (read-symbols (rest lines) (first lines) 0 0 {}))
  ([lines cur-line x y symbols]
   (let [cur (first cur-line)]
     ;(clojure.pprint/pprint (str cur " " x " " y " " cur-line))
     (cond
       ;; finished
       (and (empty? lines) (empty? cur-line)) symbols
       ;; finished line
       (empty? cur-line) (recur (rest lines) (first lines) 0 (inc y) symbols)
       ;; .
       (= \. cur) (recur lines (rest cur-line) (inc x) y symbols)
       ;; digit (ignoring for now)
       (digit? cur) (recur lines (rest cur-line) (inc x) y symbols)
       ;; symbol!
       :else (recur lines (rest cur-line) (inc x) y (assoc symbols [x y] {:symbol cur
                                                                          :adj []}))))))

(defn add-adj-number [number adj-symbols symbols]
  (if (empty? adj-symbols)
    symbols
    (let [adj-symbol (first adj-symbols)
          symbol (symbols adj-symbol)
          updated (update symbol :adj conj number)]
      (recur number (rest adj-symbols) (assoc symbols adj-symbol updated)))))

(defn read-digits
  ([lines symbols]
   (read-digits (rest lines) (first lines) 0 0 [] #{} [] symbols))
  ([lines cur-line x y cur-number cur-adj numbers symbols]
   ;(clojure.pprint/pprint ["digits" cur-line x y])
   (let [cur (first cur-line)]
     ;(clojure.pprint/pprint [cur x y cur-line numbers])
     (cond
       (and (empty? lines) (empty? cur-line))
       {:numbers (remove nil? (conj numbers (when (not-empty cur-number) {:pos  [x y]
                                                                          :num  (Integer/parseInt (apply str cur-number))
                                                                          :sym? (not (empty? cur-adj))})))
        :symbols symbols}

       (empty? cur-line)
       (recur (rest lines) (first lines) 0 (inc y) [] #{}
              (conj numbers (when (not-empty cur-number) {:pos [x y]
                                                          :num (Integer/parseInt (apply str cur-number))
                                                          :sym? (not (empty? cur-adj))}))
              (if (not-empty cur-number)
                (add-adj-number (Integer/parseInt (apply str cur-number)) cur-adj symbols)
                symbols))

       (digit? cur)
       (recur lines (rest cur-line) (inc x) y
              (conj cur-number cur)
              (into cur-adj (sym-adj [x y] symbols))
              numbers
              symbols)

       :else
       (recur lines (rest cur-line) (inc x) y [] #{}
              (conj numbers (when (not-empty cur-number) {:pos [x y]
                                                          :num (Integer/parseInt (apply str cur-number))
                                                          :sym? (not (empty? cur-adj))}))
              (if (not-empty cur-number)
                (add-adj-number (Integer/parseInt (apply str cur-number)) cur-adj symbols)
                symbols))
       ))))

(defn load-data
  ([filename]
   (->> (common/read-input filename))))


(defn p1 [filename]
  (let [data (load-data filename)
        symbols (read-symbols data)
        digits (read-digits data symbols)
        included (->> (:numbers digits)
                      (filter #(:sym? %))
                      (map :num))]
    (clojure.pprint/pprint digits)
    (apply + included)))


(defn p2 [filename]
  (let [data (load-data filename)
        symbols (read-symbols data)
        symbols-with-adj (vals (:symbols (read-digits data symbols)))
        with-two (filter (fn [sym]
                           (and
                             (= \* (:symbol sym))
                             (= 2 (count (:adj sym))))) symbols-with-adj)
        prod (map #(apply * (:adj %)) with-two)]

    (apply + prod)

    #_(clojure.pprint/pprint [symbols-with-adj prod])))