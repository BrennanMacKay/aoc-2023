(ns aoc-2023.day-1
  (:require [aoc-2023.common :as common]
            [clojure.string :as string]))

(def filename "problems/day-1-1.txt")
(def digit-p2 #"\d|one|two|three|four|five|six|seven|eight|nine|eno|owt|eerht|ruof|evif|xis|neves|thgie|enin")

(def digit-map {"one"   "1" (string/reverse "one") "1"
                "two"   "2" (string/reverse "two") "2"
                "three" "3" (string/reverse "three") "3"
                "four"  "4" (string/reverse "four") "4"
                "five"  "5" (string/reverse "five") "5"
                "six"  "6" (string/reverse "six") "6"
                "seven"  "7" (string/reverse "seven") "7"
                "eight"  "8" (string/reverse "eight") "8"
                "nine"  "9" (string/reverse "nine") "9"})

(defn first-digit [s]
  (let [d (re-find digit-p2 s)]
    (cond
      (= 1 (count d)) d
      :else (digit-map d))))

(defn calibration [s]
  (let [p1 (first-digit s)
        p2 (first-digit (string/reverse s))]
    (Integer/parseInt (str p1 p2))))

(defn load-data
  ([] (load-data filename))
  ([filename]
   (->> filename
        common/read-input
        (map calibration)
        (apply +))))
