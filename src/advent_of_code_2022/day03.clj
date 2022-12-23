(ns advent-of-code-2022.day03
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :as str]
            [clojure.set]))

(defn- halves [coll]
  (let [half (/ (count (seq coll)) 2)]
    [(take half coll) (take-last half coll)]))

(defn- common-items [colls]
  (->> colls
       (map set)
       (apply clojure.set/intersection)))

(defn- common-halves [rucksack]
  (->> rucksack
       (halves)
       (common-items)
       (first)))

(defn- common-group [rucksacks]
  (->> rucksacks
       (map seq)
       (common-items)
       (first)))

(defn- priority [item]
  (let [ascii (- (int item) (int \A))]
    (if (>= ascii 32)
      (- ascii 31)
      (+ ascii 27))))

(defn part1 []
  (->> "day03.txt"
       (resource)
       (slurp)
       (str/split-lines)
       (map #(common-halves %))
       (map priority)
       (reduce +)))

(defn part2 []
  (->> "day03.txt"
       (resource)
       (slurp)
       (str/split-lines)
       (partition 3)
       (map #(common-group %))
       (map priority)
       (reduce +)))

(comment
  (part1)
  (part2)
  )
