(ns advent-of-code-2022.day04
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :as str]
            [clojure.set]))

(defn- one-fully-overlaps? [[[a b] [x y]]]
  (or (and (<= a x) (>= b y)) (and (>= a x) (<= b y))))

(defn- one-overlaps? [[[a b] [x y]]]
  (and (>= b x) (<= a y)))

(defn- solve [f]
  (->> "day04.txt"
       (resource)
       (slurp)
       (str/split-lines)
       (map (fn [line]
              (as-> line $
                (str/split $ #",")
                (mapv (fn [pair] (mapv parse-long (str/split pair #"-"))) $))))
       (filter f)
       (count)))

(defn part1 []
  (solve one-fully-overlaps?))

(defn part2 []
  (solve one-overlaps?))

(comment
  (part1)
  (part2)
  )
