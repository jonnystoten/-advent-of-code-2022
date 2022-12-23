(ns advent-of-code-2022.day01
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :as str]))

(defn- max-n [n coll]
  (take n (sort > coll)))

(defn- sum-max-n-groups [n]
  (let [input (slurp (resource "day01.txt"))
        groups (map str/split-lines (str/split input #"\n\n"))]
    (->> groups
         (map (fn [group]
                (->> group
                     (map parse-long)
                     (reduce +))))
         (max-n n)
         (reduce +))))

(defn part1 []
  (sum-max-n-groups 1))

(defn part2 []
  (sum-max-n-groups 3))

(comment
  (part1)
  (part2)
  )
