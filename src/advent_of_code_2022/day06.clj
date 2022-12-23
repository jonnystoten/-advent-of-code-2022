(ns advent-of-code-2022.day06
  (:require [clojure.java.io :refer [resource]]))

(defn- start-of-packet-pos [size input]
  (loop [pos size
         remaining input]
    (if (apply distinct? (take size remaining))
      pos
      (recur (inc pos) (rest remaining)))))

(defn part1 []
  (->> "day06.txt"
       (resource)
       (slurp)
       (seq)
       (start-of-packet-pos 4)))

(defn part2 []
  (->> "day06.txt"
       (resource)
       (slurp)
       (seq)
       (start-of-packet-pos 14)))

(comment
  (part1)
  (part2)
  )
