(ns advent-of-code-2022.day02
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :as str]))

(def moves {"A" :rock, "B" :paper, "C" :scissors
            "X" :rock, "Y" :paper, "Z" :scissors})

(def required-result {"X" :lose, "Y" :draw, "Z" :win})

(defn- result [opp my]
  (if (= opp my)
    :draw
    (case [opp my]
      [:rock :paper] :win
      [:paper :scissors] :win
      [:scissors :rock] :win
      [:paper :rock] :lose
      [:rock :scissors] :lose
      [:scissors :paper] :lose)))

(defn- correct-move [opp result]
  (if (= result :draw)
    opp
    (case [opp result]
      [:rock :win] :paper
      [:paper :win] :scissors
      [:scissors :win] :rock
      [:paper :lose] :rock
      [:rock :lose] :scissors
      [:scissors :lose] :paper)))

(def result-points {:win 6, :draw 3, :lose 0})
(def shape-points {:rock 1, :paper 2, :scissors 3})

(defn- points [[opp my]]
  (let [result (result opp my)]
    (+ (result-points result) (shape-points my))))

(defn part1 []
  (->> "day02.txt"
       (resource)
       (slurp)
       (str/split-lines)
       (map (fn [line]
              (->> (str/split line #" ")
                   (mapv moves))))
       (map points)
       (reduce +)))

(defn part2 []
  (->> "day02.txt"
       (resource)
       (slurp)
       (str/split-lines)
       (map (fn [line]
              (let [[a b] (str/split line #" ")
                    opp (moves a)
                    result (required-result b)
                    my (correct-move opp result)]
                [opp my])))
       (map points)
       (reduce +)))

(comment
  (part1)
  (part2)
  )
