(ns advent-of-code-2022.day09
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :as str]))

(def input (->> "day09.txt"
                (resource)
                (slurp)
                (str/split-lines)
                (map (fn [line]
                       (let [[dir n] (str/split line #" ")]
                         [dir (parse-long n)])))))

(defn- move-head [[x y] dir]
  (case dir
    "U" [x (- y 1)]
    "D" [x (+ y 1)]
    "L" [(- x 1) y]
    "R" [(+ x 1) y]))

(defn- touching? [[tail-x tail-y] [head-x head-y]]
  (and (<= (abs (- tail-x head-x)) 1) (<= (abs (- tail-y head-y)) 1)))

(defn- closest-x? [[tail-x tail-y] [head-x head-y]]
  (< (abs (- tail-x head-x)) (abs (- tail-y head-y))))

(defn- equidistant? [[tail-x tail-y] [head-x head-y]]
  (= (abs (- tail-x head-x)) (abs (- tail-y head-y))))

(defn- move-tail [[head-x head-y :as head] [tail-x tail-y :as tail]]
  (cond
    (touching? tail head) tail
    (equidistant? tail head) [(/ (+ tail-x head-x) 2) (/ (+ tail-y head-y) 2)]
    (closest-x? tail head) [head-x (/ (+ tail-y head-y) 2)]
    :else [(/ (+ tail-x head-x) 2) head-y]))

(defn- move-rest [head-knot rest-knots]
  (loop [head-knot head-knot
         remaining rest-knots
         result ()]

    (if (empty? remaining)
      result
      (let [new-knot (move-tail head-knot (first remaining))]
        (recur new-knot (rest remaining) (concat result (list new-knot)))))))

(defn- process-line [acc [dir steps]]
  (loop [n steps
         [knots visited :as acc] acc]
    (if (= n 0)
      acc
      (let [head (first knots)
            new-head (move-head head dir)
            rest (rest knots)
            new-rest (move-rest new-head rest)
            visited (conj visited (last new-rest))]
        (recur (dec n) [(cons new-head new-rest) visited])))))

(defn- tail-positions [n]
  (let [[_ visited]
        (reduce process-line [(repeat n [0 0]) #{}] input)]
    (count visited)))

(defn part1 []
  (tail-positions 2))

(defn part2 []
  (tail-positions 10))

(comment
  (part1)
  (part2)
  )
