(ns advent-of-code-2022.day12
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :as str]))

(defn- height [char]
  (- (int char) (int \a)))

(defn- parse-map [lines]
  (reduce-kv
   (fn [result y line]
     (reduce-kv
      (fn [result x char]
        (cond
          (= char \S) (-> result
                          (assoc :start [x y])
                          (assoc-in [:grid [x y]] 0))
          (= char \E) (-> result
                          (assoc :dest [x y])
                          (assoc-in [:grid [x y]] 26))
          :else (-> result
                    (assoc-in [:grid [x y]] (height char)))))
      result (vec line)))
   {} lines))

(defn- get-input []
  (->> "day12.txt"
       (resource)
       (slurp)
       (str/split-lines)
       (parse-map)))

(defn- distance [neighbor current grid]
  (if (<= (- (grid neighbor) (grid current)) 1)
    1
    ##Inf))

(defn- get-neighbors [[x y] grid]
  (let [n [x (- y 1)]
        s [x (+ y 1)]
        w [(- x 1) y]
        e [(+ x 1) y]]
    (filterv #(contains? grid %) [n s e w])))

(defn- process-neighbors [queue g-score neighbors current grid]
  (loop [queue queue, g-score g-score, [n & neighbors] neighbors]
    (if (nil? n)
      [queue g-score]
      (let [tentative-g (+ (get g-score current ##Inf) (distance n current grid))]
        (if (< tentative-g (get g-score n ##Inf))
          (recur
           (concat queue [n])
           (assoc g-score n tentative-g)
           neighbors)
          (recur queue g-score neighbors))))))

(defn- a-star [starts goal grid]
  (loop [queue starts
         g-score (apply merge (map #(hash-map % 0) starts))]
    (if (empty? queue)
      (throw (ex-info "couldn't find a path" {}))
      (let [current (first queue)
            queue (rest queue)]
        (if (= current goal)
          (g-score current)
          (let [neighbors (get-neighbors current grid)
                [queue g-score]
                (process-neighbors queue g-score neighbors current grid)]
            (recur queue g-score)))))))

(defn- steps-required [starts dest grid]
   (a-star starts dest grid))

(defn part1 []
  (let [{:keys [grid start dest]} (get-input)]
    (steps-required (list start) dest grid)))

(defn part2 []
  (let [{:keys [grid _start dest]} (get-input)
        low-points (->> grid
                        (filter (fn [[_ v]] (= 0 v)))
                        (map first))]
    (steps-required low-points dest grid)))
 
(comment
  (time (part1))
  (time (part2))
  )
