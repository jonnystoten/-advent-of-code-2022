(ns advent-of-code-2022.day08
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :as str]))

(def input (->> "day08.txt"
                (resource)
                (slurp)
                (str/split-lines)))

(def grid-size (count input))
(def grid-range (range 0 grid-size))

(defn- build-grid [lines]
  (reduce-kv
   (fn [grid y line]
     (reduce-kv
      (fn [grid x ch]
        (assoc grid [x y] (parse-long (str ch))))
      grid (vec line)))
   {} lines))

(defn- build-visibility-grid-cols [grid inner-range get-tree visible]
  (reduce (fn [visible x]
            (let [acc (reduce (fn [{:keys [max visible] :as acc} y]
                                (let [val (grid (get-tree x y))]
                                  (if (< max val)
                                    {:max val, :visible (conj visible (get-tree x y))}
                                    acc)))
                              {:max -1, :visible visible}
                              inner-range)]
              (acc :visible)))
          visible
          grid-range))

(def passes (for [get-tree [(fn [x y] [x y]) (fn [x y] [y x])]
                  inner-range [grid-range (reverse grid-range)]]
              [get-tree inner-range]))

(defn- build-visibility-grid [grid]
  (reduce (fn [visible [get-tree inner-range]] (build-visibility-grid-cols grid inner-range get-tree visible)) #{} passes))

(defn- count-visible [tree height grid next past-boundary?]
  (loop [tree (next tree)
         count 0]
    (cond
      (past-boundary? tree) count
      (>= (grid tree) height) (inc count)
      :else (recur (next tree) (inc count)))))

(defn- to-top [[x y] height grid]
  (count-visible [x y] height grid (fn [[x y]] [x (- y 1)]) (fn [[_ y]] (< y 0))))

(defn- to-bottom [[x y] height grid]
  (count-visible [x y] height grid (fn [[x y]] [x (+ y 1)]) (fn [[_ y]] (>= y grid-size))))

(defn- to-left [[x y] height grid]
  (count-visible [x y] height grid (fn [[x y]] [(- x 1) y]) (fn [[x _]] (< x 0))))

(defn- to-right [[x y] height grid]
  (count-visible [x y] height grid (fn [[x y]] [(+ x 1) y]) (fn [[x _]] (>= x grid-size))))

(defn- scenic-scores [grid]
  (map (fn [tree]
         (let [height (grid tree)]
           [tree (*
              (to-top tree height grid)
              (to-bottom tree height grid)
              (to-left tree height grid)
              (to-right tree height grid))]))
       (keys grid)))

(defn part1 []
  (->> input
       (build-grid)
       (build-visibility-grid)
       (count)))

(defn part2 []
  (->> input
       (build-grid)
       (scenic-scores)
       #_(apply max)))

(comment
  (part1)
  (println (part2))
  )
