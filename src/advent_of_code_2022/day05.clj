(ns advent-of-code-2022.day05
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :as str]
            [clojure.set]))

(defn- with-indexes [coll] (map #(vector %1 %2) coll (drop 1 (range))))

(defn- parse-input []
  (let [[stacks instructions]
        (->> "day05.txt"
             (resource)
             (slurp)
             (str/split-lines)
             (partition-by #(= "" %))
             (remove #(= '("") %)))]
    [stacks instructions]))

(defn- parse-stacks [input]
  (->> input
       (drop-last)
       (reverse)
       (map (fn [line]
              (->> line
                   (drop 1)
                   (partition 1 4)
                   (flatten)
                   (map str)
                   (with-indexes))))
       (reduce (fn [acc line]
                 (reduce (fn [acc [val i]]
                           (if (= val " ")
                             acc
                             (update acc i #(cons val %)))) acc line)) {})))

(defn- parse-instructions [input]
  (->> input
       (map (fn [line]
              (let [[_ quant from to] (re-matches #"move (\d+) from (\d) to (\d)" line)]
                (mapv parse-long [quant from to]))))))

(defn- apply-instruction [stacks [quant from to]]
  (reduce (fn [stacks _]
            (-> stacks
                (update to #(cons (first (stacks from)) %))
                (update from #(rest %)))) stacks (range quant)))

(defn- apply-instruction-9001 [stacks [quant from to]]
  (-> stacks
      (update to #(concat (take quant (stacks from)) %))
      (update from #(drop quant %))))

(defn part1 []
  (let [[stacks-input instructions-input] (parse-input)
        stacks (parse-stacks stacks-input)
        instructions (parse-instructions instructions-input)
        final-stacks (reduce apply-instruction stacks instructions)]
    (apply str (map #(first (final-stacks %)) (range 1 10)))))

(defn part2 []
  (let [[stacks-input instructions-input] (parse-input)
        stacks (parse-stacks stacks-input)
        instructions (parse-instructions instructions-input)
        final-stacks (reduce apply-instruction-9001 stacks instructions)]
    (apply str (map #(first (final-stacks %)) (range 1 10)))))

(comment
  (parse-input)
  
  (def test-stacks
    {1 '("N" "Z"),
     2 '("D" "C" "M"),
     3 '("P")})
  
  (def test-instructions
    '([1 2 1]
      [3 1 3]
      [2 2 1]
      [1 1 2]))
  
  (apply-instruction test-stacks [1 2 1])
  
  (reduce apply-instruction test-stacks test-instructions)
  
  (part1)
  (part2)
  )
