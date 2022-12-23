(ns advent-of-code-2022.day10
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :as str]))

(defn- parse-instruction [line]
  (let [[op arg] (str/split line #" ")]
    (case op
      "noop" [:noop]
      "addx" [:addx (parse-long arg)])))

(def leading-cycles {:noop 0 :addx 1})

(def input (->> "day10.txt"
                (resource)
                (slurp)
                (str/split-lines)
                (map parse-instruction)))

(defn- signal-strengths [history]
  (->> history
       (map (fn [[cycle register-x]] [cycle (* cycle register-x)]))
       (into {})))

(defn- interesting-signal-strengths [signal-strengths]
  [(signal-strengths 20)
   (signal-strengths 60)
   (signal-strengths 100)
   (signal-strengths 140)
   (signal-strengths 180)
   (signal-strengths 220)])

(defn- render [cycle register-x]
  (let [pixel (mod (- cycle 1) 40)
        lit? (<= (- register-x 1) pixel (+ register-x 1))
        rendered (if lit? "#" ".")]
    (print rendered)
    (when (= pixel 39)
      (println))))

(defn- run [render? instructions]
  (loop [cycle 1
         [op arg :as current-instruction] (first instructions)
         remaining-cycles (leading-cycles op)
         rest-instructions (rest instructions)
         register-x 1
         history ()]
    (if (nil? current-instruction)
      (reverse history)
      (do
        (when render?
          (render cycle register-x))
        (let [history (cons [cycle register-x] history)
              cycle (inc cycle)]
          (if (> remaining-cycles 0)
            (recur cycle current-instruction (dec remaining-cycles) rest-instructions register-x history)
            (let [register-x (if (= op :addx)
                               (+ register-x arg)
                               register-x)
                  next-instruction (first rest-instructions)
                  [next-op] next-instruction
                  rest-instructions (rest rest-instructions)
                  leading-cycles (leading-cycles next-op)]
              (recur cycle next-instruction leading-cycles rest-instructions register-x history))))))))

(defn part1 []
  (->> input
       (run false)
       (signal-strengths)
       (interesting-signal-strengths)
       (reduce +)))

(defn part2 []
  (->> input
       (run true))
  nil)

(comment
  (part1)
  (part2)
  )
