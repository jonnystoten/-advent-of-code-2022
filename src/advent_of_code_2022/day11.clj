(ns advent-of-code-2022.day11
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :as str]))

(defn- parse-monkey [input]
  (let [lines (->> input
                   (str/split-lines)
                   (map str/trim))
        items (as-> lines $
                (nth $ 1)
                (str/replace $ "Starting items: " "")
                (str/split $ #", ")
                (map #(bigint (parse-long %)) $))
        [op arg] (as-> lines $
                   (nth $ 2)
                   (str/replace $ "Operation: new = old " "")
                   (str/split $ #" "))
        f (case op
            "+" +
            "*" *)
        operation (fn [old] (f old (if (= arg "old") old (parse-long arg))))
        divisor (as-> lines $
                  (nth $ 3)
                  (str/replace $ "Test: divisible by " "")
                  (parse-long $))
        predicate? (fn [n] (= (mod n divisor) 0))
        consequence (as-> lines $
                      (nth $ 4)
                      (str/replace $ "If true: throw to monkey " "")
                      (parse-long $))
        alternative (as-> lines $
                      (nth $ 5)
                      (str/replace $ "If false: throw to monkey " "")
                      (parse-long $))]

    {:items items
     :operation operation
     :predicate? predicate?
     :consequence consequence
     :alternative alternative
     :inspected 0}))

(def input (->> "day11.txt"
                (resource)
                (slurp)
                (#(str/split % #"\n\n"))
                (mapv parse-monkey)))

(def relief? true)

(defn- turn [monkeys n]
  (let [monkey (nth monkeys n)]
    (loop [item (first (monkey :items))
           remaining (rest (monkey :items))
           monkeys monkeys]
      (if (nil? item)
        ;; clear the current monkey's items - they've all been thrown
        (assoc-in monkeys [n :items] ())
        (let [worry-level ((monkey :operation) item)
              worry-level (if relief? (quot worry-level 3) (mod worry-level 9699690))
              next-monkey (if ((monkey :predicate?) worry-level) (monkey :consequence) (monkey :alternative))
              monkeys (-> monkeys
                          (update-in [next-monkey :items] #(concat % (list worry-level)))
                          (update-in [n :inspected] inc))]
          (recur (first remaining) (rest remaining) monkeys))))))

(defn- round [monkeys _round]
  (reduce turn monkeys (range (count monkeys))))

(defn- rounds [n monkeys]
  (reduce round monkeys (range n)))

(defn- monkey-business [num-rounds]
  (->> input
       (rounds num-rounds)
       (map (fn [m] (m :inspected)))
       (sort >)
       (take 2)
       (reduce *))
  )

(defn part1 []
  (monkey-business 20))

(defn part2 []
  (alter-var-root #'relief? (constantly false))
  (monkey-business 10000))

(comment
  (part1)
  (part2)
  )
