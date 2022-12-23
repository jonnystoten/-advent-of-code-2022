(ns advent-of-code-2022.day07
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :as str]))

(defn- parse-command [line]
  (let [[_ command arg] (re-matches #"\$ (cd|ls)(.*)" line)]
    (case command
      "ls" [:ls nil]
      "cd" [:cd (str/trim arg)])))

(defn- parse-entry [entry]
  (let [[info name] (str/split entry #" ")]
    (if (= info "dir")
      [:dir name nil]
      [:file name (parse-long info)])))

(defn- add-entry [path]
  (fn [tree entry]
    (let [[type name size] (parse-entry entry)
          value (case type
                  :dir {}
                  :file size)]
      (update-in tree path #(assoc % name value)))))

(defn- build-tree [lines]
  (loop [tree {}
         path ["/"]
         [head & tail] lines]
    (if (nil? head)
      tree
      (let [[command arg] (parse-command head)]
        (case command
          :cd (case arg
                ".." (recur tree (pop path) tail)
                "/" (recur tree ["/"] tail)
                (recur tree (conj path arg) tail))
          :ls (let [[result rest] (split-with #(not (str/starts-with? % "$ ")) tail)
                    new-tree (reduce (add-entry path) tree result)]
                (recur new-tree path rest)))))))

(defn- total-size [path tree]
  (let [dir (get-in tree path)]
    (->> dir
         (map (fn [[key entry]]
                (if (number? entry)
                  entry
                  (total-size (conj path key) tree))))
         (reduce +))))

(defn- dir-sizes [path tree]
  (let [dir (get-in tree path)]
    (->> dir
         (map (fn [[key entry]]
                (if (number? entry)
                  ()
                  (dir-sizes (conj path key) tree))))
         (reduce conj (list (total-size path tree)))
         (flatten))))

(defn part1 []
  (->> "day07.txt"
       (resource)
       (slurp)
       (str/split-lines)
       (build-tree)
       (dir-sizes ["/"])
       (filter #(< % 100000))
       (reduce +)))

(defn part2 []
  (let [tree (->> "day07.txt"
                  (resource)
                  (slurp)
                  (str/split-lines)
                  (build-tree))
        total-used (total-size ["/"] tree)
        free (- 70000000 total-used)
        required (- 30000000 free)
        sizes (dir-sizes ["/"] tree)]
    (->> sizes
         (sort)
         (filter #(> % required))
         (first))
    ))

(comment
  (parse-command "$ cd /")
  (parse-command "$ ls")
  (parse-command "$ cd ..")
  (parse-command "$ cd hello")
  
  (parse-entry "dir dmd")
  (parse-entry "25595 mdmtpjq.wmf") 
  
  (part1)
  (part2)
  )
