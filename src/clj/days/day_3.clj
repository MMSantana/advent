(ns clj.days.day-3)

(def input (slurp "resources/inputs/day_3_corrupted.txt"))

(defn grab-multiplications
  [s]
  (re-seq #"mul\(\d+,\d+\)" s))

(defn multiply
  [s]
  (apply * (map parse-long (re-seq #"\d+" s))))

(defn p1
  []
  (->> input
       grab-multiplications
       (reduce (fn [t s] (+ t (multiply s))) 0)))

(defn grab-instructions
  [s]
  (re-seq #"mul\(\d+,\d+\)|do\(\)|don't\(\)" s))

(defn p2
  []
  (->> input
       grab-instructions
       (reduce (fn [[flag _total :as controller] nv]
                 (cond
                   (= "do()" nv)
                   (assoc controller 0 true)

                   (= "don't()" nv)
                   (assoc controller 0 false)

                   (true? flag)
                   (update controller 1 #(+ % (multiply nv)))

                   :else
                   controller))
               [true 0])
       second))

(comment
  (p1)
  (p2))