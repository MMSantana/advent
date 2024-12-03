(ns clj.util)

(defn multiply-and-add-to-total
  [total [a b]]
  (+ total (* a b)))

(defn split-long
  [s]
  (map parse-long (re-seq #"\d+" s)))
