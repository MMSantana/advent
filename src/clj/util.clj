(ns clj.util)

(defn multiply-and-add-to-total
  [total [a b]]
  (+ total (* a b)))

(defn index-of [coll element]
  (first (keep-indexed #(when (= element %2) %1) coll)))

(defn insert-nth
  [x n coll]
  (let [split-vector (split-at n coll)]
    (vec (concat (first split-vector) [x] (second split-vector)))))