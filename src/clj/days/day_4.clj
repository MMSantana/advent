(ns clj.days.day-4
  (:require [clojure.string :as string]))

(def input (slurp "resources/inputs/day_4.txt"))

(def test-input "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX")

(defn transpose [coll]
  (apply (partial map str) coll))

(defn nil-shift
  [coll l r]
  (into (into (vec (repeat l nil)) coll) (repeat r nil)))

(defn shift-vectors [coll]
  (first
    (reduce
      (fn [[acc l r] nv]
        [(conj acc (nil-shift nv l r)) (inc l) (dec r)])
      [[] 0 (dec (count coll))]
      coll)))

(defn diagonals [coll]
  (apply (partial map str) (shift-vectors (mapv (partial mapv char) coll))))

(defn p1 [in]
  (count
    (filter
      not-empty
      (flatten
        (keep
          #(re-seq #"(?=(XMAS|SAMX))" %)
          (flatten ((juxt identity transpose diagonals (comp diagonals (partial map reverse))) (string/split-lines in))))))))

(defn x-mas?
  [[[a _ b] [_ c _] [d _ e]]]
  (and
    (= c \A)
    (or
      (and (= a b \M) (= d e \S))
      (and (= a d \M) (= b e \S))
      (and (= b e \M) (= a d \S))
      (and (= d e \M) (= a b \S)))))

(defn p2
  [in]
  (->> in
     string/split-lines
     (map (partial partition 3 1))
     (partition 3 1)
     (map (partial apply (partial map (partial conj []))))
     (mapcat identity)
     (filter x-mas?)
     count))

(p1 test-input)
(p2 test-input)

(time (p1 input))
(time (p2 input))