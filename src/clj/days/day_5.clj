(ns clj.days.day-5
  (:require [clojure.string :as string]
            [clj.util :as u]))

(def rules (slurp "resources/inputs/day_5_rules.txt"))
(def manual-updates (slurp "resources/inputs/day_5_updates.txt"))

(defn parse-input [in]
  (map (comp #(map parse-long %) #(re-seq #"\d+" %)) (string/split-lines in)))

(def manual-updates-lists (parse-input manual-updates))

(def rules-map
  "Maps a page number key to a list of page numbers that can not happen before itself"
  (reduce (fn [rm [bef aft]]
            (if (rm bef)
              (update rm bef #(conj % aft))
              (assoc rm bef [aft])))
          {}
          (parse-input rules)))

(defn follows-rules? [candidate-update]
  (first
    (reduce (fn [[_ previous :as acc] nv]
              (if (some previous (rules-map nv))
                (reduced [false])
                (update acc 1 #(conj % nv))))
            [true #{}]
            candidate-update)))

(defn sort-update
  [problem-update]
  (reduce (fn [fixed-update nv]
            (if-let [fconflict (some (set (rules-map nv)) fixed-update)]
              (u/insert-nth nv (u/index-of fixed-update fconflict) fixed-update)
              (conj fixed-update nv)))
          []
          problem-update))

(defn p1 []
  (transduce
    (comp (filter follows-rules?) (map #(nth % (quot (count %) 2))))
    + manual-updates-lists))

(defn p2 []
  (transduce
    (comp (filter (complement follows-rules?))
          (map (comp #(nth % (quot (count %) 2)) sort-update)))
    + manual-updates-lists))

(comment
  (p1)
  (p2)
  (time
    (dotimes [_ 1e3]
      (p1)))
  (time
    (dotimes [_ 1e3]
      (p2))))
