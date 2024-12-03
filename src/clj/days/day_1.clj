(ns clj.days.day-1)

(defn list-separator
  [pred]
  (fn [index value]
    (when (pred index) value)))

(defn number-distance
  [a b]
  (abs (- a b)))

(def input-lists
  (let [list-string-inputs (-> (slurp "resources/inputs/day_1.txt")
                               (clojure.string/replace (re-pattern "\n") "")
                               (clojure.string/split (re-pattern " ")))
        lists-input (->> list-string-inputs
                         (filter #(not (empty? %)))
                         (mapv bigint))
        first-list (sort (keep-indexed (list-separator even?) lists-input))
        second-list (sort (keep-indexed (list-separator odd?) lists-input))]
    [first-list second-list]))

(->> (last input-lists)
     (mapv number-distance (first input-lists))
     (reduce + 0))

;-------------------------------- p2 ----------------------------------

(defn count-appearances
  [x freq-table]
  [x (get freq-table x 0)])

(defn multiply-and-add-to-total
  [total [a b]]
  (+ total (* a b)))

(let [l1 (first input-lists)
      l2 (last input-lists)
      l2-frequencies (frequencies l2)
      appearances (mapv #(count-appearances % l2-frequencies) l1)]
  (reduce multiply-and-add-to-total 0 appearances))
