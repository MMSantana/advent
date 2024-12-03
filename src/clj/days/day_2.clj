(ns clj.days.day-2)

(def input-path "resources/inputs/day_2.txt")

(def input
  (slurp input-path))

(defn parse-into-vectors
  [s]
  (->> (clojure.string/split-lines s)
       (mapv #(clojure.string/split % (re-pattern " ")))
       (mapv #(mapv parse-long %))))

(defn monotonic?
  [coll]
  (boolean
    (or (apply > coll)
        (apply < coll))))

(defn gradual-shift?
  [coll]
  (letfn [(bump-detector [[b pv] nv]
            [(and b (<= (abs (- nv pv)) 3)) nv])]
    (first (reduce bump-detector [true (first coll)] coll))))

(defn safe?
  [coll]
  (and (gradual-shift? coll) (monotonic? coll)))

(->> input
     parse-into-vectors
     (filter safe?)
     count)

;-------------------------------- p2 --------------------------------------------

(defn vec-remove
  [pos coll]
  (into (subvec coll 0 pos) (subvec coll (inc pos))))

(defn worse-dampened-safe?
  [coll]
  (let [combinations (delay (for [x (range (count coll))]
                              (vec-remove x coll)))]
    (if (safe? coll)
      true
      (not-every? false? (mapv safe? @combinations)))))

(->> input
     parse-into-vectors
     (filter worse-dampened-safe?)
     count)