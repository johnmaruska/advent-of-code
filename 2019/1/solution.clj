(ns solution)

(def input-file "input.txt")

(defn fuel-required
  "Calculate fuel required for a given mass."
  [mass]
  (- (int (/ mass 3)) 2))

(defn total [v]
  (reduce + v))

(defn apply-input [f]
  (with-open [rdr (clojure.java.io/reader input-file)]
    (->> (line-seq rdr)
         (map #(Integer/parseInt %))
         f)))

(defn part-1 []
  (apply-input (fn [module-masses]
                 (total (map fuel-required module-masses)))))

(defn corrected-fuel-required
  "Given a total-module-mass, recursively calculate fuel required for combined
  module and fuel mass."
  [mass]
  (loop [fuel-acc  0
         next-mass mass]
    (let [next-fuel (fuel-required next-mass)]
      (if (neg? next-fuel)
        fuel-acc
        (recur (+ fuel-acc next-fuel)
               next-fuel)))))

(defn part-2 []
  (apply-input (fn [module-masses]
                 (total (map corrected-fuel-required module-masses)))))
