(ns solution
  (:require
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as string]
   [clojure.pprint :as pprint]))

;;; handling input

(defn read-lines [input]
  (with-open [rdr (io/reader input)]
    (vec (line-seq rdr))))

(defn get-wires [input]
  (map #(string/split % #",") (read-lines input)))

;;; where a wire goes

(defn interpret [instruction]
  {:direction (subs instruction 0 1)
   :distance  (Integer/parseInt (subs instruction 1))})

(defn coord-range
  "Get all 1D coordinates from `start` a given `distance` away in `sign` direction.
  `sign` should be either `+` or `-`"
  [start sign distance]
  (range (sign start 1) (sign start distance 1) (sign 1)))

(defn next-coords [start direction distance]
  (let [[x y] (:coord start)]
    (case direction
      "R" (map #(vector % y) (coord-range x + distance))
      "U" (map #(vector x %) (coord-range y + distance))
      "L" (map #(vector % y) (coord-range x - distance))
      "D" (map #(vector x %) (coord-range y - distance)))))

(defn steps-from [start] (drop (inc start) (range)))

(defn with-steps [coords init-step]
  (map (fn [coord step] {:coord coord :step step})
       coords
       (steps-from init-step)))

(defn next-units [start direction distance]
  (with-steps (next-coords start direction distance) (:step start)))

(defn get-next-occupied [curr-coord instruction]
  (let [{:keys [direction distance]} (interpret instruction)]
    (next-units curr-coord direction distance)))

(defn get-occupied-coordinates [wire]
  (loop [occupied-coords []
         curr-coord      {:coord [0 0] :step 0}
         instructions    wire]
    (let [newly-occupied (get-next-occupied curr-coord (first instructions))
          all-occupied   (concat occupied-coords newly-occupied)]
      (if (not (empty? (rest instructions)))
        (recur all-occupied
               (last newly-occupied)
               (rest instructions))
        all-occupied))))

;;; where wires intersect

(defn find-shared-coords [w1 w2]
  (set/intersection (set (map :coord w1))
                    (set (map :coord w2))))

(defn find-intersections [w1 w2]
  (let [shared  (find-shared-coords w1 w2)
        shared? #(contains? shared (:coord %))]
    (map (fn [m1 m2] {:w1 m1 :w2 m2})
         (sort-by :coord (filter shared? w1))
         (sort-by :coord (filter shared? w2)))))

;;; dealing with intersections
(defn magnitude [n] (if (neg? n) (- n) n))

(defn manhattan-distance [{:keys [w1 w2]}]
  (reduce + (map magnitude (:coord w1))))

(defn signal-distance [{:keys [w1 w2]}]
  (+ (:step w1) (:step w2)))

(defn closest [f ms] (apply min (map f ms)))

(defn intersections [input]
  (->> (get-wires input)
       (map get-occupied-coordinates)
       (apply find-intersections)))

(defn part1 [input]
  (closest manhattan-distance (intersections input)))

(defn part2 [input]
  (closest signal-distance (intersections input)))

(def input-file "example3.txt")
