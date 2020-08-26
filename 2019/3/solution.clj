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

(defn coord-range
  "Get all 1D coordinates from `start` a given `distance` away in `sign` direction.
  `sign` should be either `+` or `-`"
  [start sign distance]
  (range (sign start 1) (sign start distance 1) (sign 1)))

(defn interpret-instruction [[x y] instruction]
  (let [direction (subs instruction 0 1)
        distance  (Integer/parseInt (subs instruction 1))]
    (case direction
      "R" (map #(vector % y) (coord-range x + distance))
      "U" (map #(vector x %) (coord-range y + distance))
      "L" (map #(vector % y) (coord-range x - distance))
      "D" (map #(vector x %) (coord-range y - distance)))))

(defn get-occupied-coordinates [wire]
  (loop [occupied-coords []
         curr-coord      [0 0]
         instructions    wire]
    (let [newly-occupied (interpret-instruction curr-coord (first instructions))
          all-occupied   (concat occupied-coords newly-occupied)]
      (if (not (empty? (rest instructions)))
        (recur all-occupied
               (last newly-occupied)
               (rest instructions))
        (concat occupied-coords newly-occupied)))))

;;; where wires intersect

(defn find-intersections [w1 w2]
  (set/intersection (set w1) (set w2)))

;;; dealing with intersections

(defn magnitude [n] (if (neg? n) (- n) n))

(defn distance-from-central-port
  "Calculate Manhattan distance from central port.

  Coordinates are numerical pairs.
  Central-port is defined as the coordinate [0 0]."
  [coordinate]
  (reduce + (map magnitude coordinate)))

(defn closest-distance-from-central [coords]
  (apply min (map distance-from-central-port coords)))

(defn part1 [input]
  (->> input
       get-wires
       (map get-occupied-coordinates)
       (apply find-intersections)
       closest-distance-from-central))

(println "Result:" (part1 "input.txt"))
