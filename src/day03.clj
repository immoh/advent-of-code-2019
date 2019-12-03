(ns day03
  (:require
    [clojure.set]
    [clojure.string]))

(defn parse-instruction [instruction]
  [(first instruction) (Long/parseLong (subs instruction 1))])

(defn parse-line [line]
  (map parse-instruction (clojure.string/split line #",")))

(defn parse-input [input]
  (map parse-line (clojure.string/split-lines input)))

(defn move-one [dir start]
  (mapv + start
        (case dir
          \U [0 1]
          \D [0 -1]
          \R [1 0]
          \L [-1 0])))

(defn segment-coordinates [start [dir n]]
  (take n (rest (iterate (partial move-one dir) start))))

(defn path-coordinates [instructions]
  (reduce
    (fn [path instruction]
      (concat path (segment-coordinates (last path) instruction)))
    [[0 0]]
    instructions))

(defn intersections [paths]
  (reduce clojure.set/intersection (map set paths)))

(defn manhattan-distance [[x y]]
  (+ (Math/abs x) (Math/abs y)))


(defn part1 [input]
  (-> (map path-coordinates (parse-input input))
      (intersections)
      (disj [0 0])
      (->> (map manhattan-distance)
           (reduce min))))
