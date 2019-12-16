(ns day16
  (:require
    [clojure.string]))

(defn multiplier-seq [n]
  (rest (cycle (mapcat (partial repeat n) [0 1 0 -1]))))

(defn parse-input [input]
  (map #(Integer/parseInt (str %)) input))

(defn last-digit [^Integer n]
  (mod (Math/abs n) 10))

(defn nth-digit [s n]
  (last-digit (reduce + (map * s (multiplier-seq (inc n))))))

(defn next-phase [s]
  (map (partial nth-digit s) (range (count s))))

(defn nth-phase [s n]
  (nth (iterate next-phase s) n))

(defn part1 [input]
  (reduce str (take 8 (nth-phase (parse-input input) 100))))

(defn next-phase2 [s]
  (mapv last-digit (reverse (reductions + (reverse s)))))

(defn nth-phase2 [s offset iterations]
  (nth (iterate next-phase2 (drop offset s)) iterations))

(defn parse-offset [input]
  (Integer/parseInt (subs input 0 7)))

(defn repeat-seq [n s]
  (take (* n (count s)) (cycle s)))

(defn part2 [input]
  (reduce str (take 8 (nth-phase2 (repeat-seq 10000 (parse-input input)) (parse-offset input) 100))))
