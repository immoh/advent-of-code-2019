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
