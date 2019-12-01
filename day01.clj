(ns day01
  (:require
    [clojure.string]))

(defn parse-input [input]
  (map #(Long/parseLong %) (clojure.string/split-lines input)))

(defn fuel-requirement [mass]
  (- (quot mass 3) 2))

(defn part1 [input]
  (reduce + (map fuel-requirement (parse-input input))))

(defn fuel-requirement2 [mass]
  (reduce + (rest (take-while pos? (iterate fuel-requirement mass)))))

(defn part2 [input]
  (reduce + (map fuel-requirement2 (parse-input input))))