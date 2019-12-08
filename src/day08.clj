(ns day08
  (:require
    [clojure.string]))

(defn parse-input [width height input]
  (partition (* width height) input))

(defn checksum [layer]
  (* (get layer \1 0)
     (get layer \2 0)))

(defn part1 [width height input]
  (->> (parse-input width height input)
       (map frequencies)
       (sort-by #(get % \0 0))
       (first)
       (checksum)))
