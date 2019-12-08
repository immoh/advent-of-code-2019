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

(defn combine-layers [width height layers]
  (reduce
    (fn [acc layer]
      (map conj acc layer))
    (repeat (* width height) [])
    layers))

(defn visible-pixel [pixels]
  (first (drop-while #{\2} pixels)))

(defn print-image [width image]
  (doseq [row (partition width image)]
    (println (clojure.string/replace (apply str row) "0" " "))))

(defn part2 [width height input]
  (->> (parse-input width height input)
       (combine-layers width height)
       (map visible-pixel)
       (print-image width )))
