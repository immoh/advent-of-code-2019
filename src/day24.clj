(ns day24
  (:require
    [clojure.string]))

(defn parse-input [input]
  (set (keys (filter #(= \# (val %))
                     (into {} (map-indexed (fn [y line]
                                             (into {} (map-indexed (fn [x c] {[x y] c}) line)))
                                           (clojure.string/split-lines input)))))))

(defn biodiversity-rating [bugs]
  (reduce +
          (map
            (fn [[x y] n]
              (if (bugs [x y])
                (long (Math/pow 2 n))
                0))
            (for [y (range 5)
                  x (range 5)]
              [x y])
            (range))))

(defn neighbors [position]
  (map #(mapv + position %) [[0 1] [0 -1] [1 0] [-1 0]]))

(defn bug? [bugs position]
  (let [neighbor-bug-count (count (filter bugs (neighbors position)))]
    (or (and (bugs position)
             (= 1 neighbor-bug-count))
        (and (not (bugs position))
             (#{1 2} neighbor-bug-count)))))

(defn next-minute [bugs]
  (set (filter (partial bug? bugs) (for [y (range 5)
                                         x (range 5)]
                                   [x y]))))

(defn find-cycle [bugs]
  (loop [current-bugs bugs
         seen #{(biodiversity-rating bugs)}]
    (let [new-bugs (next-minute current-bugs)
          new-rating (biodiversity-rating new-bugs)]
      (if (seen new-rating)
        new-rating
        (recur new-bugs (conj seen new-rating))))))

(defn part1 [input]
  (find-cycle (parse-input input)))
