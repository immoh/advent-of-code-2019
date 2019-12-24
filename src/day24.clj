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

(defn resolve-neighbor [{:keys [level xy]} direction]
  (let [[neighbor-x neighbor-y :as neighbor-xy] (mapv + xy direction)]
    (cond
      (= neighbor-xy [2 2])
      (case direction
        [0 1] [{:level (inc level) :xy [0 0]}
               {:level (inc level) :xy [1 0]}
               {:level (inc level) :xy [2 0]}
               {:level (inc level) :xy [3 0]}
               {:level (inc level) :xy [4 0]}]
        [0 -1] [{:level (inc level) :xy [0 4]}
                {:level (inc level) :xy [1 4]}
                {:level (inc level) :xy [2 4]}
                {:level (inc level) :xy [3 4]}
                {:level (inc level) :xy [4 4]}]
        [1 0] [{:level (inc level) :xy [0 0]}
               {:level (inc level) :xy [0 1]}
               {:level (inc level) :xy [0 2]}
               {:level (inc level) :xy [0 3]}
               {:level (inc level) :xy [0 4]}]
        [-1 0] [{:level (inc level) :xy [4 0]}
                {:level (inc level) :xy [4 1]}
                {:level (inc level) :xy [4 2]}
                {:level (inc level) :xy [4 3]}
                {:level (inc level) :xy [4 4]}])

      (= neighbor-x -1)
      [{:level (dec level) :xy [1 2]}]

      (= neighbor-x 5)
      [{:level (dec level) :xy [3 2]}]

      (= neighbor-y -1)
      [{:level (dec level) :xy [2 1]}]

      (= neighbor-y 5)
      [{:level (dec level) :xy [2 3]}]

      :else
      [{:level level :xy neighbor-xy}])))

(defn neighbors2 [position]
  (mapcat (partial resolve-neighbor position) [[0 1] [0 -1] [1 0] [-1 0]]))

(defn bug2? [bugs position]
  (let [neighbor-bug-count (count (filter bugs (neighbors2 position)))]
    (or (and (bugs position)
             (= 1 neighbor-bug-count))
        (and (not (bugs position))
             (#{1 2} neighbor-bug-count)))))

(defn next-minute2 [bugs]
  (let [levels (map :level bugs)]
    (set (filter (partial bug2? bugs) (for [level (range (dec (reduce min levels)) (+ 2 (reduce max levels)))
                                           y (range 5)
                                           x (range 5)
                                           :when (not= [x y] [2 2])]
                                       {:level level :xy [x y]})))))

(defn parse-input2 [input]
  (set (map (fn [xy] {:level 0 :xy xy}) (parse-input input))))

;; too low 983
(defn part2 [input]
  (count (first (drop 200 (iterate next-minute2 (parse-input2 input))))))
