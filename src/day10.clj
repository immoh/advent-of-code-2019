(ns day10
  (:require
    [clojure.string]))

(defn parse-input [input]
  (->> (clojure.string/split-lines input)
       (map-indexed (fn [y row]
                      (map-indexed (fn [x c]
                                     {:position [x y] :content c})
                                   row)))
       (apply concat)
       (filter (comp #{\#} :content))
       (map :position)))

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn delta [[x1 y1] [x2 y2]]
  (let [x' (- x1 x2)
        y' (- y1 y2)]
    (cond
      (zero? x') [0 (if (pos? y') 1 -1)]
      (zero? y') [(if (pos? x') 1 -1) 0]
      :else (let [g (gcd (Math/abs x') (Math/abs y'))]
              [(/ x' g) (/ y' g)]))))

(defn visible-asteroids [asteroids asteroid]
  (count (distinct (map (partial delta asteroid) (disj (set asteroids) asteroid)))))

(defn part1 [input]
  (let [asteroids (parse-input input)]
    (reduce max (map (partial visible-asteroids asteroids) asteroids))))
