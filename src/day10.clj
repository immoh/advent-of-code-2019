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
  (let [x' (- x2 x1)
        y' (- y2 y1)]
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

(defn find-best-monitoring-location [asteroids]
  (->> (zipmap asteroids (map (partial visible-asteroids asteroids) asteroids))
       (sort-by val)
       (reverse)
       (first)
       (key)))

(defn abs [n]
  (if (neg? n) (- n) n))

(defn clockwise-sort-val [[x y]]
  (cond
    (= [0 -1] [x y])
    [1 0]

    (and (pos? x) (neg? y))
    [2 (abs (/ x y))]

    (= [1 0] [x y])
    [3 0]

    (and (pos? x) (pos? y))
    [4 (abs (/ y x))]

    (= [0 1] [x y])
    [5 0]

    (= (neg? x) (pos? y))
    [6 (abs (/ x y))]

    (= [-1 0] [x y])
    [7 0]

    (= (neg? x) (neg? y))
    [8 (abs (/ y x))]))

(defn square [n]
  (* n n))

(defn distance-square [[x1 y1] [x2 y2]]
  (+ (square (- x2 x1)) (square (- y2 y1))))

(defn asteroids-by-delta [monitoring-asteroid asteroids]
  (let [m (group-by (partial delta monitoring-asteroid) asteroids)]
    (zipmap (keys m)
            (map (partial sort-by (partial distance-square monitoring-asteroid)) (vals m)))))

(defn find-nth-vaporized [n asteroids-by-delta sorted-deltas]
  (loop [asteroids-by-delta asteroids-by-delta
         deltas (cycle sorted-deltas)
         i 1]
    (if-let [vaporizables (seq (asteroids-by-delta (first deltas)))]
      (if (= i n)
        (first vaporizables)
        (recur (update asteroids-by-delta (first deltas) rest) (rest deltas) (inc i)))
      (recur asteroids-by-delta (rest deltas) i))))

(defn part2 [input]
  (let [asteroids (parse-input input)
        monitoring-asteroid (find-best-monitoring-location asteroids)
        asteroids-by-delta (asteroids-by-delta monitoring-asteroid (disj (set asteroids) monitoring-asteroid))
        sorted-deltas (sort-by clockwise-sort-val (keys asteroids-by-delta))
        [x y] (find-nth-vaporized 200 asteroids-by-delta sorted-deltas)]
    (+ (* 100 x) y)))
