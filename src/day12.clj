(ns day12
  (:require
    [clojure.string]))

(defn parse-moon [s]
  (let [position (mapv #(Integer/parseInt %)
                       (rest (re-find #"<x=(.+), y=(.+), z=(.+)>" s)))]
    {:position position
     :velocity [0 0 0]}))

(defn parse-input [input]
  (map parse-moon (clojure.string/split-lines input)))

(defn delta-velocity [moon1 moon2]
  (mapv (fn [a b]
          (cond
            (= a b) 0
            (< a b) 1
            (> a b) -1))
        (:position moon1)
        (:position moon2)))

(defn adjust-velocity [moon1 moon2]
  (update moon1 :velocity (fn [velocity] (mapv + velocity (delta-velocity moon1 moon2)))))

(defn apply-gravity [moons moon]
  (reduce adjust-velocity moon moons))

(defn apply-velocity [{:keys [position velocity] :as moon}]
  (assoc moon :position (mapv + position velocity)))

(defn apply-time-step [moons]
  (->> moons
       (map (partial apply-gravity moons))
       (map apply-velocity)))

(defn apply-time-steps [moons steps]
  (nth (iterate apply-time-step moons) steps))

(defn moon-potential-energy [{:keys [position]}]
  (reduce + (map #(Math/abs %) position)))

(defn moon-kinetic-energy [{:keys [velocity]}]
  (reduce + (map #(Math/abs %) velocity)))

(defn moon-total-energy [moon]
  (* (moon-potential-energy moon) (moon-kinetic-energy moon)))

(defn system-total-energy [moons]
  (reduce + (map moon-total-energy moons)))

(defn part1 [input steps]
  (system-total-energy (apply-time-steps (parse-input input) steps)))
