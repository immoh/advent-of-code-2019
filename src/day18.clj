(ns day18
  (:require
    [clojure.set]
    [clojure.string]))

(defn parse-line [y line]
  (into {} (map-indexed (fn [x c] {[x y] c}) line)))

(defn parse-input [input]
  (into {} (map-indexed parse-line (clojure.string/split-lines input))))

(defn key-cell? [cell-type]
  (<= (int \a) (int cell-type) (int \z)))

(defn lock-cell? [cell-type]
  (<= (int \A) (int cell-type) (int \Z)))

(defn world-keys [world]
  (set (filter key-cell? (vals world))))

(defn find-start [world]
  (key (first (filter #(= \@ (val %)) world))))

(defn select-next [unvisited]
  (reduce (fn [x y]
            (if (< (val x) (val y)) x y))
          unvisited))

(defn neighbor [world position collected-keys delta]
  (let [neighbor-position (mapv + position delta)
        cell (get world neighbor-position)]
    (cond
      (#{\. \@} cell)
      {:position neighbor-position :collected-keys collected-keys}

      (key-cell? cell)
      {:position neighbor-position :collected-keys (conj collected-keys cell)}

      (and (lock-cell? cell) (collected-keys (first (clojure.string/lower-case cell))))
      {:position neighbor-position :collected-keys collected-keys})))

(defn neighbors [world visited position collected-keys]
  (->> (keep (partial neighbor world position collected-keys) [[1 0] [-1 0] [0 1] [0 -1]])
       (remove visited)))

(defn find-shortest-path [world start total-key-count]
  (loop [unvisited {start 0}
         visited #{}]
    (let [[{:keys [position collected-keys] :as node} distance] (select-next unvisited)]
      (if (= (count collected-keys) total-key-count)
        distance
        (recur (merge-with min
                           (dissoc unvisited node)
                           (zipmap (neighbors world visited position collected-keys)
                                   (repeat (inc distance))))
               (conj visited node))))))

(defn part1 [input]
  (let [world (parse-input input)]
    (find-shortest-path world
                        {:position       (find-start world)
                         :collected-keys #{}}
                        (count (world-keys world)))))

(defn modify-to-4-robots [world [x y]]
  (assoc world
    ;; -1 row
    [(dec x) (dec y)] \@
    [x (dec y)] \#
    [(inc x) (dec y)] \@
    ;; 0 row
    [(dec x) y] \#
    [x y] \#
    [(inc x) y] \#
    ;; +1 row
    [(dec x) (inc y)] \@
    [x (inc y)] \#
    [(inc x) (inc y)] \@))

(defn split-world [world [cx cy]]
  [(into {} (filter (fn [[[x y] _]] (and (<= x cx) (<= y cy))) world))
   (into {} (filter (fn [[[x y] _]] (and (>= x cx) (<= y cy))) world))
   (into {} (filter (fn [[[x y] _]] (and (<= x cx) (>= y cy))) world))
   (into {} (filter (fn [[[x y] _]] (and (>= x cx) (>= y cy))) world))])

(defn part2 [input]
  (let [world (parse-input input)
        center (find-start world)
        modified-world (modify-to-4-robots world center)
        all-keys (world-keys modified-world)]
    (reduce + (map (fn [robot-world]
                     (find-shortest-path robot-world
                                         {:position       (find-start robot-world)
                                          :collected-keys (clojure.set/difference all-keys (world-keys robot-world))}
                                         (count all-keys)))
                   (split-world modified-world center)))))
