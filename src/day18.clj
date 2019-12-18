(ns day18
  (:require
    [clojure.string]))

(defn parse-line [y line]
  (into {} (map-indexed (fn [x c] {[x y] c}) line)))

(defn parse-input [input]
  (into {} (map-indexed parse-line (clojure.string/split-lines input))))

(defn key-cell? [cell-type]
  (<= (int \a) (int cell-type) (int \z)))

(defn lock-cell? [cell-type]
  (<= (int \A) (int cell-type) (int \Z)))

(defn key-count [world]
  (count (filter key-cell? (vals world))))

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

(defn find-shortest-path [world]
  (let [total-key-count (key-count world)]
    (loop [unvisited {{:position       (find-start world)
                       :collected-keys #{}} 0}
           visited #{}]
      (let [[{:keys [position collected-keys] :as node} distance] (select-next unvisited)]
        (if (= (count collected-keys) total-key-count)
          distance
          (recur (merge-with min
                             (dissoc unvisited node)
                             (zipmap (neighbors world visited position collected-keys)
                                     (repeat (inc distance))))
                 (conj visited node)))))))

(defn part1 [input]
  (find-shortest-path (parse-input input)))
