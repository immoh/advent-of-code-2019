(ns day20
  (:require
    [clojure.string]))

(defn parse-line [y line]
  (into {} (map-indexed (fn [x c] {[x y] c}) line)))

(defn parse-input [input]
  (into {} (map-indexed parse-line (clojure.string/split-lines input))))

(def directions [[0 1] [0 -1] [1 0] [-1 0]])

(defn get-next2 [maze point direction]
  (let [next2 [(get maze (mapv + point direction)) (get maze (mapv + point direction direction))]]
    (apply str (if (some neg? direction)
                 (reverse next2)
                 next2))))

(defn label? [s]
  (re-matches #"[A-Z]{2}" s))

(defn get-label [maze point]
  (first (filter label? (map (partial get-next2 maze point) directions))))

(defn find-port [maze [point _]]
  (when-let [label (get-label maze point)]
    {label #{point}}))

(defn tile? [[_ c]]
  (= c \.))

(defn find-ports [maze]
  (reduce (partial merge-with into) (keep (partial find-port maze) (filter tile? maze))))

(defn tile-neighbors [maze point]
  (filter #(= (get maze %) \.) (map #(mapv + point %) directions)))

(defn find-neighbors [maze]
  (let [tiles (keys (filter tile? maze))]
    (zipmap tiles (map (partial tile-neighbors maze) tiles))))

(defn port-connections [ports]
  (into {} (keep (fn [[_ tiles]]
                   (let [[tile1 tile2] (vec tiles)]
                     (when tile2
                       {tile1 tile2
                        tile2 tile1})))
                 ports)))

(defn construct-world [maze]
  (let [ports (find-ports maze)]
    {:start     (first (get ports "AA"))
     :end       (first (get ports "ZZ"))
     :neighbors (merge-with conj (find-neighbors maze) (port-connections ports))}))

(defn select-next-tile [distances visited]
  (first (sort-by val (remove (comp visited key) distances))))

(defn find-shortest-path [{:keys [start end neighbors]}]
  (loop [distances {start 0}
         visited #{}]
    (let [[current-tile distance] (select-next-tile distances visited)]
      (if (= current-tile end)
        distance
        (recur (merge-with min distances (zipmap (get neighbors current-tile)
                                                 (repeat (inc distance))))
               (conj visited current-tile))))))

(defn part1 [input]
  (find-shortest-path (construct-world (parse-input input))))


(defn select-next-tile2 [unvisited]
  (reduce (fn [x y] (if (< (val x) (val y)) x y)) unvisited))

(defn port-type [tiles [x y]]
  (if (and (some (fn [[x2 _]] (< x2 x)) tiles)
           (some (fn [[x2 _]] (> x2 x)) tiles)
           (some (fn [[_ y2]] (< y2 y)) tiles)
           (some (fn [[_ y2]] (> y2 y)) tiles))
    :inner
    :outer))

(defn ports-with-types [all-tiles ports]
  (into {} (keep (fn [[_ tiles]]
                   (let [[tile1 tile2] (vec tiles)]
                     (when tile2
                       (let [[type1 type2] (map (partial port-type all-tiles) [tile1 tile2])]
                         {tile1 {:to tile2 :type type1}
                          tile2 {:to tile1 :type type2}}))))
                 ports)))

(defn construct-world2 [maze]
  (let [ports (find-ports maze)
        tiles (set (keys (filter tile? maze)))]
    {:start {:xy (first (get ports "AA")) :level 0}
     :end   {:xy (first (get ports "ZZ")) :level 0}
     :tiles  tiles
     :ports (ports-with-types tiles ports)}))

(defn neighbors2 [tiles ports visited {:keys [xy level]}]
  (remove visited (concat (filter (comp tiles :xy) (map (fn [direction-xy]
                                                          {:xy    (mapv + xy direction-xy)
                                                           :level level})
                                                        directions))
                          (when-let [{:keys [to type]} (get ports xy)]
                            (when (or (not (zero? level)) (= :inner type))
                              [{:xy to :level (if (= :inner type) (inc level) (dec level))}])))))

(defn find-shortest-path2 [{:keys [start end tiles ports]}]
  (loop [unvisited {start 0}
         visited #{}]
    (let [[current-tile distance] (select-next-tile2 unvisited)]
      (if (= current-tile end)
        distance
        (recur (merge-with min
                           (dissoc unvisited current-tile)
                           (zipmap (neighbors2 tiles ports visited current-tile)
                                   (repeat (inc distance))))
               (conj visited current-tile))))))

(defn part2 [input]
  (find-shortest-path2 (construct-world2 (parse-input input))))
