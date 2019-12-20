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
