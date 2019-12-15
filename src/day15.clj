(ns day15
  (:require
    [clojure.string]))

(defn parse-input [input]
  (zipmap
    (range)
    (map #(Long/parseLong %) (clojure.string/split input #","))))

(defn resolve-read-param [program param-mode relative-base arg]
  (case param-mode
    :position (get program arg 0)
    :immediate arg
    :relative (get program (+ relative-base arg) 0)))

(defn resolve-write-param [param-mode relative-base arg]
  (if (= param-mode :relative)
    (+ relative-base arg)
    arg))

(defn op1-2 [{:keys [program index relative-base] :as state} f [param1-mode param2-mode param3-mode]]
  (let [arg1 (get program (+ index 1))
        arg2 (get program (+ index 2))
        arg3 (get program (+ index 3))
        param1 (resolve-read-param program param1-mode relative-base arg1)
        param2 (resolve-read-param program param2-mode relative-base arg2)
        output-index (resolve-write-param param3-mode relative-base arg3)]
    (merge state
           {:program (assoc program output-index (f param1 param2))
            :index   (+ index 4)})))

(defn op3 [{:keys [program index inputs relative-base] :as state} [param-mode]]
  (merge state
         {:program (assoc program
                     (resolve-write-param param-mode relative-base (get program (inc index)))
                     (first inputs))
          :index   (+ index 2)
          :inputs  (vec (rest inputs))}))

(defn op4 [{:keys [program index relative-base outputs] :as state} [param-mode]]
  (merge state
         {:program program
          :index   (+ index 2)
          :outputs (conj outputs (resolve-read-param program param-mode relative-base (get program (inc index))))}))

(defn op5-6 [{:keys [program index relative-base] :as state} pred [param1-mode param2-mode]]
  (merge state
         {:index (if (pred (resolve-read-param program param1-mode relative-base (get program (inc index))))
                   (resolve-read-param program param2-mode relative-base (get program (+ index 2)))
                   (+ index 3))}))

(defn op7-8 [{:keys [program index relative-base] :as state} pred [param1-mode param2-mode param3-mode]]
  (merge state
         {:program (assoc program
                     (resolve-write-param param3-mode relative-base (get program (+ index 3)))
                     (if (pred (resolve-read-param program param1-mode relative-base (get program (inc index)))
                               (resolve-read-param program param2-mode relative-base (get program (+ index 2))))
                       1
                       0))
          :index   (+ index 4)}))

(defn op9 [{:keys [program index relative-base] :as state} [param-mode]]
  (merge state
         {:relative-base (+ relative-base (resolve-read-param program param-mode relative-base (get program (inc index))))
          :index         (+ index 2)}))

(defn result [{:keys [outputs]}]
  outputs)

(def mode-kw {\0 :position
              \1 :immediate
              \2 :relative})

(defn parse-instruction [{:keys [program index]}]
  (let [op-code (str (get program index))
        modes (reverse (drop-last 2 op-code))]
    {:op              (Integer/parseInt (apply str (take-last 2 op-code)))
     :parameter-modes [(mode-kw (nth modes 0 \0))
                       (mode-kw (nth modes 1 \0))
                       (mode-kw (nth modes 2 \0))]}))

(defn run-program
  ([state]
   (run-program state nil))
  ([state input]
   (let [{:keys [op parameter-modes]} (parse-instruction state)]
     (case op
       1 (recur (op1-2 state + parameter-modes) input)
       2 (recur (op1-2 state * parameter-modes) input)
       3 (recur (op3 (assoc state :inputs [input]) parameter-modes) input)
       4 {:state (op4 state parameter-modes)}
       5 (recur (op5-6 state (complement zero?) parameter-modes) input)
       6 (recur (op5-6 state zero? parameter-modes) input)
       7 (recur (op7-8 state < parameter-modes) input)
       8 (recur (op7-8 state = parameter-modes) input)
       9 (recur (op9 state parameter-modes) input)
       99 {:result (result state)}))))

(def direction->command {[0 -1] 1
                         [0 1]  2
                         [-1 0] 3
                         [1 0]  4})

(def directions (keys direction->command))

(defn wall? [[_ {:keys [cell]}]]
  (= cell 0))

(defn neighbor [world position intcode distance direction]
  (let [neighbor-position (mapv + position direction)]
    {neighbor-position (if-let [node (get world neighbor-position)]
                         (update node :distance (fn [current-distance] (min current-distance distance)))
                         (let [{:keys [state]} (run-program intcode (direction->command direction))
                               [cell] (get state :outputs)]
                           {:cell     cell
                            :intcode  (assoc state :outputs [])
                            :distance distance}))}))

(defn neighbors [world position intcode distance]
  (into {} (mapv (partial neighbor world position intcode distance) directions)))

(defn find-unvisited-with-smallest-distance [world]
  (->> world
       (remove (comp :visited? val))
       (remove wall?)
       (sort-by (comp :distance val))
       (first)))

(defn find-shortest-path [intcode-program]
  (loop [world {[0 0] {:cell     1
                       :intcode  intcode-program
                       :distance 0}}]

    (let [[position {:keys [cell intcode distance]}] (find-unvisited-with-smallest-distance world)]
      (if (= cell 2)
        distance
        (recur (merge (assoc-in world [position :visited?] true)
                      (neighbors world position intcode (inc distance))))))))

(defn part1 [input]
  (find-shortest-path {:program       (parse-input input)
                       :inputs        []
                       :index         0
                       :relative-base 0
                       :outputs       []}))

(defn map-complete-world [intcode-program]
  (loop [world {[0 0] {:cell     1
                       :intcode  intcode-program
                       :distance 0}}]

    (if-let [[position {:keys [intcode distance]}] (find-unvisited-with-smallest-distance world)]
      (recur (merge (assoc-in world [position :visited?] true)
                    (neighbors world position intcode (inc distance))))
      world)))

(defn open? [[_ {:keys [cell]}]]
  (= cell 1))

(defn oxygen? [[_ {:keys [cell]}]]
  (= cell 2))

(defn switch-to-oxygen [world position]
  (update-in world [position :cell] (fn [cell] (if (= cell 1) 2 cell))))

(defn spread-oxygen-to-neighbors [world oxygen]
  (reduce switch-to-oxygen world (map (partial mapv + oxygen) directions)))

(defn spread-oxygen [world oxygens]
  (reduce spread-oxygen-to-neighbors world (keys oxygens)))

(defn part2 [input]
  (loop [world (map-complete-world {:program       (parse-input input)
                                    :inputs        []
                                    :index         0
                                    :relative-base 0
                                    :outputs       []})
         i 0]
    (if (zero? (count (filter open? world)))
      i
      (recur (spread-oxygen world (filter oxygen? world)) (inc i)))))
