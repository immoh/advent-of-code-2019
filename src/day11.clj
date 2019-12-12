(ns day11
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

(defn run-program [state]
  (let [{:keys [op parameter-modes]} (parse-instruction state)]
    (case op
      1 (recur (op1-2 state + parameter-modes))
      2 (recur (op1-2 state * parameter-modes))
      3 (recur (op3 state parameter-modes))
      4 {:state (op4 state parameter-modes)}
      5 (recur (op5-6 state (complement zero?) parameter-modes))
      6 (recur (op5-6 state zero? parameter-modes))
      7 (recur (op7-8 state < parameter-modes))
      8 (recur (op7-8 state = parameter-modes))
      9 (recur (op9 state parameter-modes))
      99 {:result (result state)})))

(defn get-tile-color [white-tile? robot-position]
  (if (white-tile? robot-position) 1 0))

(def directions-map {[0 -1] {0 [-1 0]
                             1 [1 0]}
                     [1 0]  {0 [0 -1]
                             1 [0 1]}
                     [0 1]  {0 [1 0]
                             1 [-1 0]}
                     [-1 0] {0 [0 1]
                             1 [0 -1]}})

(defn turn [current-direction turn-direction]
  (get-in directions-map [current-direction turn-direction]))

(defn move [position direction]
  (mapv + position direction))

(defn run-program-twice [intcode-state]
  (let [{:keys [state result] :as return-value} (run-program intcode-state)]
    (if result
      return-value
      (run-program state))))

(defn run-robot [intcode-program white-tiles]
  (loop [robot-position [0 0]
         robot-direction [0 -1]
         intcode-state intcode-program
         white-tiles white-tiles
         painted-tiles #{}]
    (let [{:keys [result state]} (run-program-twice (assoc intcode-state
                                                      :inputs [(get-tile-color white-tiles robot-position)]
                                                      :outputs []))]

      (if result
        {:white-tiles white-tiles
         :painted-tiles painted-tiles}
        (let [[color turn-direction] (get state :outputs)
              new-robot-direction (turn robot-direction turn-direction)
              new-robot-position (move robot-position new-robot-direction)]
          (recur new-robot-position
                 new-robot-direction
                 state
                 (if (= color 1)
                   (conj white-tiles robot-position)
                   (disj white-tiles robot-position))
                 (conj painted-tiles robot-position)))))))

(defn part1 [input]
  (count (:painted-tiles (run-robot {:program       (parse-input input)
                                     :inputs        []
                                     :index         0
                                     :relative-base 0
                                     :outputs       []}
                                    #{}))))

(defn visualize [white-tiles]
  (doseq [y (range (reduce min (map second white-tiles))
                   (inc (reduce max (map second white-tiles))))]
    (println (reduce str (for [x (range (reduce min (map first white-tiles))
                                        (inc (reduce max (map first white-tiles))))]
                           (if (contains? white-tiles [x y]) "#" " "))))))

(defn part2 [input]
  (visualize (:white-tiles (run-robot {:program       (parse-input input)
                                       :inputs        []
                                       :index         0
                                       :relative-base 0
                                       :outputs       []}
                                      #{[0 0]}))))
