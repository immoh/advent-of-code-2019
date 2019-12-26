(ns day25
  (:refer-clojure :exclude [take drop])
  (:require
    [clojure.string]
    [clojure.math.combinatorics :as combinatorics]))

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
  (if (seq inputs)
    (merge state
           {:program (assoc program
                       (resolve-write-param param-mode relative-base (get program (inc index)))
                       (first inputs))
            :index   (+ index 2)
            :inputs  (vec (rest inputs))})
    state))

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
      3 {:state (op3 state parameter-modes)}
      4 {:state (op4 state parameter-modes)}
      5 (recur (op5-6 state (complement zero?) parameter-modes))
      6 (recur (op5-6 state zero? parameter-modes))
      7 (recur (op7-8 state < parameter-modes))
      8 (recur (op7-8 state = parameter-modes))
      9 (recur (op9 state parameter-modes))
      99 {:result (result state)})))

(defn run-until-next-output
  ([program cmd]
    (run-until-next-output (assoc program :inputs (mapv int (str cmd "\n")))))
  ([program]
   (loop [program program
          output-buffer []
          idle-counter 0]
     (let [{:keys [result state]} (run-program program)]
       (if result
         {:output (reduce str (map char output-buffer))}
         (let [{:keys [inputs outputs] :as new-program} state]
           (if (or (seq inputs) (seq outputs))
             (recur (assoc new-program :outputs [])
                    (concat output-buffer outputs)
                    0)
             (if (= idle-counter 2)
               {:program new-program :output (reduce str (map char output-buffer))}
               (recur new-program output-buffer (inc idle-counter))))))))))

(defn get-inventory-items [intcode]
  (->> (run-until-next-output intcode "inv")
       (:output)
       (clojure.string/split-lines)
       (filter #(clojure.string/starts-with? % "-"))
       (map #(subs % 2))
       (set)))
(defonce state (atom nil))

(defn start-game [input]
  (let [{:keys [program output]} (run-until-next-output {:program       (parse-input input)
                                                         :inputs        []
                                                         :index         0
                                                         :relative-base 0
                                                         :outputs       []})]
    (reset! state program)
    (println output)))

(defn- run-command [cmd]
  (let [{:keys [program output]} (run-until-next-output @state cmd)]
    (reset! state program)
    (println output)))

(defn east [] (run-command "east"))
(defn west [] (run-command "west"))
(defn south [] (run-command "south"))
(defn north [] (run-command "north"))

(defn inv [] (run-command "inv"))

(defn take [item]
  (run-command (str "take " item)))

(defn drop [item]
  (run-command (str "drop " item)))


(def inventories (for [x1 (range 2)
                       x2 (range 2)
                       x3 (range 2)
                       x4 (range 2)
                       x5 (range 2)
                       x6 (range 2)
                       x7 (range 2)
                       x8 (range 2)]
                   (set (keep identity [(when (zero? x1) "dark matter")
                                        (when (zero? x2) "klein bottle")
                                        (when (zero? x3) "monolith")
                                        (when (zero? x4) "cake")
                                        (when (zero? x5) "mutex")
                                        (when (zero? x6) "tambourine")
                                        (when (zero? x7) "fuel cell")
                                        (when (zero? x8) "astrolabe")]))))

(defn drop-everything []
  (doseq [item (get-inventory-items @state)]
    (drop item)))

(defn take-items [items]
  (doseq [item items]
    (take item)))

(defn collect-items []
  (south)
  (take "cake")
  (south)
  (west)
  (take "mutex")
  (east)
  (north)
  (north)
  (west)
  (take "klein bottle")
  (south)
  (east)
  (take "monolith")
  (south)
  (take "fuel cell")
  (west)
  (west)
  (take "astrolabe")
  (east)
  (east)
  (north)
  (west)
  (north)
  (west)
  (north)
  (take "tambourine")
  (south)
  (west)
  (take "dark matter")
  (west))

(defn try-all-inventories []
  (loop [inventories (combinatorics/subsets (vec (get-inventory-items @state)))]
    (when-let [inventory (first inventories)]
      (drop-everything)
      (take-items inventory)
      (let [{:keys [program output]} (run-until-next-output @state "north")]
        (println output)
        (when program
          (reset! state program)
          (recur (rest inventories)))))))

(defn part1 [input]
  (start-game input)
  (collect-items)
  (try-all-inventories))
