(ns day07
  (:require
    [clojure.string]))

(defn parse-input [input]
  (mapv #(Integer/parseInt %) (clojure.string/split input #",")))

(defn resolve-param [program param-mode arg]
  (case param-mode
    :position (get program arg)
    :immediate arg))

(defn op1-2 [{:keys [program index] :as state} f [param1-mode param2-mode]]
  (let [[arg1 arg2 output-index] (drop (inc index) program)
        param1 (resolve-param program param1-mode arg1)
        param2 (resolve-param program param2-mode arg2)]
    (merge state
           {:program (assoc program output-index (f param1 param2))
            :index   (+ index 4)})))

(defn op3 [{:keys [program index inputs] :as state}]
  (let [output-index (get program (inc index))]
    (merge state
           {:program (assoc program output-index (first inputs))
            :index   (+ index 2)
            :inputs  (rest inputs)})))

(defn op4 [{:keys [program index] :as state} [param-mode]]
  (merge state
         {:program program
          :index   (+ index 2)
          :output  (resolve-param program param-mode (get program (inc index)))}))

(defn op5-6 [{:keys [program index] :as state} pred [param1-mode param2-mode]]
  (merge state
         {:index (if (pred (resolve-param program param1-mode (get program (inc index))))
                   (resolve-param program param2-mode (get program (+ index 2)))
                   (+ index 3))}))

(defn op7-8 [{:keys [program index] :as state} pred [param1-mode param2-mode]]
  (merge state
         {:program (assoc program
                     (get program (+ index 3))
                     (if (pred (resolve-param program param1-mode (get program (inc index)))
                               (resolve-param program param2-mode (get program (+ index 2))))
                       1
                       0))
          :index   (+ index 4)}))

(defn result [{:keys [output]}]
  output)

(def mode-kw {\0 :position
              \1 :immediate})

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
      3 (recur (op3 state))
      4 (recur (op4 state parameter-modes))
      5 (recur (op5-6 state (complement zero?) parameter-modes))
      6 (recur (op5-6 state zero? parameter-modes))
      7 (recur (op7-8 state < parameter-modes))
      8 (recur (op7-8 state = parameter-modes))
      99 (result state))))

(defn run-amplifiers [program phase-settings]
  (reduce
    (fn [output phase-setting]
      (run-program {:program program
                    :inputs  [phase-setting output]
                    :index   0}))
    0
    phase-settings))

(defn permutations [s]
  (if (= 1 (count s))
    [s]
    (for [x s
          p (permutations (disj s x))]
      (into [x] p))))

(defn part1 [input]
  (reduce max
          (map (partial run-amplifiers (parse-input input))
               (permutations #{0 1 2 3 4}))))
