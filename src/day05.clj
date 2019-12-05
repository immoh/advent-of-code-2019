(ns day05
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

(defn op3 [{:keys [program index input] :as state}]
  (let [output-index (get program (inc index))]
    (merge state
           {:program (assoc program output-index input)
            :index   (+ index 2)})))

(defn op4 [{:keys [program index] :as state} [param-mode]]
  (merge state
         {:program program
          :index   (+ index 2)
          :output  (resolve-param program param-mode (get program (inc index)))}))

(defn result [{:keys [output]}]
  output)

(def mode-kw {\0 :position
              \1 :immediate})

(defn parse-instruction [{:keys [program index]}]
  (let [op-code (str (get program index))
        modes (reverse (drop-last 2 op-code))]
    {:op (Integer/parseInt (apply str (take-last 2 op-code)))
     :parameter-modes [(mode-kw (nth modes 0 \0))
                       (mode-kw (nth modes 1 \0))
                       (mode-kw (nth modes 2 \0))]}))

(defn run-program [state]
  (let [{:keys [op parameter-modes] :as instruction} (parse-instruction state)]
    (case op
      1 (recur (op1-2 state + parameter-modes))
      2 (recur (op1-2 state * parameter-modes))
      3 (recur (op3 state))
      4 (recur (op4 state parameter-modes))
      99 (result state))))

(defn solve [program input]
  (run-program {:program (parse-input program)
                :input   input
                :index   0}))
