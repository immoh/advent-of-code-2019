(ns day13
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

(defn run-multiple-times
  ([intcode-state n]
    (run-multiple-times intcode-state n nil))
  ([intcode-state n input]
   (let [{:keys [state result] :as return-value} (run-program intcode-state input)]
     (if (or result (= n 1))
       return-value
       (recur state (dec n) input)))))

(defn run-arcade [intcode-program]
  (loop [world {}
         intcode-state intcode-program]
    (let [{:keys [result state]} (run-multiple-times intcode-state 3)]
      world
      (if result
        world
        (let [[x y tile] (get state :outputs)]
          (recur (assoc world [x y] tile) (assoc state :outputs [])))))))

(defn block-tiles [world]
  (count (filter #(= % 2) (vals world))))

(defn- high-score-instruction? [x y]
  (= [x y] [-1 0]))

(defn tile [n]
  (case n
    1 "#"
    2 "B"
    3 "-"
    4 "o"
    " "))

(defn ball-position [world]
  (ffirst (filter #(= 4 (val %)) world)))

(defn paddle-position [world]
  (ffirst (filter #(= 3 (val %)) world)))

(defn sign [x]
  (cond
    (zero? x) 0
    (pos? x) 1
    (neg? x) -1))

(defn joystick-position [[ball-x _] [paddle-x]]
  (if (and ball-x paddle-x)
    (sign (- ball-x paddle-x))
    0))

(defn run-arcade2 [intcode-program]
  (loop [world {}
         ball nil
         paddle nil
         high-score 0
         intcode-state intcode-program]
    (let [{:keys [result state]} (run-multiple-times intcode-state 3 (joystick-position ball paddle))]
      (if result
        (if (zero? (block-tiles world)) high-score ::fail)
        (let [[x y z] (get state :outputs)]
          (if (high-score-instruction? x y)
            (recur world ball paddle z (assoc state :outputs []))
            (let [new-world (assoc world [x y] z)]
              (recur new-world
                     (or (ball-position new-world) ball)
                     (or (paddle-position new-world) paddle)
                     high-score
                     (assoc state :outputs [])))))))))

(defn part1 [input]
  (block-tiles (run-arcade {:program       (parse-input input)
                            :inputs        []
                            :index         0
                            :relative-base 0
                            :outputs       []})))

(defn part2 [input]
  (run-arcade2 {:program       (assoc (parse-input input) 0 2)
                :index         0
                :relative-base 0
                :outputs       []}))
