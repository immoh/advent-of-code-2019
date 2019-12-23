(ns day23
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


(defn run-program-cycle [computer]
  (let [{:keys [outputs] :as state} (:state (run-program computer))]
    (if (< 0 (count outputs) 3)
      (recur state)
      state)))

(defn part1 [input]
  (loop [computers (into {} (map (fn [i]
                                   {i (:state (run-program {:program       (parse-input input)
                                                            :inputs        [i]
                                                            :index         0
                                                            :relative-base 0
                                                            :outputs       []}))})
                                 (range 50)))
         queue []
         nat-buffer nil
         i 0]
    (let [[computer-id xy] (or (first queue) [(mod i 50) [-1]])
          {:keys [outputs] :as new-state} (run-program-cycle (update (get computers computer-id) :inputs into xy))]
      (if-let [[address x y] (seq outputs)]
        (if (= address 255)
          y
          (recur (assoc computers computer-id (assoc new-state :outputs []))
                 (conj (vec (rest queue)) [address [x y]])
                 nat-buffer
                 0))
        (recur (assoc computers computer-id new-state)
               (vec (rest queue))
               nat-buffer
               (if (seq queue) 0 (inc i)))))))

;; 15029 too high
(defn part2 [input]
  (loop [computers (into {} (map (fn [i]
                                   {i (:state (run-program {:program       (parse-input input)
                                                            :inputs        [i]
                                                            :index         0
                                                            :relative-base 0
                                                            :outputs       []}))})
                                 (range 50)))
         queue []
         nat-buffer nil
         nat-send-ys #{}
         i 0]
    (if (= i 4000)
      (let [y (second nat-buffer)]
        (if (nat-send-ys y)
          y
          (recur computers
                 [[0 nat-buffer]]
                 nil
                 (conj nat-send-ys y)
                 0)))
      (let [[computer-id xy] (or (first queue) [(mod i 50) [-1]])
            {:keys [outputs] :as new-state} (run-program-cycle (update (get computers computer-id) :inputs into xy))]
        (if-let [[address x y] (seq outputs)]
          (if (= address 255)
            (recur (assoc computers computer-id (assoc new-state :outputs []))
                   (vec (rest queue))
                   [x y]
                   nat-send-ys
                   0)
            (recur (assoc computers computer-id (assoc new-state :outputs []))
                   (conj (vec (rest queue)) [address [x y]])
                   nat-buffer
                   nat-send-ys
                   0))
          (recur (assoc computers computer-id new-state)
                 (vec (rest queue))
                 nat-buffer
                 nat-send-ys
                 (if (seq queue) 0 (inc i))))))))
