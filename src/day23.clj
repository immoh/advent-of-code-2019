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

(defn dequeue [queues n]
  (update queues n (fn [queue] (vec (rest queue)))))

(defn enqueue [queues n p]
  (update queues n (fn [queue] (vec (conj queue p)))))

(defn run-computer-network [program]
  (loop [computers (into {} (map (fn [i]
                                   {i (:state (run-program {:program       program
                                                            :inputs        [i]
                                                            :index         0
                                                            :relative-base 0
                                                            :outputs       []}))})
                                 (range 50)))
         queues {}
         i 0]
    (let [msg (or (first (get queues i)) -1)
          {:keys [outputs] :as new-state} (:state (run-program (update (get computers i) :inputs (fn [inputs] (vec (conj inputs msg))))))]
      (if (= (count outputs) 3)
        (let [[address x y] outputs]
          (if (= address 255)
            y
            (recur (assoc computers i (assoc new-state :outputs []))
                   (-> queues (dequeue i) (enqueue address x) (enqueue address y))
                   (mod (inc i) 50))))
        (recur (assoc computers i new-state)
               (-> queues (dequeue i))
               (mod (inc i) 50))))))

(defn part1 [input]
  (run-computer-network (parse-input input)))
