(ns day17
  (:require
    [clojure.set]
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
      4 (recur (op4 state parameter-modes))
      5 (recur (op5-6 state (complement zero?) parameter-modes))
      6 (recur (op5-6 state zero? parameter-modes))
      7 (recur (op7-8 state < parameter-modes))
      8 (recur (op7-8 state = parameter-modes))
      9 (recur (op9 state parameter-modes))
      99 (result state))))

(defn get-scaffolds [chars]
  (loop [scaffolds #{}
         x 0
         y 0
         chars chars]
    (if-let [c (first chars)]
      (case c
        10 (recur scaffolds 0 (inc y) (rest chars))
        46 (recur scaffolds (inc x) y (rest chars))
        (recur (conj scaffolds [x y]) (inc x) y (rest chars)))
      scaffolds)))

(defn intersection? [scaffold? position]
  (every? #(scaffold? (mapv + position %)) [[0 1] [0 -1] [1 0] [-1 0]]))

(defn find-intersections [scaffolds]
  (filter (partial intersection? scaffolds) scaffolds))

(defn alignment-parameter [[x y]]
  (* x y))

(defn sum [xs]
  (reduce + xs))

(defn part1 [input]
  (->> (run-program {:program       (parse-input input)
                     :inputs        []
                     :index         0
                     :relative-base 0
                     :outputs       []})
       (get-scaffolds)
       (find-intersections)
       (map alignment-parameter)
       (sum)))

(defn robot-found [world position direction]
  (-> world
      (update :scaffolds conj position)
      (assoc :robot {:position position :direction direction})))

(defn parse-world [chars]
  (loop [world {:scaffolds #{}}
         x 0
         y 0
         chars chars]
    (if-let [c (first chars)]
      (case c
        10 (recur world 0 (inc y) (rest chars))
        35 (recur (update world :scaffolds conj [x y]) (inc x) y (rest chars))
        46 (recur world (inc x) y (rest chars))
        60 (recur (robot-found world [x y] [-1 0]) (inc x) y (rest chars))
        62 (recur (robot-found world [x y] [1 0]) (inc x) y (rest chars))
        94 (recur (robot-found world [x y] [0 -1]) (inc x) y (rest chars))
        118 (recur (robot-found world [x y] [0 1]) (inc x) y (rest chars)))
      world)))

(def left {[1 0]  [0 -1]
           [0 -1] [-1 0]
           [-1 0] [0 1]
           [0 1]  [1 0]})

(def right (clojure.set/map-invert left))

(defn end? [{{:keys [position direction]} :robot scaffolds :scaffolds}]
  (not-any? scaffolds (map #(mapv + position %) [direction (left direction) (right direction)])))

(defn turn-needed? [{{:keys [position direction]} :robot scaffolds :scaffolds}]
  (not (scaffolds (mapv + position direction))))

(defn turn-direction [{{:keys [position direction]} :robot scaffolds :scaffolds}]
  (first (filter #(scaffolds (mapv + position (% direction))) [right left])))

(def turn-instruction {right "R"
                       left  "L"})

(defn forward-moves [{{:keys [position direction]} :robot scaffolds :scaffolds}]
  (loop [n 0
         position position]
    (if (scaffolds (mapv + position direction))
      (recur (inc n) (mapv + position direction))
      n)))

(defn find-path [world]
  (loop [world world
         path []]
    (cond
      (end? world) path
      (turn-needed? world) (let [turn-fn (turn-direction world)]
                             (recur (update-in world [:robot :direction] turn-fn)
                                    (conj path (turn-instruction turn-fn))))
      :else (let [forward-moves (forward-moves world)]
              (recur (update world :robot (fn [{position :position [dx dy] :direction :as robot}]
                                            (assoc robot :position (mapv + position [(* forward-moves dx)
                                                                                     (* forward-moves dy)]))))
                     (conj path forward-moves))))))

(defn seq-starts-with? [s sub]
  (= (take (count sub) s) sub))

(defn find-next-piece [s a b c]
  (first (filter (partial seq-starts-with? s) [a b c])))

(defn try-composition [s a b c]
  (loop [s s
         ms []]
    (if (seq s)
      (when-let [sub (find-next-piece s a b c)]
        (recur (drop (count sub) s) (conj ms ({a "A" b "B" c "C"} sub))))
      {:a a :b b :c c :movement-routine ms})))

(defn find-composition [s movement-functions]
  (first (drop-while nil?
                     (for [a movement-functions
                           b movement-functions
                           c movement-functions
                           :when (and (not= a b) (not= b c) (not= a c))]
                       (try-composition s a b c)))))

(defn valid-movement-function? [s]
  (<= (count (clojure.string/join "," s)) 20))

(defn all-subseqs [s]
  (for [start (range (count s))
        length (range 1 (inc (- (count s) start)))]
    (take length (drop start s))))

(defn find-possible-movement-functions? [s]
  (distinct (filter valid-movement-function? (all-subseqs s))))

(defn find-movement-function-composition [s]
  (find-composition s (find-possible-movement-functions? s)))

(defn create-input-seq [a b c movement-routine]
  (concat
    (map int (clojure.string/join "," movement-routine))
    [10]
    (map int (clojure.string/join "," a))
    [10]
    (map int (clojure.string/join "," b))
    [10]
    (map int (clojure.string/join "," c))
    [10 110 10]))

(defn part2 [input]
  (let [program (parse-input input)
        {:keys [a b c movement-routine]} (->> (run-program {:program       program
                                                            :inputs        []
                                                            :index         0
                                                            :relative-base 0
                                                            :outputs       []})
                                              (parse-world)
                                              (find-path)
                                              (find-movement-function-composition))]
    (last (run-program {:program       (assoc program 0 2)
                        :inputs        (create-input-seq a b c movement-routine)
                        :index         0
                        :relative-base 0
                        :outputs       []}))))
