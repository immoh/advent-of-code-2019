(ns day22
  (:require
    [clojure.string]))

(defmulti apply-technique (fn [_ _ cmd _] cmd))

(defmethod apply-technique "deal with increment" [deck-size card-position _ increment]
  (mod (*' card-position increment) deck-size))

;; 0 1 (2) 3 4 5 6 7 8 9    (position 2)
;; 9 8 7 6 5 4 3 (2) 1 0    (position 7)
(defmethod apply-technique "deal into new stack" [deck-size card-position _ _]
  (-' deck-size card-position 1))

(defmethod apply-technique "cut" [deck-size card-position cmd offset]
  (cond
    (zero? offset) card-position

    (pos? offset) (if (<= card-position (dec offset))
                    ;; 0 1 (2) 3 4 5 6 7 8 9     cut 4
                    ;; 4 5 6 7 8 9 0 1 (2) 3     position 8 = (+ (- 10 4) 2)
                    (+' (-' deck-size offset) card-position)
                    ;; 0 1 (2) 3 4 5 6 7 8 9     cut 2
                    ;; (2) 3 4 5 6 7 8 9 0 1     position 0 = 2 - 2
                    (-' card-position offset))
    (neg? offset) (recur deck-size card-position cmd (+ deck-size offset))))

(defn maybe-parse-int [s]
  (try
    (Integer/parseInt s)
    (catch NumberFormatException _
      nil)))

(defn parse-line [line]
  (let [split-line (clojure.string/split line #" ")
        arg (maybe-parse-int (last split-line))]
    (if arg
      {:cmd (clojure.string/join " " (butlast split-line))
       :arg arg}
      {:cmd line})))

(defn apply-technique* [{:keys [deck-size card-position]} line]
  (let [{:keys [cmd arg]} (parse-line line)]
    {:deck-size     deck-size
     :card-position (apply-technique deck-size card-position cmd arg)}))

(defn get-card-position [deck-size card-position lines]
  (:card-position (reduce apply-technique* {:deck-size deck-size :card-position card-position} lines)))

(defn part1 [input]
  (get-card-position 10007 2019 (clojure.string/split-lines input)))

(defmulti apply-technique2 (fn [_ _ cmd _] cmd))

(defmethod apply-technique "deal into new stack" [deck-size card-position _ _]
  (-' deck-size card-position 1))

(defn apply-technique2* [{:keys [deck-size position card]} line]
  (let [{:keys [cmd arg]} (parse-line line)]
    {:deck-size     deck-size
     :card-position (apply-technique2 deck-size card-position cmd arg)}))

(defn get-card-in-position [deck-size position lines]
  (:card (reduce apply-technique2* {:deck-size deck-size :position position :card 2020})))

(defn part2 [input]
  (get-card-in-position 101741582076661 2020 (clojure.string/split-lines input)))