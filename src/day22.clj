(ns day22
  (:require
    [clojure.string]))

(defmulti apply-technique (fn [cmd & _] cmd))

(defn get-next-free [deck-size indices n]
  (let [n' (mod n deck-size)]
    (if (get indices n')
      (recur deck-size indices (inc n'))
      n')))

(defn deal-with-increment [deck increment]
  (:indices (reduce (fn [{:keys [indices current]} n]
                      (let [free-index (get-next-free (count deck) indices (+ current increment))]
                        {:indices (assoc indices free-index n)
                         :current free-index}))
                    {:indices {0 (first deck)} :current 0}
                    (rest deck))))

(defmethod apply-technique "deal with increment" [_ deck increment]
  (map val (sort-by key (deal-with-increment deck increment))))

(defmethod apply-technique "deal into new stack" [_ deck _]
  (reverse deck))

(defmethod apply-technique "cut" [_ deck offset]
  (cond
    (zero? offset) deck
    (pos? offset) (concat (drop offset deck) (take offset deck))
    (neg? offset) (concat (take-last (- offset) deck) (drop-last (- offset) deck))))

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

(defn apply-technique* [deck line]
  (prn line)
  (let [{:keys [cmd arg]} (parse-line line)]
    (apply-technique cmd deck arg)))

(defn apply-techniques [deck cmds]
  (reduce apply-technique* deck cmds))

(defn part1 [card-count input]
  (get (zipmap (apply-techniques (range card-count) (clojure.string/split-lines input))
               (range))
       2019))
