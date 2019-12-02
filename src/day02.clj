(ns day02
  (:require
    [clojure.string]))

(defn parse-input [input]
  (vec (map #(Long/parseLong %) (clojure.string/split input #","))))

(defn operate [{:keys [program index]} f]
  (let [[arg1-index arg2-index output-index] (drop (inc index) program)]
    {:program (assoc program output-index (f (get program arg1-index) (get program arg2-index)))
     :index   (+ index 4)}))

(defn op-code [{:keys [program index]}]
  (get program index))

(defn result [{:keys [program]}]
  (first program))

(defn run-program [state]
  (case (op-code state)
    1 (recur (operate state +))
    2 (recur (operate state *))
    99 (result state)))

(defn run-program-with-noun-and-verb [program noun verb]
  (run-program {:program (-> program
                             (assoc 1 noun)
                             (assoc 2 verb))
                :index 0}))

(defn part1 [input]
  (run-program-with-noun-and-verb (parse-input input) 12 2))

(defn part2 [input]
  (let [program (parse-input input)]
    (loop [noun-verb-pairs (for [noun (range 0 100)
                                 verb (range 0 100)]
                             [noun verb])]
      (let [[noun verb] (first noun-verb-pairs)]
        (if (= 19690720 (run-program-with-noun-and-verb program noun verb))
          (+ (* 100 noun) verb)
          (recur (rest noun-verb-pairs)))))))