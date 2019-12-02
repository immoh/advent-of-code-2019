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

(defn part1 [input]
  (run-program {:program (-> (parse-input input)
                             (assoc 1 12)
                             (assoc 2 2))
                :index 0}))
