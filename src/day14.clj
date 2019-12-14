(ns day14
  (:require
    [clojure.string]))

(defn parse-chemical-quantity [s]
  (let [[quantity chemical] (clojure.string/split s #" ")]
    {chemical (Integer/parseInt quantity)}))

(defn parse-rule [s]
  (let [[inputs output] (clojure.string/split s #" => ")]
    {:output (parse-chemical-quantity output)
     :inputs (into {} (map parse-chemical-quantity (clojure.string/split inputs #", ")))}))

(defn parse-input [input]
  (map parse-rule (clojure.string/split-lines input)))

(defn index-by-output-chemicals [rules]
  (into {} (map (fn [{:keys [output inputs]}]
                  {(-> output first key) {:quantity (-> output first val)
                                          :inputs   inputs}})
                rules)))

(defn multiplier [required-quantity quantity]
  (let [m (quot required-quantity quantity)]
    (if (zero? (rem required-quantity quantity)) m (inc m))))

(defn requirements-1 [reverse-rules [chemical required-quantity]]
  (let [{:keys [quantity inputs]} (get reverse-rules chemical)]
    (if (and (pos? required-quantity) quantity)
      (let [multiplier (multiplier required-quantity quantity)]
        (merge
          {chemical (- required-quantity (* multiplier quantity))}
          (zipmap (keys inputs)
                  (map (partial * multiplier) (vals inputs)))))
      {chemical required-quantity})))

(defn resolve-requirements [reverse-rules requirements]
  (let [new-requirements (reduce (partial merge-with +) (map (partial requirements-1 reverse-rules) requirements))]
    (if (= new-requirements requirements)
       requirements
       (recur reverse-rules new-requirements))))

(defn ore-rule? [rule]
  (= ["ORE"] (keys (:inputs rule))))

(defn part1 [input]
  (let [{ore-rules true non-ore-rules false} (group-by ore-rule? (parse-input input))]
    (get (->> (resolve-requirements (index-by-output-chemicals non-ore-rules) {"FUEL" 1})
              (resolve-requirements (index-by-output-chemicals ore-rules)))
         "ORE")))
