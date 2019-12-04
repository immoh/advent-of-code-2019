(ns day04)

(defn digit-pairs [n]
  (partition 2 1 (map #(Integer/parseInt (str %)) (str n))))

(defn has-same-adjacent-digits? [pairs]
  (some (fn [[x y]] (= x y)) pairs))

(defn all-increasing-digits? [pairs]
  (every? (fn [[x y]] (nat-int? (- y x))) pairs))

(defn meets-criteria? [n]
  ((every-pred has-same-adjacent-digits? all-increasing-digits?) (digit-pairs n)))

(defn part1 [x y]
  (count (filter meets-criteria? (range x (inc y)))))

(defn same-digit-groups [n]
  (partition-by identity (str n)))

(defn has-same-digit-group-of-two? [groups]
  (some #(= (count %) 2) groups))

(defn meets-criteria2? [n]
  (let [same-digit-groups (same-digit-groups n)]
    (and (all-increasing-digits? (digit-pairs n))
         (has-same-digit-group-of-two? same-digit-groups))))

(defn part2 [x y]
  (count (filter meets-criteria2? (range x (inc y)))))
