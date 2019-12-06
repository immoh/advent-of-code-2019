(ns day06
  (:require
    [clojure.string]))

(defn parse-input [input]
  (map (comp vec reverse #(clojure.string/split % #"\)")) (clojure.string/split-lines input)))


(defn find-path* [relations path]
  (if-let [next (get relations (last path))]
    (recur relations (conj path next))
    path))

(defn find-path [relations from]
  (find-path* relations [from]))

(defn find-all-paths [orbits-relations]
  (map (partial find-path orbits-relations) (keys orbits-relations)))

(defn part1 [input]
  (reduce + (map (comp count rest) (find-all-paths (into {} (parse-input input))))))

(defn part2 [input]
  (let [orbit-relations (into {} (parse-input input))
        ]
    (->> (concat (rest (find-path orbit-relations "YOU"))
                 (rest (find-path orbit-relations "SAN")))
         (frequencies)
         (filter #(= 1 (val %)))
         (count))))
