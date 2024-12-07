(ns day2.day2
  (:require
   [babashka.fs :as fs]
   [clojure.java.io :as io]
   [clojure.string :as str]))

;; part 1
(with-open [rdr (io/reader (fs/file (.getParent (fs/file *file*)) "input.txt"))]
  (->> (line-seq rdr)
       (mapv (fn [line] (->> (str/split line #"\s+") (map Integer/parseInt))))
       (map (fn [report]
              (->> (partition 2 1 report)
                   (map #(apply - %))
                   ((juxt
                     #(reduce (fn [a b] (if (= (compare a 0) (compare b 0)) a (reduced false))) %)
                     #(every? (fn [delta] (and (>= (abs delta) 1) (<= (abs delta) 3))) %))))))
       (map #(every? identity %))
       (filter true?)
       (count)))


(comment
  (->> (partition 2 1 [1 2 1 3])
       (map #(apply - %))
       ((juxt
         #(reduce (fn [a b] (if (= (compare a 0) (compare b 0)) a (reduced false))) %)
         #(every? (fn [delta] (and (>= (abs delta) 1) (<= (abs delta) 3))) %))))

  (->> [[1 false] [true true]]
       (map #(every? identity %))))
