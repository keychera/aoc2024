(ns day1.day1
  (:require
   [babashka.fs :as fs]
   [clojure.java.io :as io]
   [clojure.string :as str]))

;; part 1
(with-open [rdr (io/reader (fs/file (.getParent (fs/file *file*)) "input.txt"))]
  (let [[list1 list2]
        (->>  (line-seq rdr)
              (mapv (fn [line] (->> (str/split line #"\s+") (map Integer/parseInt))))
              (flatten)
              (map-indexed vector)
              (group-by (fn [el] (even? (first el))))
              (vals)
              (map (fn [li] (sort (mapv last li)))))
        
        distances
        (->> (interleave list1 list2)
             (partition 2)
             (map (fn [[a b]] (abs (- a b)))))]
    (reduce + distances)))


;; part 2
(with-open [rdr (io/reader (fs/file (.getParent (fs/file *file*)) "input.txt"))]
  (let [[list1 list2]
        (->>  (line-seq rdr)
              (mapv (fn [line] (->> (str/split line #"\s+") (map Integer/parseInt))))
              (flatten)
              (map-indexed vector)
              (group-by (fn [el] (even? (first el))))
              (vals)
              (map (fn [li] (sort (mapv last li)))))

        similarity
        (->> list1 (map (fn [el1]
                          (->> list2 (filter (fn [el2] (= el1 el2))) count (* el1)))))]
    (reduce + similarity)))


(comment
  ;; comparing with https://github.com/reflechant/aoc2024-clj/blob/main/src/reflechant/aoc2024/day01/part1.clj
  ;; solution above can be better done with below
  (apply map vector [[1 2] [3 4] [5 6]])
  (apply map - [[1 2] [3 4] [5 6]])
  )