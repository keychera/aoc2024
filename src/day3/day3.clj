(ns day3.day3
  (:require
   [babashka.fs :as fs]
   [clojure.string :as str])
  (:import [java.util Scanner]))

;; part 1
(with-open [scanner (.. (Scanner. (fs/file (.getParent (fs/file *file*)) "input.txt"))
                        (useDelimiter "\\)"))]
  (loop [result 0]
    (if (.hasNext scanner)
      (let [delimited    (.next scanner)
            [found? a b] (re-find (re-matcher #"mul\((\d{1,3}),(\d{1,3})$" delimited))
            mul-result   (if found? (* (Integer/valueOf a) (Integer/valueOf b)) 0)]
        (println delimited found? a b)
        (recur (+ result mul-result)))
      result)))

;; part 2
(with-open [scanner (.. (Scanner. (fs/file (.getParent (fs/file *file*)) "input.txt"))
                        (useDelimiter "\\)"))]
  (loop [result 0 do? true]
    (if (.hasNext scanner)
      (let [delimited    (.next scanner)
            keep-doing?  (or (when (re-find (re-matcher #"do\($" delimited)) true)
                             (when (re-find (re-matcher #"don't\($" delimited)) false))
            do?          (if (some? keep-doing?) keep-doing? do?)
            [found? a b] (when do? (re-find (re-matcher #"mul\((\d{1,3}),(\d{1,3})$" delimited)))
            mul-result   (if found? (* (Integer/valueOf a) (Integer/valueOf b)) 0)]
        (println do? delimited  found? a b)
        (recur (+ result mul-result) do?))
      result)))

(comment
  (->> (str/split "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))" #"\)")
       (mapv (fn [s]
               (let [[found? a b] (re-find (re-matcher #"mul\((\d{1,3}),(\d{1,3})$" s))]
                 (println found?)
                 (if found? (* (Integer/valueOf a) (Integer/valueOf b)) 0))))
       (reduce +)))

