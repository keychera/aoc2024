(ns day5.day5
  (:require
   [babashka.fs :as fs]
   [clojure.java.io :as io]
   [clojure.string :as str]))

;; part 1
(with-open [reader (io/reader (fs/file (.getParent (fs/file *file*)) "input.txt"))]
  (let [[weight line-rem]
        (loop [weight {}
               [line & line-rem] (line-seq reader)]
          (if (not (str/blank? line))
            (let [a (subs line 0 2) b (subs line 3 5)]
              (recur (update weight a (fn [m] (assoc m b 1))) line-rem))
            [weight line-rem]))]
    (loop [median-total 0
           [line & line-rem] line-rem]
      (if (not (str/blank? line))
        (let [pg-order (->> (str/split line #","))
              correct? (loop [[head & remaining] pg-order]
                         (let [correct? (->> remaining
                                             (map (fn [el]
                                                    (or (get-in weight [head el])
                                                        (- (get-in weight [el head]))
                                                        1)))
                                             (every? #{1}))]
                           (if (and correct? (some? head))
                             (recur remaining)
                             correct?)))
              median (if correct? (get pg-order (quot (count pg-order) 2)) 0)]
          (recur (+ median-total (Integer/valueOf median)) line-rem))
        median-total))))

