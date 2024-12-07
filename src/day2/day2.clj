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
                   ((fn [deltas]
                      (and (reduce (fn [a b] (if (= (compare a 0) (compare b 0)) a (reduced false))) deltas)
                           (every? (fn [delta] (and (>= (abs delta) 1) (<= (abs delta) 3))) deltas)))))))
       (map #(every? identity %))
       (filter true?)
       (count)))

;; part 2

;; https://stackoverflow.com/a/18319708/8812880
(defn vec-remove
  "remove elem in coll"
  [coll pos]
  (into (subvec coll 0 pos) (subvec coll (inc pos))))

(with-open [rdr (io/reader (fs/file (.getParent (fs/file *file*)) "input.txt"))]
  (->> (line-seq rdr)
       (mapv (fn [line] (->> (str/split line #"\s+") (map Integer/parseInt))))
       (map (fn [report]
              (->> (partition 2 1 report)
                   (mapv #(apply - %))
                   ((fn [deltas]
                      (boolean
                       (or (and (reduce (fn [a b] (if (= (compare a 0) (compare b 0)) a (reduced false))) deltas)
                                (every? (fn [delta] (and (>= (abs delta) 1) (<= (abs delta) 3))) deltas))
                           (->> (range -1 (count deltas))
                                ;; the idea here is to look all the delta iteration if one element is missing
                                (mapv (fn [idx] (cond (= idx -1) ;; first element missing
                                                      (vec-remove deltas 0)
                                                      (= idx (dec (count deltas))) ;; last element missing
                                                      (vec-remove deltas (dec (count deltas)))
                                                      :else ;; elements in the middle missing
                                                      (-> (update deltas (inc idx) + (nth deltas idx))
                                                          (vec-remove idx)))))
                                (map (fn [deltas']
                                       (and (reduce (fn [a b] (if (= (compare a 0) (compare b 0)) a (reduced false))) deltas')
                                            (every? (fn [delta] (and (>= (abs delta) 1) (<= (abs delta) 3))) deltas'))))
                                (some true?)))))))))
       (filter true?)
       (count)))

(comment
  (->> [[7 6 4 2 1]
        [1 2 7 8 9]
        [9 7 6 2 1]
        [1 3 2 4 5]
        [1 100 2 4 5]
        [100 1 2 4 5]
        [1 2 4 100]
        [8 6 4 4 1]
        [1 3 6 7 9]]
       (mapv (fn [report]
               (->> (partition 2 1 report)
                    (mapv #(apply - %))
                    ((fn [deltas]
                       (->> (range -1 (count deltas))
                            (mapv (fn [idx]
                                    (cond (= idx -1) (vec-remove deltas 0)
                                          (= idx (dec (count deltas))) (vec-remove deltas (dec (count deltas)))
                                          :else (-> (update deltas (inc idx) + (nth deltas idx))
                                                    (vec-remove idx)))))
                            ((juxt
                              (constantly [report deltas])
                              (constantly (and (reduce (fn [a b] (if (= (compare a 0) (compare b 0)) a (reduced false))) deltas)
                                               (every? (fn [delta] (and (>= (abs delta) 1) (<= (abs delta) 3))) deltas)))
                              #(->> %
                                    (map (fn [deltas']
                                           (and (reduce (fn [a b] (if (= (compare a 0) (compare b 0)) a (reduced false))) deltas')
                                                (every? (fn [delta] (and (>= (abs delta) 1) (<= (abs delta) 3))) deltas'))))
                                    (some true?))))
                            #_(some true?))))))))

  (->> [3 2 4 5 100]
       (partition 2 1)
       (mapv #(apply - %))
       ((fn [deltas]
          [deltas
           (->> (range -1 (count deltas))
                (mapv (fn [idx]
                        (cond (= idx -1) (vec-remove deltas 0)
                              (= idx (dec (count deltas))) (vec-remove deltas (dec (count deltas)))
                              :else (-> (update deltas (inc idx) + (nth deltas idx))
                                        (vec-remove idx))))))])))

  (->> [[1 false] [true true]]
       (map #(every? identity %))))
