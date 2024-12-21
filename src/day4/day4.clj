(ns day4.day4
  (:require
   [babashka.fs :as fs]
   [clojure.string :as str]
   [clojure.java.io :as io]))


(with-open [reader (io/reader (fs/file (.getParent (fs/file *file*)) "input.txt"))]
  (->> (loop [scan {} row 0
              [line & line-rem] (line-seq reader)]
         (let [chars
               (loop [scan scan col 0
                      [ch & ch-rem] line]
                 (if (some? ch)
                   (let [[l<-ch  ll _  _  _]  (scan [row (dec col)])
                         [u<-ch  _  uu _  _]  (scan [(dec row) col])
                         [ul<-ch _  _  ul _]  (scan [(dec row) (dec col)])
                         [ur<-ch _  _  _  ur] (scan [(dec row) (inc col)])]
                     (recur (assoc scan [row col]
                                   [ch
                                    (if (or (str/includes? "XMAS" (str ll ch))
                                            (str/includes? "SAMX" (str ll ch)))
                                      (str ll ch)
                                      (str l<-ch ch))
                                    (if (or (str/includes? "XMAS" (str uu ch))
                                            (str/includes? "SAMX" (str uu ch)))
                                      (str uu ch)
                                      (str u<-ch ch))
                                    (if (or (str/includes? "XMAS" (str ul ch))
                                            (str/includes? "SAMX" (str ul ch)))
                                      (str ul ch)
                                      (str ul<-ch ch))
                                    (if (or (str/includes? "XMAS" (str ur ch))
                                            (str/includes? "SAMX" (str ur ch)))
                                      (str ur ch)
                                      (str ur<-ch ch))])
                            (inc col) ch-rem))
                   scan))
               scan  (into scan chars)]
           (if (some? line)
             (recur scan (inc row) line-rem)
             scan)))
       vals
       (map rest) flatten (filter #{"XMAS" "SAMX"}) count))



