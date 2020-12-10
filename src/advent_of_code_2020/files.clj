(ns advent-of-code-2020.files
  (:import (java.io FileReader BufferedReader)))

(defn process-file [file-name line-func line-acc]
  (with-open [rdr (BufferedReader. (FileReader. file-name))]
    (reduce line-func line-acc (line-seq rdr))))


(defn to-vector [acc line]
  (conj acc (Integer/parseInt line)))

(defn to-str-vec [acc line]
  (conj acc line))
