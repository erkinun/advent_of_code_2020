(ns advent-of-code-2020.day-six-customs
  (:require [advent-of-code-2020.files :refer :all]
            [clojure.string :as str]))

(def test-custom-input (process-file "test-customs.txt" to-str-vec []))
(def custom-input (process-file "customs.txt" to-str-vec []))

(defn per-group [input]
  (let [{:keys [customs cur]} (reduce
                  (fn [{:keys [:customs cur]} line]
                    (if (= "" line)
                      {:customs (conj customs cur) :cur ""}
                      {:customs customs :cur (str cur " " line)}))
                  {:customs [] :cur ""}
                  input)]
    (conj customs cur)))

(defn group-answers [answers]
  (let [as-a-list (-> answers
                      str/trim
                      (str/split #" "))
        chars-list (map (comp set char-array) as-a-list)
        merged-answers (reduce
                         (fn [chars-set chars]
                           (into chars-set chars))
                         #{}
                         chars-list)]
    (count merged-answers)))

(defn group-answers-intersection [answers]
  (let [as-a-list (-> answers
                      str/trim
                      (str/split #" "))
        chars-list (map (comp set char-array) as-a-list)
        intersection (apply clojure.set/intersection chars-list)]
    (count intersection)))

(defn answer-count [input]
  (let [answers-by-group (per-group input)
        counts-by-group (map group-answers answers-by-group)]
    (reduce + 0 counts-by-group)))

(defn answer-count-intersection [input]
  (->> input
       per-group
       (map group-answers-intersection)
       (reduce + 0)))

(comment
  (group-answers " ab ac")
  (group-answers-intersection "abc")
  (per-group test-custom-input)
  (answer-count test-custom-input)
  (answer-count custom-input)
  (answer-count-intersection custom-input))