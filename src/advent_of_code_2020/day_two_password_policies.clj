(ns advent-of-code-2020.day-two-password-policies
  (:require [advent-of-code-2020.files :refer :all]))

(def policies (set (process-file "pass_policies.txt" to-str-vec [])))

(defn is-pass-valid [min max char password]
  (let [c (first (char-array char))
        mi (Integer/parseInt min)
        ma (Integer/parseInt max)
        num (->> password (filter #(= c %)) count)]
    (and (>= num mi) (<= num ma))))

(defn tabogan-policy [first-char last char password]
  (let [c (first (char-array char))
        f (- (Integer/parseInt first-char) 1)
        l (- (Integer/parseInt last) 1)
        char-at-f (nth password f)
        char-at-l (nth password l)]
    (cond
      (and (= c char-at-f) (= c char-at-l)) false
      (and (not= c char-at-f) (not= c char-at-l)) false
      :else true)))

(def corrects (filter (fn [policy-line]
                        (let [[min max char pass] (rest (re-matches #"(\d+)-(\d+) (\S): (\w+)" policy-line))]
                          (is-pass-valid min max char pass))) policies))

(def wrongs (filter (fn [policy-line]
                        (let [[min max char pass] (rest (re-matches #"(\d+)-(\d+) (\S): (\w+)" policy-line))]
                          (not (is-pass-valid min max char pass)))) policies))

(def correct-tabogan (filter (fn [policy-line]
                        (let [[min max char pass] (rest (re-matches #"(\d+)-(\d+) (\S): (\w+)" policy-line))]
                          (tabogan-policy min max char pass))) policies))