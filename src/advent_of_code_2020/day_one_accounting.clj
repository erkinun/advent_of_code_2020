(ns advent-of-code-2020.day-one-accounting
  (:require [advent-of-code-2020.files :refer :all]))

(prn (process-file "expenses.txt" to-vector []))

(defn numbers-sum-to [nums goal]
  (filter (fn [n]
            (let [req (- goal n)]
              (contains? nums req))) nums))

(defn three-numbers-sum-to [nums goal]
  (filter (fn [n]
            (let [req (- goal n)]
              (not-empty (numbers-sum-to nums req)))) nums))

(def numbers (set (process-file "expenses.txt" to-vector [])))