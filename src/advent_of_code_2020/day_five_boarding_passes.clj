(ns advent-of-code-2020.day-five-boarding-passes
  (:require [advent-of-code-2020.files :refer :all]))

(defn find-part [{:keys [min max]} letter]
  (let [mid (int (/ (- max min) 2))]
    (if (= "F" letter)
      {:min min :max (+ min mid)}
      {:min (+ min (inc mid)) :max max})))

(defn find-seat [{:keys [min max]} letter]
  (let [mid (int (/ (- max min) 2))]
    (if (= "L" letter)
      {:min min :max (+ min mid)}
      {:min (+ min (inc mid)) :max max})))

;; TODO unify the functions
(defn find-row [part-str]
  (let [steps (clojure.string/split part-str #"")
        {:keys [min max]} (reduce find-part {:min 0 :max 127} steps)]
    (if (not= min max)
      (throw (ex-info "Error: min max are not same" min max))
      min)))

(defn seat-finder [seat-str]
  (let [steps (clojure.string/split seat-str #"")
        {:keys [min max]} (reduce find-seat {:min 0 :max 7} steps)]
    (if (not= min max)
      (throw (ex-info "Error: min max are not same" min max))
      min)))

(defn seat-id [boarding-pass]
  (let [row-part (subs boarding-pass 0 7)
        seat-part (subs boarding-pass 7)
        row (find-row row-part)
        seat (seat-finder seat-part)]
    (+ seat (* 8 row))))

(def boarding-passes (process-file "boarding-passes.txt" to-str-vec []))
(def seat-ids (sort (map seat-id boarding-passes)))
(apply max seat-ids)