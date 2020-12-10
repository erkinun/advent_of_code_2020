(ns advent-of-code-2020.day-nine-encoding)


(def test-encodings (->> "test-encoding.txt"
                         slurp
                         clojure.string/split-lines
                         (map #(Long/parseLong %))
                         vec))

(def encodings (->> "encoding.txt"
                    slurp
                    clojure.string/split-lines
                    (map #(Long/parseLong %))
                    vec))

(defn is-sum-available [sum vector index look-back]
  (let [stripped-vec (subvec vector (- index look-back) index)]
    (some (fn [n]
            (let [remainder (- sum n)
                  rest      (-> stripped-vec set (disj n))]
              (contains? rest remainder)))
          stripped-vec)))

(defn attack-encoding [encodings look-back]
  (loop [remaining (subvec encodings look-back)
         i         look-back]
    (if (empty? remaining)
      "encoding is fine"
      (let [number (first remaining)
            result (is-sum-available number encodings i look-back)]
        (if (nil? result)
          number
          (recur (rest remaining) (inc i)))))))

;; https://www.geeksforgeeks.org/find-subarray-with-given-sum/
(defn sub-array-for-sum [sum encodings]
  (loop [lower   0
         index   0
         sliding []]
    (if (= index (count encodings))
      sliding
      (let [cur         (nth encodings index)
            running-sum (reduce + 0 sliding)]
        (cond
          (= (+ cur running-sum) sum) (conj sliding cur)
          (> (+ cur running-sum) sum) (do
                                        (recur (inc lower) index
                                               (subvec sliding 1)))
          (< (+ cur running-sum) sum) (recur lower (inc index) (conj sliding cur)))))))

(comment
  (is-sum-available 219 test-encodings 15 5)
  (attack-encoding test-encodings 5)
  (attack-encoding encodings 25)
  (let [result (sub-array-for-sum 41682220 encodings)]
    (+ (apply min result) (apply max result)))
  (sub-array-for-sum 127 test-encodings)
  encodings)
