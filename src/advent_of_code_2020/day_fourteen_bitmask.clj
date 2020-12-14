(ns advent-of-code-2020.day-fourteen-bitmask
  (:require [clojure.string :as str]))

(def program (->> "test-program.txt"
               slurp
               clojure.string/split-lines))

(def real-program (->> "program.txt"
                  slurp
                  clojure.string/split-lines))

(def computer (atom {:mask nil
                     :memory {}}))

(defn bit-mask [mask num]
  (let [x->zero (-> mask (str/replace "X" "0") ((fn [n] (Long/parseLong n 2))))
        x->one  (-> mask (str/replace "X" "1") ((fn [n] (Long/parseLong n 2))))]
    (-> num (bit-or x->zero) (bit-and x->one))))

(defn compute-line! [line]
  (let [mask-matcher #"mask = (\w+)"
        instr-matcher #"mem\[(\d+)\] = (\d+)"
        is-mask? (re-matches mask-matcher line)]
    (if is-mask?
      (swap! computer assoc :mask (second is-mask?))
      (let [[_ addr value] (re-matches instr-matcher line)
            masked-val (bit-mask (:mask @computer) (Integer/parseInt value))]
        (swap! computer update-in [:memory] assoc addr masked-val)))))

(defn execute [pgrm]
  (loop [pg pgrm]
    (if (empty? pg)
      (->> @computer :memory vals (reduce + 0))
      (do
        (compute-line! (first pg))
        (recur (rest pg))))))


(comment

  (bit-mask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X" 11)
  (bit-mask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X" 101)
  (bit-mask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X" 0)
  (compute-line! "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X")
  (compute-line! "mem[8] = 11")
  (compute-line! "mem[7] = 101")
  (compute-line! "mem[8] = 0")
  (execute program)
  (execute real-program))