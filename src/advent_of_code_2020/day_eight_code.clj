(ns advent-of-code-2020.day-eight-code)

(def instructions (-> "code.txt" slurp clojure.string/split-lines))

(def global-acc (atom 0))

(defn parse-cmd [cmd-str]
  (let [[_ cmd op val] (re-matches #"(\w+) (\+|\-)(\d+)" cmd-str)]
    {:cmd cmd :op op :val val}))

(defn op-val [op val]
  (let [num (Integer/parseInt val)]
    (if (= "-" op)
      (- 0 num)
      num)))

(defn execute [{:keys [cmd op val]}]
  (condp = cmd
    "nop" {:next 1}
    "jmp" {:next (op-val op val)}
    "acc" (let [value (op-val op val)]
            (swap! global-acc + value)
            {:next 1})))

(defn execute-program [instructions]
  (reset! global-acc 0)
  (loop [cur-index 0
         visited-indexes [0]]
    (if (>= cur-index (count instructions))
      "boot complete"
      (let [cur-cmd (nth instructions cur-index)
            cur-execution (execute cur-cmd)
            next-index (+ cur-index (:next cur-execution))]
        (if (contains? (set visited-indexes) next-index)
          visited-indexes
          (recur
            next-index
            (conj visited-indexes next-index)))))))

(defn swap-cmd [{:keys [cmd op val] :as cmd-map}]
  (condp = cmd
    "nop" {:cmd "jmp" :op op :val val}
    "jmp" {:cmd "nop" :op op :val val}
    "acc" cmd-map))

(defn fix-program [instructions]
  (let [parsed-instr (mapv parse-cmd instructions)
        result (execute-program parsed-instr)]
    (if (= "boot complete" result)
      @global-acc
      (loop [tries result]
        (if (empty? tries)
          "nothing to try"
          (let [try-index (first tries)
                swapped (->> try-index (nth parsed-instr) swap-cmd)
                fixed-instr (assoc parsed-instr try-index swapped)
                new-result (execute-program fixed-instr)]
            (if (= "boot complete" new-result)
              @global-acc
              (recur (rest tries)))))))))
