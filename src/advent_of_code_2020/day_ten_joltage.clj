(ns advent-of-code-2020.day-ten-joltage)

(def adapters (->> "test1-adapters.txt"
                   slurp
                   clojure.string/split-lines
                   (map #(Integer/parseInt %))
                   set))

(def adapters-2 (->> "test2-adapters.txt"
                   slurp
                   clojure.string/split-lines
                   (map #(Integer/parseInt %))
                   set))

(def real-adapters (->> "adapters.txt"
                     slurp
                     clojure.string/split-lines
                     (map #(Integer/parseInt %))
                     set))

(defn pick-next [jolt adapters]
  (loop [steps [1 2 3]]
    (if (empty? steps)
      false
      (let [next-step (+ (first steps) jolt)
            found     (contains? adapters next-step)]
        (if found
          next-step
          (recur (rest steps)))))))


(defn attach-adapter [adapters]
  (loop [one-jolts    0
         three-jolts  0
         as           adapters
         current-step 0]
    (if (empty? as)
      {:ones   one-jolts
       :threes three-jolts}
      (let [result (pick-next current-step adapters)]
        (if (not result)
          {:ones   one-jolts
           :threes three-jolts}
          (let [step (- result current-step)]
            (condp = step
              1 (recur (inc one-jolts) three-jolts (rest as) result)
              2 (recur one-jolts three-jolts (rest as) result)
              3 (recur one-jolts (inc three-jolts) (rest as) result))))))))

(defn create-new-paths [adapters path]
  (let [jolt (last path)]
    (loop [steps     [1 2 3]
           new-paths []]
      (if (empty? steps)
        new-paths
        (let [next-step (+ (first steps) jolt)
              found     (contains? adapters next-step)]
          (if found
            (recur (rest steps) (conj new-paths (conj path next-step)))
            (recur (rest steps) new-paths)))))))

;; part 2 record every possible track down the memory lane
(defn all-combinations [adapters]
  (let [max (apply max adapters)]
    (loop [legal-paths [[0]]
           valid-paths []]
      (let [current-path (first legal-paths)
            rest-of-path (rest legal-paths)]
        (if (empty? legal-paths)
          valid-paths
          (if (= max (last current-path))
            (recur rest-of-path (conj valid-paths current-path))
            (recur (filter
                     #(not= [] %)
                     (concat rest-of-path (create-new-paths adapters current-path)))
                   valid-paths)))))))


(comment
  (pick-next 0 adapters)
  (pick-next 1 adapters)
  (pick-next 4 adapters)
  (pick-next 5 adapters)
  (pick-next 6 adapters)
  (pick-next 7 adapters)
  (create-new-paths adapters [0])
  (create-new-paths adapters [0 1])
  (create-new-paths adapters [0 1 4])
  (create-new-paths adapters [0 1 4 5])
  (create-new-paths adapters [0 1 4 5 7 10])
  (all-combinations adapters)
  (all-combinations adapters-2)
  (all-combinations real-adapters)
  (attach-adapter adapters))