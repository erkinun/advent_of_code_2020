(ns advent-of-code-2020.day-twelve-rain)

(def directions {
                 :east  {:l :north
                         :r :south}
                 :north {:l :west
                         :r :east}
                 :west  {:l :south
                         :r :north}
                 :south {:l :east
                         :r :west}
                 })

(def status (atom {:facing :east
                   :x      0                                ;; west/east
                   :y      0                                ;; north/south
                   }))

(defn forward [value]
  (let [direction (:facing @status)]
    (condp = direction
      :east (swap! status update-in [:x] + value)
      :south (swap! status update-in [:y] - value)
      :west (swap! status update-in [:x] - value)
      :north (swap! status update-in [:y] + value))))

(defn turn [direction value]
  (let [times  (/ value 90)
        facing (:facing @status)]
    (loop [t          times
           cur-facing facing]
      (if (= t 0)
        (swap! status assoc :facing cur-facing)
        (recur (dec t) (-> directions cur-facing direction))))))

(defn process-instruction [inst-str]
  (let [cmd   (subs inst-str 0 1)
        value (Integer/parseInt (subs inst-str 1))]
    (condp = cmd
      "N" (swap! status update-in [:y] + value)
      "S" (swap! status update-in [:y] - value)
      "E" (swap! status update-in [:x] + value)
      "W" (swap! status update-in [:x] - value)
      "F" (forward value)
      "R" (turn :r value)
      "L" (turn :l value))
    @status))

(def test-instructions (->> "test-instructions.txt"
                            slurp
                            clojure.string/split-lines))

(def instructions (->> "instructions.txt"
                       slurp
                       clojure.string/split-lines))

(defn navigate [instructions]
  (loop [is instructions]
    (if (empty? is)
      @status
      (do
        (process-instruction (first is))
        (recur (rest is))))))

(defn manhattan []
  (+ (Math/abs (:x @status))
     (Math/abs (:y @status))))

(comment

  (process-instruction "F10")
  (process-instruction "N3")
  (process-instruction "F7")
  (process-instruction "R90")
  (process-instruction "F11")
  (process-instruction "L180")
  (process-instruction "R90")
  (manhattan)
  (navigate test-instructions)
  (navigate instructions))