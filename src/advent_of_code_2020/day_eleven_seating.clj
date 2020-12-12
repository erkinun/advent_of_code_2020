(ns advent-of-code-2020.day-eleven-seating)

(def floor-plan (->> "test-seating.txt"
                     slurp
                     clojure.string/split-lines
                     (map #(clojure.string/split % #""))))

(def real-plan (->> "seating.txt"
                    slurp
                    clojure.string/split-lines
                    (map #(clojure.string/split % #""))))

(defn print-plan [plan]
  (->> plan
       (map #(clojure.string/join %))
       vec))

;; TODO this func perf can be improved and maybe even memoized
;; TODO the filter below could then be removed
(defn adjacent-indexes [x y]
  (for [xi (range (- x 1) (+ x 2))
        yi (range (- y 1) (+ y 2))]
    [xi yi]))

(defn seat [plan xi yi] (-> plan (nth yi) (nth xi)))

(defn adjacent-seats [plan x y]
  (let [width    (-> plan first count)
        height   (count plan)
        adj-is   (adjacent-indexes x y)
        valid-is (filter (fn [[xi yi]]
                           (cond
                             (neg-int? xi) false
                             (neg-int? yi) false
                             (>= yi height) false
                             (>= xi width) false
                             (and (= x xi) (= y yi)) false
                             :else true)) adj-is)]
    (->> valid-is
         (map (fn [[xi yi]] (-> plan (nth yi) (nth xi)))))))

(def mem-adjacent-seats (memoize adjacent-seats))

(defn seat-state [seat adj-seats]
  (let [not-occupied (not (some #(= "#" %) adj-seats))
        occupied     (->> adj-seats (filter #(= "#" %)) count (<= 4))]
    (cond
      (and (= "L" seat) not-occupied) "#"
      (and (= "#" seat) occupied) "L"
      :else seat)))

(defn next-floor [fl]
  (for [y (range 0 (count fl))]
    (for [x (range 0 (-> fl first count))]
      (let [next-seat (seat-state (seat fl x y) (adjacent-seats fl x y))]
        next-seat))))

(def mem-next-floor (memoize next-floor))

(defn floors-same [f1 f2]
  (let [width      (-> f1 first count)
        height     (count f1)
        seat-eqs   (for [x (range 0 width)]
                     (for [y (range 0 height)]
                       (let [s1 (seat f1 x y)
                             s2 (seat f2 x y)]
                         (= s1 s2))))
        any-false? (some false? (apply concat seat-eqs))]
    (if any-false?
      false
      true)))

(defn move-seats [floor]
  (loop [tries      0
         same-floor false
         current    floor]
    (if same-floor
      current
      (let [next (mem-next-floor current)
            ;_    (prn "tries: " tries)
            ]
        (recur (inc tries) (floors-same current next) next)))))

(defn occupied-row [row]
  (reduce (fn [acc seat] (+ acc (if (= "#" seat) 1 0))) 0 row))

(defn occupied-seats [floor-plan]
  (reduce (fn [acc row] (+ acc (occupied-row row))) 0 floor-plan))

(comment
  (print-plan floor-plan)
  (adjacent-seats floor-plan 0 0)
  (adjacent-seats floor-plan 0 9)
  (adjacent-seats floor-plan 9 0)
  (adjacent-seats floor-plan 9 9)
  (time (mem-adjacent-seats floor-plan 5 5))
  (seat-state "L" (adjacent-seats floor-plan 0 0))
  (seat-state "#" '("." "." "." "#" "#" "#" "#" "L"))
  (seat-state "L" '("." "." "." "#" "#" "#" "L" "L"))
  (seat-state "." (adjacent-seats floor-plan 0 0))
  (seat-state "#" (adjacent-seats (next-floor floor-plan) 0 4))
  (time (print-plan
          (-> floor-plan
              next-floor
              next-floor
              next-floor)))
  (print-plan (next-floor floor-plan))
  (print-plan (next-floor (next-floor floor-plan)))
  (time (mem-next-floor floor-plan))
  (time (next-floor floor-plan))
  (floors-same floor-plan floor-plan)
  (time (floors-same floor-plan (next-floor floor-plan)))
  (print-plan (move-seats floor-plan))
  (time (move-seats floor-plan))
  (time (occupied-seats (move-seats floor-plan)))


  (time (let [next-floor (next-floor real-plan)]
          "done"))
  (time (let [next-floor (mem-next-floor real-plan)]
          "done"))
  (occupied-seats (move-seats real-plan))
  (def result (move-seats real-plan))

  (time (floors-same real-plan real-plan))
  (time (floors-same real-plan (next-floor real-plan)))
  (+ 1 2)
  )