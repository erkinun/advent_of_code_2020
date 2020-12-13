(ns advent-of-code-2020.day_thirteen_bus)

(def data (->> "bus-data.txt"
                       slurp
                       clojure.string/split-lines))

(def arrival-ts (-> data first ((fn [n] (Integer/parseInt n)))))
(def bus-times (-> data second (#(clojure.string/split % #","))))
(def valid-times (->> bus-times
                      (filter #(not= "x" %))
                      (map #(Integer/parseInt %))))

(defn earliest-for [arrival bus-id]
  (let [modulo (mod arrival bus-id)
        diff (- bus-id modulo)]
    (+ arrival diff)))

(def departure-times
  (map (fn [bus-id] {:id        bus-id
                     :departure (earliest-for arrival-ts bus-id)})
       valid-times))

(def earliest (first (sort-by :departure departure-times)))

(comment
  (* (:id earliest) (- (:departure earliest) arrival-ts))
  )


