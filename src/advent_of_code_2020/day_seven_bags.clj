(ns advent-of-code-2020.day-seven-bags)

; (def contents (-> "test-bags.txt" slurp clojure.string/split-lines))
(def contents (-> "all-bags.txt" slurp clojure.string/split-lines))

(def can-contain (apply merge (map (fn [sentence]
                                     (let [the-bag (first (re-find #"^(\w+ \w+)" sentence))
                                           bags    (map first (re-seq #"(\d \w* \w*)" sentence))]
                                       {the-bag bags})) contents)))

(def to-be-contained
  (reduce-kv (fn [m k v]
               (let [vals (map
                            (comp second
                                  (partial re-matches #"\d+ (\w+ \w+)"))
                            v)]
                 (reduce (fn [acc new-key]
                           (update acc new-key #(conj % k))) m vals))) {} can-contain))

(defn which-bags?
  [bag]
  (loop [found []
         look-for [bag]]
    (if (empty? look-for)
      (set found)
      (let [[f & rest] look-for
            bags (get to-be-contained f)]
        (recur (apply conj found bags)
               (concat rest bags))))))