(ns advent-of-code-2020.day-four-passports
  (:require [advent-of-code-2020.files :refer :all]
            [clojure.string :as str]))

(def test-pass (process-file "test-passports.txt" to-str-vec []))

(def reduced-passports (reduce
                        (fn [{:keys [passports cur]} line]
                          (if (= "" line)
                            {:passports (conj passports cur) :cur ""}
                            {:passports passports :cur (str cur " " line)}))
                        {:passports [] :cur ""}
                        test-pass))

(def actual-passports (conj (:passports reduced-passports) (:cur reduced-passports)))

(def required-fields #{"byr"
                       "iyr"
                       "eyr"
                       "hgt"
                       "hcl"
                       "ecl"
                       "pid"})

(defn valid-passport [passport]
  (let [key-vals (str/split passport #" ")
        keys     (set (mapv (fn [kv] (first (str/split kv #":"))) key-vals))]
    (every? (fn [field] (contains? keys field)) required-fields)))

(def valid-passes (->> actual-passports
                       (filter valid-passport)
                       count))

(defn valid-passports [file-name]
  (let [pass-lines (process-file file-name to-str-vec [])
        reduced (reduce
                  (fn [{:keys [passports cur]} line]
                    (if (= "" line)
                      {:passports (conj passports cur) :cur ""}
                      {:passports passports :cur (str cur " " line)}))
                  {:passports [] :cur ""}
                  pass-lines)
        actual-passports (conj (:passports reduced) (:cur reduced))]
    (->> actual-passports
         (filter valid-passport)
         count)))