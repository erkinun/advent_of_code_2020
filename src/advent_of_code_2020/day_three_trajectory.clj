(ns advent-of-code-2020.day-three-trajectory
  (:require [advent-of-code-2020.files :refer :all]))

(def map-toboggan (process-file "map.txt" to-str-vec []))
(def test-map (process-file "test-map.txt" to-str-vec []))

(def width (-> map-toboggan first count))

(defn tree [row x]
  (= \# (nth row x)))

(def initial
  {:step 0
   :x 0
   :num-tree 0})

;; TODO handle down-step
(defn trees-by-slopes [map-itself right-step down-step]
  (let [initial {:step     1
                 :x        0
                 :num-tree 0}
        width (-> map-itself first count)]
    (reduce (fn [{:keys [step x num-tree]} next-row]
              (if (= 0 (dec step))
                (let [is-tree      (tree next-row x)
                      cur-down     (dec step)
                      cur-num-tree (if (and is-tree (= 0 cur-down))
                                     (inc num-tree)
                                     num-tree)]
                  {:step     (if (= 0 cur-down)
                               down-step
                               cur-down)
                   :x        (mod (+ right-step x) width)
                   :num-tree cur-num-tree})
                {:step (dec step)
                 :x x
                 :num-tree num-tree
                 })) initial map-itself)))

(def number-of-trees (trees-by-slopes map-toboggan 3 1))