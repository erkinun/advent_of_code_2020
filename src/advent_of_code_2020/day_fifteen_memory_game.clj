(ns advent-of-code-2020.day-fifteen-memory-game)

(def state (atom {:numbers #{}
                  :when    {}
                  :last    0
                  :turn    1}))


(defn start-number! [{:keys [numbers when _ turn]} num]
  (let [when-num (let [when-vec (get when last)]
                   (if (nil? when-vec)
                     '()
                     when-vec))]
    {:numbers (conj numbers num)
     :when    (assoc when num (conj when-num turn))
     :last    num
     :turn    (inc turn)}))

(defn last-number! [{:keys [numbers when last turn]}]
  (let [when-num          (let [when-vec (get when last)]
                            (if (nil? when-vec)
                              '()
                              when-vec))
        to-be-spoken      (if (= 1 (count when-num))
                            0
                            (let [last            (first when-num)
                                  one-before-last (second when-num)]
                              (- last one-before-last)))
        when-to-be-spoken (get when to-be-spoken)]
    {:numbers (conj numbers to-be-spoken)
     :when    (assoc when to-be-spoken (conj when-to-be-spoken turn))
     :last    to-be-spoken
     :turn    (inc turn)}))

(def test-start-nums [0 3 6])

(defn play-until [n start-nums]
  (let [actual-steps (- n (count start-nums))]
    (doall (for [start-num start-nums]
             (swap! state start-number! start-num)))
    (loop [steps actual-steps]
      (if (= 0 steps)
        @state
        (do
          (swap! state last-number!)
          (recur (dec steps)))))))

(comment

  (swap! state start-number! 0)
  (swap! state start-number! 3)
  (swap! state start-number! 6)
  (swap! state last-number!)
  (:last (play-until 2020 test-start-nums))
  (:last (play-until 2020 [1,3,2]))
  (:last (play-until 2020 [2,1,3]))
  (:last (play-until 2020 [1,2,3]))
  (:last (play-until 2020 [3,1,2]))
  (:last (play-until 2020 [15,12,0,14,3,1]))
  @state
  )