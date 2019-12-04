(defn aoc201901 []
  [(->> (clojure.string/split (slurp "input201901") #"\n")
        (map #(Integer/parseInt %))
        (map #(/ (- % (mod % 3) 6) 3))
        (reduce +))
   (->> (clojure.string/split (slurp "input201901") #"\n")
        (map #(Integer/parseInt %))
        (map #(iterate (fn [x] (/ (- x (mod x 3) 6) 3)) %))
        (map #(drop 1 %))
        (map #(take-while pos? %))
        flatten
        (reduce +))])

(defn aoc201902 []
  (let [f (fn [start end]
            (loop [pc 0
                   program (assoc (->> (clojure.string/split (slurp "input201902") #"\n|,")
                                       (mapv #(Integer/parseInt %)))
                                  1 start 2 end)]
              (let [[op op1 op2 op3] (take 4 (drop pc program))]
                (cond
                  (= op 1)
                  (recur (+ pc 4) (assoc program op3 (+ (nth program op1) (nth program op2))))
                  (= op 2)
                  (recur (+ pc 4) (assoc program op3 (* (nth program op1) (nth program op2))))
                  (= op 99)
                  [(first program) (+ (* start 100) end)]))))]
    [(first (f 12 2))
     (->> (for [start (range 100)
                end   (range 100)]
            (f start end))
          (filter #(= 19690720 (first %)))
          first
          second)]))

(defn aoc201903 []
  (let [f (fn [{[x y] :o r :r d :d} [direction & distance]]
            (let [distance (Integer/parseInt (apply str distance))]
              (merge {:d (+ d distance)}
                     (cond
                       (= direction \U)
                       {:o [x (+ y distance)]
                        :r (clojure.set/union
                            (set (for [n (range distance)] (with-meta [x (+ y n 1)] {:d (+ d n 1)})))
                            r)}
                       (= direction \D)
                       {:o [x (- y distance)]
                        :r (clojure.set/union
                            (set (for [n (range distance)] (with-meta [x (- y n 1)] {:d (+ d n 1)})))
                            r)}
                       (= direction \R)
                       {:o [(+ x distance) y]
                        :r (clojure.set/union
                            (set (for [n (range distance)] (with-meta [(+ x n 1) y] {:d (+ d n 1)})))
                            r)}
                       (= direction \L)
                       {:o [(- x distance) y]
                        :r (clojure.set/union
                            (set (for [n (range distance)] (with-meta [(- x n 1) y] {:d (+ d n 1)})))
                            r)}))))
        [line1 line2] (clojure.string/split (slurp "input201903") #"\n")
        l1 (clojure.string/split line1 #",")
        l2 (clojure.string/split line2 #",")
        l1 (:r (reduce f {:d 0 :o [0 0] :r #{}} l1))
        l2 (:r (reduce f {:d 0 :o [0 0] :r #{}} l2))
        marks (clojure.set/intersection l1 l2)]
    [(reduce min (map #(+ (Math/abs (first %)) (Math/abs (second %))) marks))
     (reduce min (map #(+ (:d (meta (l1 %))) (:d (meta (l2 %)))) marks))]))

(defn aoc201904 []
  (let [[start end] (->> (clojure.string/split (slurp "input201904") #"\n|-")
                         (mapv #(Integer/parseInt %)))
        f (fn [s]
            (and
             (apply <= s)
             (some #(= (first %) (second %)) (partition 2 1 s))))
        g (fn [s]
            (and
             (apply <= s)
             (some #(and (= (first %) (second %)) (= 2 (count %))) (partition-by identity s))))]
    (map #(->> (for [n (range start (inc end))]
                 (% (map int (seq (str n)))))
               (filter true?)
               (count)) [f g])))
