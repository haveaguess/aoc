(ns y2021.core
  (:require [clojure.edn :as edn]
            [clojure.set :as set]
            [clojure.string :as str]))

;; 202101
(let [data (->> (slurp "src/y2021/input202101") (re-seq #"\d+") (map edn/read-string))
      f (fn [n] (->> (partition n 1 data) (keep #(if (< (nth % 0) (nth % (dec n))) 1)) count))]
  (map f [2 4]))
;; (1709 1761)


;; 202102
(let [data (->> (slurp "src/y2021/input202102") (re-seq #"\w+") (partition 2))
      f (fn [r [dir offset]]
          (let [offset (edn/read-string offset)]
            (condp = dir
              "forward" (update r :x + offset)
              "down"    (update r :y - offset)
              "up"      (update r :y + offset))))
      g (fn [r [dir offset]]
          (let [offset (edn/read-string offset)]
            (condp = dir
              "forward" (-> r (update :x + offset) (update :y + (* offset (:aim r))))
              "down"    (update r :aim + offset)
              "up"      (update r :aim - offset))))
      q (map #(reduce % {:x 0, :y 0, :aim 0} data) [f g])]
  (map #(* (:x %) (:y %)) q))
;; (-1524750 1592426537)


;; 202103
(let [data (->> (slurp "src/y2021/input202103") (re-seq #"\w+"))
      data1 (->> (apply map #(frequencies %&) data)
                 (map #(let [one (% \1), zero (% \0)] (if (< one zero) \0 \1)))
                 str/join)
      gamma (Integer/parseInt data1 2)
      epsilon (- (dec (bit-shift-left 1 (count data1))) gamma)
      f (fn [data f]
          (loop [data data, offset 0]
            (if (= 1 (count data))
              (Integer/parseInt (first data) 2)
              (let [{one "1", zero "0"} (->> data (map #(subs % offset (inc offset))) frequencies)
                    i (if (f one zero) "1" "0")]
                (recur (filter #(= i (subs % offset (inc offset))) data) (inc offset))))))]
  [(* gamma epsilon)
   (->> [>= <] (map (partial f data)) (reduce *))])
;; [3813416 2990784]


;; 202104
(let [data (->> (slurp "src/y2021/input202104") (re-seq #"[^\n]+"))
      f (fn [board]
          (let [x (map set (partition 5 board))
                y (apply map hash-set (partition 5 board))]
            (into x y)))
      g (fn [board drawn n] (* (reduce + (set/difference (apply set/union board) drawn)) n))
      draws (->> (first data) (re-seq #"\d+") (map edn/read-string))
      boards (->> (rest data) (mapcat #(re-seq #"\d+" %)) (map edn/read-string) (partition 25) (map f))]
  (->> (reduce (fn [[drawn boards score] n]
                 (let [drawn (conj drawn n)
                       winner-boards (filter (fn [board] (some #(set/subset? % drawn) board)) boards)
                       left-boards (remove (fn [board] (some #(set/subset? % drawn) board)) boards)]
                   [drawn left-boards (into score (map #(g % drawn n) winner-boards))]))
               [(set (take 4 draws)) boards []]
               (drop 4 draws))
       peek
       ((juxt first peek))))
;; [82440 20774]


;; 202105
(let [data (->> (slurp "src/y2021/input202105") (re-seq #"\d+") (map edn/read-string) (partition 4))
      f (fn [r [x1 y1 x2 y2]]
          (reduce #(update % %2 (fn [x] (if x (inc x) 1)))
                  r
                  (cond (= x1 x2) (for [y (cond (<= y1 y2) (range y1 (inc y2))
                                                :else      (range y2 (inc y1)))]
                                    [x1 y])
                        (= y1 y2) (for [x (cond (<= x1 x2) (range x1 (inc x2))
                                                :else      (range x2 (inc x1)))]
                                    [x y1]))))
      g (fn [r [x1 y1 x2 y2]]
          (reduce #(update % %2 (fn [x] (if x (inc x) 1)))
                  r
                  (if (and (not= x1 x2) (not= y1 y2))
                    (map vector
                         (for [x (range x1 (if (< x1 x2) (inc x2) (dec x2)) (if (< x1 x2) 1 -1))] x)
                         (for [y (range y1 (if (< y1 y2) (inc y2) (dec y2)) (if (< y1 y2) 1 -1))] y)))))
      q1 (reduce f {} data)
      q2 (reduce g q1 data)]
  (map #(->> % vals (remove #{1}) count) [q1 q2]))
;; (6007 19349)


;; 202106
(let [data (->> (slurp "src/y2021/input202106") (re-seq #"\d+") (map edn/read-string) frequencies)
      f (fn [data]
          (reduce (fn [r [timer n]]
                    (cond (= timer 0)
                          (-> r (assoc 8 n) (update 6 #(+ n (or % 0))))
                          (= timer 7)
                          (update r 6 #(+ n (or % 0)))
                          :else
                          (assoc r (dec timer) n)))
                  {}
                  data))]
  (map #(->> (nth (iterate f data) %) vals (reduce +)) [80 256]))
;; (387413 1738377086345)


;; 202107
(let [data (->> (slurp "src/y2021/input202107") (re-seq #"\d+") (map edn/read-string) sort)
      m0 (first data)
      m1 (last data)
      f0 #(Math/abs (- % %2))
      f1 #(let [k (Math/abs (- % %2))] (/ (* k (inc k)) 2))]
  (for [f [f0 f1]]
    (->> (for [x (range m0 (inc m1))]
           (reduce + (map #(f % x) data)))
         sort
         first)))
;; (356922 100347031)


;; 202108
(let [data (->> (slurp "src/y2021/input202108") (re-seq #"[^\n]+") (map #(re-seq #"\w+" %)))
      f (fn [l6set l5set]
          (into
           (for [l6 l6set
                 :let [l6l5diff (map #(count (set/difference l6 %)) l5set)]]
             [l6 ({[2 2 2] 0, [1 2 2] 6, [1 1 2] 9} (sort l6l5diff))])
           (for [l5 l5set
                 :let [l5l6diff (map #(count (set/difference l5 %)) l6set)]]
             [l5 ({[1 1 1] 2, [0 1 1] 3, [0 0 1] 5} (sort l5l6diff))])))
      g (fn [line]
          (let [x0 (map set (take 10 line))
                l6set (filter #(= 6 (count %)) x0)
                l5set (filter #(= 5 (count %)) x0)
                m (-> (reduce (fn [r [l n]] (assoc r (some #(if (= l (count %)) %) x0) n))
                              {}
                              [[2 1] [4 4] [3 7] [7 8]])
                      (into (f l6set l5set)))]
            (->> line (drop 10) (map set) (map m) (reduce #(+ (* 10 %) %2)))))]
  [(->> data (mapcat #(drop 10 %)) (filter #(#{2 3 4 7} (count %))) count)
   (reduce + (map g data))])
;; [321 1028926]


;; 202109
(let [data (vec (for [line (->> (slurp "src/y2021/input202109") (re-seq #"[^\n]+"))]
                  (->> (re-seq #"\d" line) (mapv edn/read-string))))
      h (count data)
      w (count (first data))
      low-points (for [y (range h), x (range w)
                       :let [v (get-in data [y x])
                             neighbors (for [[oy ox] [[0 1] [0 -1] [1 0] [-1 0]]
                                             :let [nv (get-in data [(+ y oy) (+ x ox)])]
                                             :when nv]
                                         nv)]
                       :when (every? #(< v %) neighbors)]
                   v)
      neighbor (fn [ps points]
                 (set (for [[y x] ps
                            [oy ox] [[0 1] [0 -1] [1 0] [-1 0]]
                            :let [p [(+ y oy) (+ x ox)]]
                            :when (points p)]
                        p)))
      basin (fn [basin-points]
              (loop [points basin-points
                     ps (hash-set (first points))]
                (let [ps-neighbor (neighbor ps points)]
                  (if (seq ps-neighbor)
                    (recur (set/difference points ps-neighbor) (set/union ps ps-neighbor))
                    ps))))]
  [(+ (reduce + low-points) (count low-points))
   (->> (loop [basin-points (set (for [y (range h), x (range w)
                                       :when (not= 9 (get-in data [y x]))]
                                   [y x]))
               basin-count []]
          (if (seq basin-points)
            (let [b (basin basin-points)]
              (recur (set/difference basin-points b) (conj basin-count (count b))))
            basin-count))
        (sort >)
        (take 3)
        (reduce *))])
;; [489 1056330]


;; 202110
(let [m1 {\( \), \{ \}, \[ \], \< \>}
      m2 {\) 3, \] 57, \} 1197, \> 25137}
      m3 {\) 1, \] 2, \} 3, \> 4}
      f (fn [line]
          (reduce #(cond (= (m1 (first %)) %2) (rest %)
                         (m1 %2) (cons %2 %)
                         :else (reduced %2))
                  (list (first line))
                  (rest line)))
      g (fn [xs] (reduce #(+ (* 5 %) (m3 (m1 %2))) 0 xs))
      data (->> (slurp "src/y2021/input202110") (re-seq #"[^\n]+") (map f))]
  [(->> data (remove seq?) (map m2) (reduce +))
   (let [data1 (->> (filter seq? data) (map g) sort)]
     (nth data1 (quot (count data1) 2)))])
;; [315693 1870887234]


;; 202111
(let [data (vec (for [line (->> (slurp "src/y2021/input202111") (re-seq #"[^\n]+"))]
                  (->> (re-seq #"\d" line) (mapv edn/read-string))))
      h (count data)
      w (count (first data))
      total (* h w)
      g0 #(update-in % %2 inc)                      ;; inc energy
      g1 #(assoc-in % %2 0)                         ;; 9+ becomes 0
      g2 #(set (for [y (range h), x (range w)       ;; find all 9+
                     :when (< 9 (get-in % [y x]))]
                 [y x]))
      g3 #(for [[y x] %                             ;; find adjacent octopuses of 9+
                yo [-1 0 1], xo [-1 0 1]
                :when (not= xo yo 0)
                :let [y' (+ y yo), x' (+ x xo)]
                :when (and (< -1 y' h) (< -1 x' w))]
            [y' x'])
      f (fn [{a0 :data cnt :cnt}]
          (let [a1 (reduce g0 a0 (for [y (range h), x (range w)] [y x]))
                a2 (loop [a1 a1
                          flash0 #{}
                          flash1 (g2 a1)]
                     (if-let [flashd (seq (set/difference flash1 flash0))]
                       (let [a1' (reduce g0 a1 (g3 flashd))]
                         (recur a1' flash1 (g2 a1')))
                       {:data a1, :cnt (+ cnt (count flash0))}))
                a3 (reduce g1 (:data a2) (g2 (:data a2)))]
            (assoc a2 :data a3)))]
  [(:cnt (nth (iterate f {:data data, :cnt 0}) 100))
   (->> (iterate f {:data data, :cnt 0})
        (map :cnt)
        (partition 2 1)
        (keep-indexed (fn [n [x0 x1]] (if (= total (- x1 x0)) (inc n))))
        first)])
;; [1688 403]


;; 202112
(let [f (fn [r [v1 v2]]
          (-> r
              (update v1 #(if (seq %) (conj % v2) [v2]))
              (update v2 #(if (seq %) (conj % v1) [v1]))))
      data (->> (slurp "src/y2021/input202112") (re-seq #"\w+") (partition 2) (reduce f {}))
      small-caves (->> data keys (filter #(and (not (#{"start" "end"} %)) (re-find #"[a-z]" %))))
      g (fn [v]
          (let [m (frequencies v)]
            (some #(= 2 (m %)) small-caves)))
      h (fn [f]
          (fn [{:keys [vs r]}]
            (let [t (for [v vs
                          neighbor (data (first v))]
                      (cond (= "start" neighbor) nil
                            (= "end" neighbor) {:r 1}
                            (and (re-find #"[a-z]" neighbor)
                                 ((set v) neighbor)
                                 (f v)) nil
                            :else {:vs (cons neighbor v)}))]
              {:vs (->> (keep :vs t) (reduce conj []))
               :r (->> (keep :r t) count (+ r))})))]
  (for [f [identity g]]
    (->> (iterate (h f) {:vs ['("start")] :r 0})
         (drop-while #(seq (:vs %)))
         first
         :r)))
;; (5252 147784)


;; 202113
(let [data (->> (slurp "src/y2021/input202113") (re-seq #"\d+|x|y") (map edn/read-string) (partition 2))
      f (fn [{:keys [s r] :as m} [x y]]
          (cond (= x 'y) (let [s' (into #{} (for [[x0 y0] s
                                                  :when (not= y0 y)]
                                              [x0 (cond (< y0 y) y0
                                                        :else (+ y y (- y0)))]))]
                           {:s s', :r (conj r s')})
                (= x 'x) (let [s' (into #{} (for [[x0 y0] s
                                                  :when (not= x0 x)]
                                              [(cond (< x0 y) x0
                                                     :else (+ y y (- x0))) y0]))]
                           {:s s', :r (conj r s')})
                :else (update m :s conj [x y])))
      r (:r (reduce f {:s #{}, :r []} data))
      x (->> (map first (last r)) (reduce max))
      y (->> (map second (last r)) (reduce max))
      a (reduce (fn [a [x y]] (assoc-in a [y x] "▓"))
                (vec (repeat (inc y) (vec (repeat (inc x) "░"))))
                (last r))]
  [(count (first r))
   (dorun (map #(println (str/join %)) a))])
;; [775 nil]
;; ▓▓▓░░▓▓▓▓░▓░░▓░▓▓▓░░▓░░▓░▓▓▓░░▓░░▓░▓▓▓░
;; ▓░░▓░▓░░░░▓░░▓░▓░░▓░▓░░▓░▓░░▓░▓░▓░░▓░░▓
;; ▓░░▓░▓▓▓░░▓░░▓░▓░░▓░▓░░▓░▓░░▓░▓▓░░░▓░░▓
;; ▓▓▓░░▓░░░░▓░░▓░▓▓▓░░▓░░▓░▓▓▓░░▓░▓░░▓▓▓░
;; ▓░▓░░▓░░░░▓░░▓░▓░░░░▓░░▓░▓░░░░▓░▓░░▓░▓░
;; ▓░░▓░▓▓▓▓░░▓▓░░▓░░░░░▓▓░░▓░░░░▓░░▓░▓░░▓


;; 202114
;; track pairs instead
(let [[s0 & rules] (->> (slurp "src/y2021/input202114") (re-seq #"\w+"))
      rules (->> (for [[s d] (partition 2 rules)] [(seq s) (first d)]) (into {}))
      m (->> (partition 2 1 s0) frequencies)
      f (fn [m]
          (reduce (fn [r [x cnt]]
                    (update r x #(+ (or % 0) cnt)))
                  {}
                  (for [[[c1 c2 :as c] cnt] m
                        x (partition 2 1 [c1 (rules c) c2])]
                    [x cnt])))
      g (fn [m]
          (reduce (fn [r [[c1 c2] cnt]]
                    (-> r (update c1 #(+ (or % 0) cnt)) (update c2 #(+ (or % 0) cnt))))
                  {(first s0) 1, (last s0) 1}
                  m))
      h (fn [m] (let [f #(fn [m] (-> (apply % val m) val (/ 2)))]
                  (reduce - (map #((f %) m) [max-key min-key]))))]
  (map #(-> (iterate f m) (nth %) g h) [10 40]))
;; (2745 3420801168962)


;; 202121
;; I want to mention very nice solution to problem 2 here:
;; https://github.com/zelark/AoC-2021/blob/main/src/zelark/aoc_2021/day_21.clj
;; https://github.com/nbardiuk/adventofcode/blob/master/2021/src/day21.clj
(let [[_ p1 _ p2] (->> (slurp "src/y2021/input202121") (re-seq #"\d+") (map edn/read-string))
      normal-dice (cycle (range 1 101))
      dirac-dice (frequencies
                  (for [i (range 1 4)
                        j (range 1 4)
                        k (range 1 4)]
                    (+ i j k)))
      f0 (fn [player-state]
           (for [[pos m] player-state
                 [score n1] m
                 [offset n2] dirac-dice
                 :let [new-pos (mod (+ pos offset) 10)]]
             [new-pos (+ score new-pos 1) (* n1 n2)]))
      f1 (fn [pos-state]
           (reduce (fn [m [pos score n]]
                     (update-in m [pos score] #(+ n (or % 0))))
                   {}
                   pos-state))
      f2 (fn [pos-state]
           (for [[pos m] pos-state
                 [score n] m
                 :when (<= 21 score)]
             [pos score n]))
      f3 (fn [pos-state win-pos]
           (reduce (fn [r [pos score n]]
                     (update r pos dissoc score))
                   pos-state
                   win-pos))]
  [;; state is a map of position to score
   (loop [state {0 [(dec p1) 0], 1 [(dec p2) 0]}
          player 0
          n 0
          dice normal-dice]
     (let [[pos score] (state player)
           new-pos (mod (reduce + pos (take 3 dice)) 10)
           new-score (+ score new-pos 1)]
       (if (<= 1000 new-score) (* (second (state (- 1 player))) (+ n 3))
           (recur (assoc state player [new-pos new-score]) (- 1 player) (+ n 3) (drop 3 dice)))))
   ;; state is a map of position to score to occurence. Initial position has score of 0 and occurence of 1
   (loop [state {0 {(dec p1) {0 1N}}, 1 {(dec p2) {0 1N}}}
          win {0 0, 1 0}
          player 0]
     (let [pos-state (f1 (f0 (state player)))
           win-pos (f2 pos-state)
           pos-state (f3 pos-state win-pos)
           multiplier (->> (state (- 1 player)) vals (mapcat vals) (reduce +))
           win (update win player + (* multiplier (reduce + (map #(nth % 2) win-pos))))]
       (if (empty? (mapcat val pos-state))
         (reduce max (vals win))
         (recur (assoc state player pos-state) win (- 1 player)))))])
;; [504972 446968027750017N]


;; 202125
(let [data0 (slurp "src/y2021/input202125")
      data1 (->> data0 (re-seq #"[^\n]+") (map #(re-seq #"." %)))
      data2 (vec (re-seq #"." data0))
      h (count data1)
      w (count (first data1))
      f0 (fn [data points]
           (reduce (fn [r [[p1 v1] [p2 v2]]]
                     (-> r (assoc p1 v1) (assoc p2 v2)))
                   data
                   points))
      f1 (fn [data]
           (f0 data (for [y (range h), x (range w)
                          :let [i (+ x (* y w))
                                k (if (= x (dec w))
                                    (and (= ">" (data i)) (= "." (data (- (inc i) w))) [[i "."] [(- (inc i) w) ">"]])
                                    (and (= ">" (data i)) (= "." (data (inc i)))       [[i "."] [(inc i) ">"]]))]
                          :when k]
                      k)))
      f2 (fn [data]
           (f0 data (for [y (range h), x (range w)
                          :let [i (+ x (* y w))
                                k (if (= y (dec h))
                                    (and (= "v" (data i)) (= "." (data x))       [[i "."] [x "v"]])
                                    (and (= "v" (data i)) (= "." (data (+ i w))) [[i "."] [(+ i w) "v"]]))]
                          :when k]
                      k)))]
  (loop [data data2
         n 1]
    (let [datax (f2 (f1 data))]
      (if (= data datax) n (recur datax (inc n))))))
;; 426
