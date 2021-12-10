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