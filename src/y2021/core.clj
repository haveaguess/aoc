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
      data1 (mapcat #(drop 10 %) data)
      f (fn [l6set l5set]
          (into
           (for [l6 l6set
                 :let [l6l5diff (sort (map #(count (set/difference l6 %)) l5set))]]
             (cond
               (= [2 2 2] l6l5diff) [l6 0]
               (= [1 2 2] l6l5diff) [l6 6]
               (= [1 1 2] l6l5diff) [l6 9]))
           (for [l5 l5set
                 :let [l5l6diff (sort (map #(count (set/difference l5 %)) l6set))]]
             (cond
               (= [1 1 1] l5l6diff) [l5 2]
               (= [0 1 1] l5l6diff) [l5 3]
               (= [0 0 1] l5l6diff) [l5 5]))))
      g (fn [line]
          (let [x0 (map set (take 10 line))
                l6 (filter #(= 6 (count %)) x0)
                l5 (filter #(= 5 (count %)) x0)
                m (-> (reduce (fn [r [l n]] (assoc r (some #(if (= l (count %)) %) x0) n))
                              {}
                              [[2 1] [4 4] [3 7] [7 8]])
                      (into (f l6 l5)))]
            (->> line (drop 10) (map set) (map m) (str/join) Integer/parseInt)))]
  [(count (filter #(#{2 3 4 7} (count %)) data1))
   (reduce + (map g data))])
;; [321 1028926]
