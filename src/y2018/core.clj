(ns y2018.core)

(defn aoc201801 []
  (let [l (->>(re-seq #"\S+" (slurp "y2018/input201801"))
              (map #(Integer/parseInt %)))]
    [(reduce + l)
     (loop [l (cycle l)
            s 0
            r #{}]
       (let [t (+ s (first l))]
         (if (r t)
           t
           (recur (rest l) t (conj r t)))))]))
;; (aoc201801)
;; [508 549]

(defn aoc201802 []
  (let [lines (re-seq #"\S+" (slurp "y2018/input201802"))
        f (fn [n]
            (reduce +
                    (map (fn [x] (if (seq (->> (frequencies x)
                                               (filter #(= n (second %)))))
                                   1
                                   0)) lines)))
        d2 (f 2)
        d3 (f 3)]
    [(* d2 d3)
     (->> (for [a lines
                b lines
                :when (not= a b)]
            (if (= 1 (reduce + (map #(if (= %1 %2) 0 1) a b)))
              [a b]))
          (remove nil?)
          first
          (apply map #(if (= %1 %2) %1 ""))
          (apply str))]))
;; (aoc201802)
;; [6972 "aixwcbzrmdvpsjfgllthdyoqe"]

(defn aoc201803 []
  (let [f (fn [[id x y w h]]
            (for [a (range x (+ x w))
                  b (range y (+ y h))]
              [a b]))
        separate? (fn [[_ x11 y11 x12 y12] [_ x21 y21 x22 y22]]
                    (let [xa (max x11 x21)
                          ya (max y11 y21)
                          xb (min x12 x22)
                          yb (min y12 y22)]
                      (not (and (<= x11 xa x12)
                                (<= x21 xa x22)
                                (<= y11 ya y12)
                                (<= y21 ya y22)
                                (<= x11 xb x12)
                                (<= x21 xb x22)
                                (<= y11 yb y12)
                                (<= y21 yb y22)))))
        lines (->> (re-seq #"\d+" (slurp "y2018/input201803"))
                   (map #(Integer/parseInt %))
                   (partition 5))
        lines2 (map (fn [[id x y w h]] [id x y (+ x w -1) (+ y h -1)]) lines)]
    [(->> (map f lines)
          (apply concat)
          frequencies
          (filter #(< 1 (second %)))
          count)
     (->> (for [a lines2]
            (for [b lines2
                  :when (not= a b)]
              (if (separate? a b) a)))
          (drop-while #(some nil? %))
          ffirst
          first)]))
;; (aoc201803)
;; [103482 686]

(defn aoc201804 []
  (let [lines (->> (re-seq #"[^\n]+" (slurp "y2018/input201804"))
                   sort
                   (map #(re-seq #":\d\d|#\d+|falls|wakes" %)))
        ttable (->> (reduce #(let [[minute info] %2]
                               (cond
                                 (= "falls" info)
                                 (assoc %1 :start (Integer/parseInt (subs minute 1)))
                                 (= "wakes" info)
                                 (update %1 :r conj [(:gid %1) (:start %1) (Integer/parseInt (subs minute 1))])
                                 :else
                                 (assoc %1 :gid (Integer/parseInt (subs info 1)))))
                            {:gid 0 :start 0 :r []}
                            lines)
                    :r)
        gid (->> (reduce (fn [result [gid start end]]
                           (merge-with + result {gid (- end start)})) {} ttable)
                 (sort-by val)
                 last
                 first)
        minute (->> (filter #(= gid (first %)) ttable)
                    (map (fn [[_ start end]] (range start end)))
                    flatten
                    frequencies
                    (sort-by val)
                    last
                    first)]
    [(* gid minute)
     (->> (map (fn [[gid start end]] {gid (range start end)}) ttable)
          (reduce #(merge-with concat %1 %2))
          (map (fn [[gid l]]
                 (let [[minutes times] (->> (frequencies l)
                                            (sort-by val)
                                            last)]
                   [times (* gid minutes)])))
          (sort-by first)
          last
          second)]))
;; (aoc201804)
;; [35623 23037]

(defn aoc201805 []
  (let [f (fn [s a]
            (if-let [b (first s)]
              (if (or (= (+ 32 b) a)
                      (= (+ 32 a) b))
                (rest s)
                (cons a s))
              (list a)))
        line (->> (slurp "y2018/input201805")
                  (map int))
        lines (map #(remove % line) (map #(apply hash-set %&) (range 65 91) (range 97 123)))]
    [(count (reduce f (list (first line)) (rest line)))
     (apply min (map #(count (reduce f (list (first %)) (rest %))) lines))]))
;; (aoc201805)
;; [10584 6968]

(defn aoc201806 []
  (let [dots (->> (re-seq #"\d+" (slurp "y2018/input201806"))
                  (map #(Integer/parseInt %))
                  (partition 2))
        x0 (reduce min (map first dots))
        x1 (reduce max (map first dots))
        y0 (reduce min (map second dots))
        y1 (reduce max (map second dots))
        f (fn [a b] (Math/abs (- a b)))
        result (for [a (range x0 (inc x1))]
                 (for [b (range y0 (inc y1))
                       :let [distance (map-indexed (fn [id x] [id (reduce + (map f x [a b]))]) dots)
                             shortest (reduce min (map second distance))
                             shortest-nodes (filter #(= shortest (second %)) distance)]]
                   (if (< 1 (count shortest-nodes)) "." (ffirst shortest-nodes))))
        result (-> (set (concat (first result) (last result) (map first result) (map last result)))
                   (remove (flatten result))
                   frequencies)]
    (->> (sort-by second result)
         last
         second)))
;; (time (aoc201806))
;; [5429]
