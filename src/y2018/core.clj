(ns y2018.core
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

;; 201801
(let [l (->> (slurp "src/y2018/input201801") (re-seq #"\S+") (map edn/read-string))]
  [(reduce + l)
   (loop [[l & more] (cycle l)
          s 0
          r #{}]
     (let [t (+ s l)]
       (if (r t)
         t
         (recur more t (conj r t)))))])
;; [508 549]


;; 201802
(let [lines (re-seq #"\S+" (slurp "src/y2018/input201802"))
      f (fn [n]
          (->> lines
               (keep (fn [x] (if (seq (->> (frequencies x) (filter #(= n (val %))))) 1)))
               count))
      d2 (f 2)
      d3 (f 3)]
  [(* d2 d3)
   (->> (for [a lines
              b lines
              :when (not= a b)]
          (if (= 1 (reduce + (map #(if (= %1 %2) 0 1) a b)))
            [a b]))
        (some identity)
        (apply map #(if (= %1 %2) %1 ""))
        str/join)])
;; [6972 "aixwcbzrmdvpsjfgllthdyoqe"]


;; 201803
(let [f (fn [[id x y w h]]
          (for [a (range x (+ x w))
                b (range y (+ y h))]
            [a b]))
      separate? (fn [[_ x11 y11 x12 y12] [_ x21 y21 x22 y22]]
                  (or (<= x12 x21)
                      (<= x22 x11)
                      (<= y12 y21)
                      (<= y22 y11)))
      lines (->> (re-seq #"\d+" (slurp "src/y2018/input201803"))
                 (map edn/read-string)
                 (partition 5))
      lines2 (map (fn [[id x y w h]] [id x y (+ x w) (+ y h)]) lines)]
  [(->> (mapcat f lines)
        frequencies
        (filter #(< 1 (val %)))
        count)
   (->> (for [a lines2
              :when (every? identity (for [b lines2
                                           :when (not= a b)]
                                       (if (separate? a b) a)))]
          a)
        ffirst)])
;; [103482 686]


;; 201804
(let [lines (->> (slurp "src/y2018/input201804")
                 (re-seq #".+")
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
      gid (->> ttable
               (reduce (fn [result [gid start end]] (merge-with + result {gid (- end start)})) {})
               (sort-by val)
               last
               first)
      minute (->> (filter #(= gid (first %)) ttable)
                  (mapcat (fn [[_ start end]] (range start end)))
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
        second)])
;; [35623 23037]


;; 201805
(let [f (fn [s a]
          (if-let [b (first s)]
            (if (or (= (+ 32 b) a) (= (+ 32 a) b))
              (rest s)
              (cons a s))
            (list a)))
      g (fn [xs] (count (reduce f (take 1 xs) (rest xs))))
      line (->> (slurp "src/y2018/input201805") (map int))
      lines (->> (map #(hash-set % %2) (range 65 91) (range 97 123)) (map #(remove % line)))]
  [(g line)
   (reduce min (map g lines))])
;; [10584 6968]


;; 201806
;; if distance is larger we need to consider outside of box [[x0,y0] [x1,y1]]
(let [xys (->> (re-seq #"\d+" (slurp "src/y2018/input201806"))
               (map edn/read-string)
               (partition 2))
      [x0 x1] (->> (map first  xys) sort ((juxt first last)))
      [y0 y1] (->> (map second xys) sort ((juxt first last)))
      f (fn [a b] (Math/abs (- a b)))
      result (for [a (range x0 (inc x1))]
               (for [b (range y0 (inc y1))
                     :let [distance (map-indexed (fn [id xy] [id (reduce + (map f xy [a b]))]) xys)
                           shortest (reduce min (map second distance))
                           shortest-nodes (filter #(= shortest (second %)) distance)]]
                 (if (< 1 (count shortest-nodes)) "." (ffirst shortest-nodes))))
      q1 (-> (set (concat (first result) (last result) (map first result) (map last result)))
             (remove (flatten result))
             frequencies)
      q2 (for [a (range x0 (inc x1))
               b (range y0 (inc y1))
               :let [distance (reduce + (map (fn [xy] (reduce + (map f xy [a b]))) xys))]
               :when (< distance 10000)]
           1)]
  [(->> (sort-by val q1) last val)
   (count q2)])
;; [5429 32614]


;; 201807
(let [parse (fn [xs]
              (loop [[a b & more] xs
                     r {}]
                (if a
                  (recur more (if (r a) (update r a conj b) (assoc r a [b])))
                  r)))
      input (->> (slurp "src/y2018/input201807")
                 (re-seq #"tep (.)")
                 (map second))
      m (parse input)
      q1 (loop [pending (set input)
                m m
                r []]
           (if (seq pending)
             (let [vs (set (flatten (vals m)))
                   available (->> (clojure.set/difference pending vs) sort first)]
               (recur (disj pending available)
                      (dissoc m available)
                      (conj r available)))
             (str/join r)))
      q2 (loop [pending (set input)
                m m
                q {}
                r 0]
           (if (or (seq pending) (seq q))
             (let [vs (set (flatten (vals m)))
                   available (->> (clojure.set/difference pending vs) sort (take (- 5 (count q))))
                   q1 (into q (map #(vector % (->> % first char int (+ -4))) available))
                   soonest-second (reduce min (vals q1))
                   done-steps (keep #(if (= soonest-second (val %)) (key %)) q1)]
               (recur (apply disj pending available)
                      (apply dissoc m done-steps)
                      (->> (apply dissoc q1 done-steps)
                           (map #(vector (key %) (- (val %) soonest-second)))
                           (into {}))
                      (+ r soonest-second)))
             r))]
  [q1 q2])
;; ["FHMEQGIRSXNWZBCLOTUADJPKVY" 917]


;; 201808
(let [input (->> (slurp "src/y2018/input201808") (re-seq #"\d+") (mapv edn/read-string))
      x (fn [mode]
          (fn f [offset]
            (let [[n-node n-meta] (subvec input offset)]
              (loop [n 0, child-offset (+ 2 offset), children-meta-value []]
                (if (< n n-node)
                  (let [[child-offset child-value] (f child-offset)]
                    (recur (inc n) child-offset (conj children-meta-value child-value)))
                  [(+ child-offset n-meta)
                   (let [meta-vec (subvec input child-offset (+ child-offset n-meta))]
                     (if (= :meta mode)
                       (+ (reduce + children-meta-value) (reduce + meta-vec))
                       (->> (if (= 0 n-node)
                              meta-vec
                              (map #(get children-meta-value (dec %) 0) meta-vec))
                            (reduce +))))])))))]
  (map #(((x %) 0) 1) [:meta :value]))
;; (40984 37067)


;; 201809
(let [f (fn [players n]
          )
      [players n] (->> (slurp "src/y2018/input201809") (re-seq #"\d+") (map edn/read-string))]
  (f players n))


;; 201810
(let [input (->> (slurp "src/y2018/input201810") (re-seq #"-?\d+") (map edn/read-string) (partition 4))
      cal-range #(let [x (map first %)
                       y (map second %)]
                   (+ (- (reduce max x) (reduce min x)) (- (reduce max y) (reduce min y))))
      f (fn [{:keys [data cnt]}]
          (let [new-data (map (fn [[x y vx vy]] [(+ x vx) (+ y vy) vx vy]) data)]
            (if (< (cal-range data) (cal-range new-data))
              {:cnt cnt :data data :done? :yes}
              {:cnt (inc cnt) :data new-data})))
      draw (fn [{:keys [data cnt]}]
             (let [x (map first data)
                   y (map second data)
                   [x1 x0 y1 y0] [(reduce max x) (reduce min x) (reduce max y) (reduce min y)]
                   x-span (- x1 x0 -1)
                   y-span (- y1 y0 -1)
                   new-data (map (fn [[x y _ _]] [(- y y0) (- x x0)]) data)
                   grid (vec (repeat y-span (vec (repeat x-span "░"))))
                   painted-grid (reduce #(assoc-in % %2 "▓") grid new-data)]
               (mapv #(println (str/join %)) painted-grid)
               cnt))]
  (->> (iterate f {:data input :cnt 0})
       (some #(if (:done? %) %))
       draw))
;; 10003


;; 201811
;; This is the distribution
;; [-5 9147] [-4 8873] [-3 9219] [-2 8848] [-1 9216] [0 8861] [1 9323] [2 8652] [3 9115] [4 8746]
;; since the average is below 0 the bigger the grid the likely smaller the sum of the grid
;; grid size of 15 appears to peak and the system does it as correct answer
;; going up till grid size 300 takes more time
(let [serial (->> (slurp "src/y2018/input201811") edn/read-string)
      power (fn [x y]
              (- (mod (quot (* (+ x 10) (+ serial (* y (+ x 10)))) 100) 10) 5))
      grid (into {} (for [x (range 1 301), y (range 1 301)] [[x y 1] (power x y)]))
      f (fn [grid n]
          (let [v (for [x (range 1 (- 302 n)), y (range 1 (- 302 n))]
                    [[x y n] (+ (grid [x y (dec n)])
                                (reduce + (map grid (for [ay (range y (+ y n))]    [(+ x n -1) ay 1])))
                                (reduce + (map grid (for [ax (range x (+ x n -1))] [ax (+ y n -1) 1]))))])
                max-v (->> (map second v) (reduce max))
                loc (keep #(if (= max-v (second %)) (first %)) v)]
            (println "grid size" n ": max" max-v "at" loc)
            (into grid v)))]
  (reduce f grid (range 2 31 #_301)))
;; grid size 2 : max 16 at ([21 60 2] [21 61 2] [34 9 2])
;; grid size 3 : max 29 at ([243 43 3])
;; grid size 4 : max 40 at ([236 162 4] [236 163 4] [236 164 4])
;; grid size 5 : max 49 at ([124 229 5] [235 150 5])
;; grid size 6 : max 65 at ([234 149 6])
;; grid size 7 : max 69 at ([233 161 7])
;; grid size 8 : max 79 at ([232 160 8])
;; grid size 9 : max 101 at ([237 155 9])
;; grid size 10 : max 109 at ([236 154 10])
;; grid size 11 : max 115 at ([236 155 11])
;; grid size 12 : max 114 at ([236 154 12])
;; grid size 13 : max 114 at ([236 151 13] [236 153 13])
;; grid size 14 : max 118 at ([236 151 14])
;; grid size 15 : max 127 at ([236 151 15])
;; grid size 16 : max 119 at ([236 150 16])
;; grid size 17 : max 114 at ([234 149 17])
;; grid size 18 : max 109 at ([236 148 18])
;; grid size 19 : max 106 at ([236 147 19])
;; grid size 20 : max 94 at ([232 146 20])
;; grid size 21 : max 83 at ([231 145 21])
;; grid size 22 : max 73 at ([224 146 22])
;; grid size 23 : max 62 at ([224 145 23])
;; grid size 24 : max 51 at ([224 144 24])
;; grid size 25 : max 25 at ([215 143 25])
;; grid size 26 : max 34 at ([218 137 26])
;; grid size 27 : max 27 at ([217 141 27])
;; grid size 28 : max 24 at ([218 137 28])
;; grid size 29 : max 8 at ([217 139 29])
;; grid size 30 : max -12 at ([216 138 30])


;; 201812
;; '.....' has to be translated into '.'
;; in my case the string stablizes just keeps shifting to right
;;      0     0    2416 "#.#####.#.#.####.####.#.#...#.......##..##.#.#.#.###..#.....#.####..#.#######.#....####.#....##....#"
;;      0     0    2416 "#.#####.#.#.####.####.#.#...#.......##..##.#.#.#.###..#.....#.####..#.#######.#....####.#....##....#"
;;  10000  9985  572454 "....###.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#....."
;;  20000 19985 1142454 "....###.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#....."
;;  30000 29985 1712454 "....###.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#....."
;;  40000 39985 2282454 "....###.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#....."
;;  50000 49985 2852454 "....###.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#....."
;;  60000 59985 3422454 "....###.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#....."
;;  70000 69985 3992454 "....###.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#....."
;;  80000 79985 4562454 "....###.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#....."
;;  90000 89985 5132454 "....###.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#....."
;; 100000 99985 5702454 "....###.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#....."
(let [[init & rules] (->> (slurp "src/y2018/input201812") (re-seq #"[#.]+"))
      rules (into {} (->> (partition 2 rules) (map vec)))
      g (fn [s p]
          (let [x (keep-indexed #(if (= %2 \#) %) s)]
            (+ (reduce + x) (* p (count x)))))
      f (fn [s p limit]
          (loop [s s, p p, n 0]
            (if (= (mod n 10000) 0) (prn n p (g s p) s))
            (if (< n limit)
              (let [x (->> (str "...." s "....") (partition 5 1) (map #(get rules (str/join %) ".")))
                    s (str/join x)
                    s1 (if (= (subs s 0 9) ".........") (subs s 5) s)
                    y (if (= (subs s 0 9) ".........") 3 -2)
                    s2 (if (= (subs s1 (- (count s1) 9)) ".........") (subs s1 0 (- (count s1) 5)) s1)]
                (recur s2
                       (+ p y)
                       (inc n)))
              (g s p))))]
  [(mapv #(f init 0 %) [20 100000])
   (->> (/ 50000000000 10000) (* 570000) (+ 2454))])
;; [[3494 5702454] 2850000002454]


;; 201813
(let [input (slurp "src/y2018/input201813")
      width (str/index-of input "\n")
      m (->> (str/split-lines input) (mapv vec))
      dir-map {\^      [-1  0], [\^ :left]     \<
               \v      [ 1  0], [\^ :right]    \>
               \<      [ 0 -1], [\^ :straight] \^
               \>      [ 0  1], [\v :left]     \>
               [\^ \\] \<     , [\v :right]    \<
               [\^ \/] \>     , [\v :straight] \v
               [\v \\] \>     , [\> :left]     \^
               [\v \/] \<     , [\> :right]    \v
               [\< \\] \^     , [\> :straight] \>
               [\< \/] \v     , [\< :left]     \v
               [\> \\] \v     , [\< :right]    \^
               [\> \/] \^     , [\< :straight] \<}
      turn-map {:left :straight, :straight :right, :right :left}
      carts (->> (str/replace input "\n" "")
                 (keep-indexed (fn [idx ch]
                                 (if (#{\v \^ \< \>} ch)
                                   (let [y (quot idx width)
                                         x (mod idx width)]
                                     [y x ch :left])))))
      move-cart (fn [[y x dir turn]]
                  (let [[new-y new-x] (map + [y x] (dir-map dir))
                        c (get-in m [new-y new-x])]
                    (cond (= \+ c)     [new-y new-x (dir-map [dir turn]) (turn-map turn)]
                          (#{\\ \/} c) [new-y new-x (dir-map [dir c]) turn]
                          :else        [new-y new-x dir turn])))
      xy-str (fn [[y x]] (str x "," y))
      g (fn [{:keys [carts r] :as state} cart]
          (if (carts cart)
            (let [updated-cart (move-cart cart)
                  updated-cart-pos (take 2 updated-cart)
                  other-carts (disj carts cart)
                  other-carts-pos (set (map #(take 2 %) other-carts))]
              (cond (other-carts-pos updated-cart-pos)
                    {:carts (set (remove #(= (take 2 %) updated-cart-pos) other-carts))
                     :r (conj r updated-cart-pos)}
                    :else
                    (assoc state :carts (conj other-carts updated-cart))))
            state))]
  (loop [{:keys [carts r]} {:carts carts, :r []}]
    (if (= 1 (count carts))
      (map xy-str [(first r) (take 2 (first carts))])
      (recur (reduce g {:carts (set carts) :r r} (sort carts))))))
;; ("58,93" "91,72")


;; 201814
(let [input-s (slurp "src/y2018/input201814")
      input-n (edn/read-string input-s)
      input-a (map edn/read-string (re-seq #"\d" input-s))
      r [3 7]
      f (fn [r a b]
          (let [s (+ (r a) (r b))
                s0 (quot s 10)
                s1 (mod s 10)
                new-r (into r (if (<= 10 s) [s0 s1] [s1]))
                new-a (mod (+ 1 a (r a)) (count new-r))
                new-b (mod (+ 1 b (r b)) (count new-r))]
            [new-r new-a new-b]))
      q1 (loop [r r
                [a b] [0 1]]
           (if (<= (+ 10 input-n) (count r))
             (str/join (subvec r input-n (+ input-n 10)))
             (let [[new-r new-a new-b] (f r a b)]
               (recur new-r [new-a new-b]))))
      q2 (loop [r r
                [a b] [0 1]]
           (let [c (count r)]
             (cond (and (<= 7 c) (= input-a (subvec r (- c (count input-a)) c)))
                   (- c (count input-a))
                   (and (<= 7 c) (= input-a (subvec r (- c (count input-a) 1) (- c 1))))
                   (- c (count input-a) 1)
                   :else (let [[new-r new-a new-b] (f r a b)]
                           (recur new-r [new-a new-b])))))]
  [q1 q2])
;; ["3138510102" 20179081]
