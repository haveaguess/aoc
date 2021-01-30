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
      q2 (let [l (count input-a)]
           (loop [r r
                  [a b] [0 1]]
             (let [c (count r)]
               (cond (and (<= (inc l) c) (= input-a (subvec r (- c l 1) (- c 1))))
                     (- c l 1)
                     (and (<= l c) (= input-a (subvec r (- c l) c)))
                     (- c l)
                     :else (let [[new-r new-a new-b] (f r a b)]
                             (recur new-r [new-a new-b]))))))]
  [q1 q2])
;; ["3138510102" 20179081]


;; 201815
(let [input (slurp "src/y2018/input201815")
      fixed-grid (->> (str/escape input {\E \., \G, \.}) str/split-lines (map vec) vec)
      width (count (first fixed-grid))
      target-map {\E \G, \G \E}
      egs (->> (str/replace input "\n" "")
               (keep-indexed
                (fn [idx ch]
                  (if (#{\E \G} ch)
                    [idx {:xy [(quot idx width) (mod idx width)] :ch ch :hp 200}])))
               (into {}))
      build-grid (fn [grid egs] (reduce-kv #(assoc-in % (:xy %3) (:ch %3)) grid egs))
      draw (fn [msg grid egs]
             (println msg)
             (mapv #(println (str/join %)) (build-grid grid egs))
             (mapv println (sort-by #(:xy (val %)) egs)))
      expand (fn [loc] (map #(let [new-xy (mapv + (:xy loc) %)]
                               {:xy new-xy :next (or (:next loc) new-xy)})
                            [[-1 0] [0 -1] [0 1] [1 0]]))
      find-neighbor
      (fn find-neighbor [xy ch egs]
        (let [grid (build-grid fixed-grid egs)]
          (loop [radius 1
                 seen #{xy}
                 starting [{:xy xy}]]
            (let [edge-xys (->> (mapcat expand starting)
                                (remove #(or (#{\# ch} (get-in grid (:xy %) \#))
                                             (seen (:xy %)))))
                  target-xys (filter #(= (target-map ch) (get-in grid (:xy %))) edge-xys)]
              (cond (seq target-xys)
                    (cond (= radius 1)
                          [radius xy (->> (filter #((set (map :xy target-xys)) (:xy (val %))) egs)
                                          (sort-by #(vector (:hp (val %)) (:xy (val %))))
                                          first
                                          key)]
                          (= radius 2)
                          (let [new-xy (->> (map #(vector (:xy %) (:next %)) target-xys) sort first second)]
                            [radius
                             new-xy
                             ((find-neighbor new-xy ch egs) 2)])
                          :else
                          [radius
                           (->> (map #(vector (:xy %) (:next %)) target-xys) sort first second)
                           :skip])
                    (seq edge-xys)
                    (let [consolidated-edge-xys
                          (->> (group-by :xy edge-xys)
                               (map (fn [x] {:xy (key x)
                                             :next (->> x val (map :next) sort first)})))]
                      (recur (inc radius)
                             (into seen (map :xy consolidated-edge-xys))
                             consolidated-edge-xys)))))))
      q1 (loop [rounds 0
                egs egs
                [eg-id & more-ids :as ids] (map key (sort-by #(:xy (val %)) egs))]
           (cond (= 1 (->> (vals egs) (map :ch) frequencies count))
                 (let [r (if (some egs ids) rounds (inc rounds))
                       hp (reduce + (map :hp (vals egs)))]
                   (* r hp))

                 eg-id
                 (if-let [{:keys [xy ch hp]} (egs eg-id)]
                   (if-let [[radius new-xy neighbor-id] (find-neighbor xy ch egs)]
                     (let [egs0 (if (<= radius 2)
                                  (if (< 3 (get-in egs [neighbor-id :hp]))
                                    (update-in egs [neighbor-id :hp] - 3)
                                    (dissoc egs neighbor-id))
                                  egs)
                           egs1 (assoc-in egs0 [eg-id :xy] new-xy)]
                       (recur rounds egs1 more-ids))
                     (recur rounds egs more-ids))
                   (recur rounds egs more-ids))

                 :else
                 (recur (inc rounds) egs (map key (sort-by #(:xy (val %)) egs)))))
      f (fn [elf-power]
          (let [power-map {\E elf-power, \G 3}]
            (loop [rounds 0
                   egs egs
                   [eg-id & more-ids :as ids] (map key (sort-by #(:xy (val %)) egs))]
              (cond (= 1 (->> (vals egs) (map :ch) frequencies count))
                    (let [r (if (some egs ids) rounds (inc rounds))
                          hp (reduce + (map :hp (vals egs)))]
                      (* r hp))

                    eg-id
                    (if-let [{:keys [xy ch hp]} (egs eg-id)]
                      (if-let [[radius new-xy neighbor-id] (find-neighbor xy ch egs)]
                        (let [egs0 (if (<= radius 2)
                                     (if (< (power-map ch) (get-in egs [neighbor-id :hp]))
                                       (update-in egs [neighbor-id :hp] - (power-map ch))
                                       (if (= ch \E)
                                         (dissoc egs neighbor-id)
                                         {:failed :true}))
                                     egs)
                              egs1 (assoc-in egs0 [eg-id :xy] new-xy)]
                          (if (:failed egs1)
                            nil
                            (recur rounds egs1 more-ids)))
                        (recur rounds egs more-ids))
                      (recur rounds egs more-ids))

                    :else
                    (recur (inc rounds) egs (map key (sort-by #(:xy (val %)) egs)))))))
      q2 (->> (map f (drop 4 (range))) (some identity))]
  [q1 q2])
;; [228240 52626]


;; 201816
(let [parse (fn [xs]
              (loop [[x & more :as xs] xs
                     sample []]
                (if (= x "B")
                  (recur (drop 12 more) (conj sample (->> (take 12 more) (map edn/read-string) (partition 4))))
                  [sample (->> (map edn/read-string xs) (partition 4))])))
      solve (fn [m]
              (loop [mapping {}
                     m m]
                (if-let [[name opcode-set] (some #(if (= 1 (count (val %))) %) m)]
                  (let [opcode (first opcode-set)
                        m0 (reduce #(update % %2 disj opcode) m (keys m))]
                    (recur (assoc mapping opcode name) m0))
                  mapping)))
      registers [0 0 0 0]
      fns {:addr (fn [r [_ a b c]] (assoc r c (+       (r a) (r b))))
           :addi (fn [r [_ a b c]] (assoc r c (+       (r a)    b)))
           :mulr (fn [r [_ a b c]] (assoc r c (*       (r a) (r b))))
           :muli (fn [r [_ a b c]] (assoc r c (*       (r a)    b)))
           :banr (fn [r [_ a b c]] (assoc r c (bit-and (r a) (r b))))
           :bani (fn [r [_ a b c]] (assoc r c (bit-and (r a)    b)))
           :borr (fn [r [_ a b c]] (assoc r c (bit-or  (r a) (r b))))
           :bori (fn [r [_ a b c]] (assoc r c (bit-or  (r a)    b)))
           :setr (fn [r [_ a b c]] (assoc r c (r a)))
           :seti (fn [r [_ a b c]] (assoc r c a))
           :gtir (fn [r [_ a b c]] (assoc r c (if (>    a  (r b)) 1 0)))
           :gtri (fn [r [_ a b c]] (assoc r c (if (> (r a)    b)  1 0)))
           :gtrr (fn [r [_ a b c]] (assoc r c (if (> (r a) (r b)) 1 0)))
           :eqir (fn [r [_ a b c]] (assoc r c (if (=    a  (r b)) 1 0)))
           :eqri (fn [r [_ a b c]] (assoc r c (if (= (r a)    b)  1 0)))
           :eqrr (fn [r [_ a b c]] (assoc r c (if (= (r a) (r b)) 1 0)))}
      [samples test] (->> (slurp "src/y2018/input201816") (re-seq #"B|\d+") parse)
      q1 (->> samples
              (filter (fn [[ir code or]] (<= 3 (count (filter #(= (% (vec ir) code) or) (vals fns))))))
              count)
      mapping (->> samples
                   (mapcat (fn [[ir code or]]
                             (keep (fn [[name f]] (if (= (f (vec ir) code) or) [name (first code)])) fns)))
                   (reduce (fn [r [name opcode]]
                             (if (r name) (update r name conj opcode) (assoc r name #{opcode})))
                           {})
                   solve)
      q2 ((reduce #((fns (mapping (first %2))) % %2) registers test) 0)]
  [q1 q2])
;; [636 674]


;; 201817
(let [parse (fn [[a b c d e f]]
              (let [[b d f] (map edn/read-string [b d f])
                    range1 (range b (inc b))
                    range2 (range d (inc f))
                    [x-range y-range] (if (= "x" a) [range1 range2] [range2 range1])]
                (for [x x-range, y y-range]
                  [y x])))
      data0 (->> (slurp "src/y2018/input201817")
                 (re-seq #"x|y|\.\.|\d+")
                 (partition 6)
                 (mapcat parse))
      first-line-clay (reduce min (map first data0))
      data  (cons [0 500] data0)
      ys    (map first  data)
      xs    (map second data)
      min-y (reduce min ys)
      max-y (reduce max ys)
      min-x (dec (reduce min xs))
      max-x (inc (reduce max xs))
      base-grid (vec (repeat (- max-y min-y -1) (vec (repeat (- max-x min-x -1) "."))))
      translate #(map - % [min-y min-x])
      grid (-> (reduce #(assoc-in % (translate %2) "#") base-grid data)
               (assoc-in (translate [0 500]) "+"))
      draw (fn [grid] (mapv #(println (str/join %)) grid))
      search-factory (fn [f]
                       (fn [grid y x]
                         (map #(loop [x0 (% x)] (if (f "#" (get-in grid [y x0] "#")) x0 (recur (% x0)))) [dec inc])))
      clay-lr (search-factory =)
      empty-lr (search-factory not=)
      going-down (fn [grid y x]
                   (loop [y (inc y), grid grid]
                     (if (<= y (- max-y min-y))
                       (case (get-in grid [y x])
                         "|" [:done grid]
                         "." (recur (inc y) (assoc-in grid [y x] "|"))
                         "~" [:up grid (dec y)]
                         "#" (let [[clay-left clay-right] (clay-lr grid (dec y) x)
                                   [empty-left empty-right] (empty-lr grid y x)
                                   left-spill (< clay-left empty-left)
                                   right-spill (< empty-right clay-right)]
                               (if (or left-spill right-spill)
                                 [:spring
                                  (reduce #(assoc-in % [(dec y) %2] "|")
                                          grid
                                          (concat
                                           (if left-spill (range empty-left (inc x)))
                                           (if right-spill (range x (inc empty-right)))))
                                  (concat (if left-spill [[y empty-left]])
                                          (if right-spill [[y empty-right]]))]
                                 [:up grid (dec y)])))
                       [:done grid])))
      going-up (fn [grid y x]
                 (loop [y y
                        [prev-left prev-right] (clay-lr grid y x)
                        grid0 (reduce #(assoc-in %1 [y %2] "~") grid (range (inc prev-left) prev-right))]
                   (let [[left right] (clay-lr grid0 (dec y) x)
                         fill (if (or (< left prev-left) (< prev-right right)) "|" "~")
                         real-left (if (< left prev-left)
                                     (loop [a (dec prev-left)]
                                       (if (= "#" (get-in grid0 [y a])) (recur (dec a)) a))
                                     left)
                         real-right (if (< prev-right right)
                                      (loop [a (inc prev-right)]
                                        (if (= "#" (get-in grid0 [y a])) (recur (inc a)) a))
                                      right)
                         grid1 (->> (range (if (< left prev-left) real-left (inc left))
                                           (if (< prev-right right) (inc real-right) right))
                                    (reduce #(assoc-in % [(dec y) %2] fill) grid0))]
                     (if (or (< left prev-left) (< prev-right right))
                       [grid1 (concat (if (< left prev-left)   [[(dec y) real-left]])
                                      (if (< prev-right right) [[(dec y) real-right]]))]
                       (recur (dec y) [left right] grid1)))))
      f (fn [[y x] grid]
          (let [[status grid0 y-or-spring] (going-down grid y x)]
            (cond (= :done status)
                  [grid0 :done]
                  (= :spring status)
                  [grid0 y-or-spring]
                  (= :up status)
                  (going-up grid0 y-or-spring x))))]
  #_(draw grid)
  (loop [[spring & more-springs] [(translate [0 500])]
         grid grid]
    (if spring
      (let [[grid0 new-spring-or-status] (f spring grid)]
        (if (= :done new-spring-or-status)
          (recur more-springs grid0)
          (recur (concat new-spring-or-status more-springs) grid0)))
      (let [x (-> (flatten (subvec grid first-line-clay)) frequencies (select-keys ["|" "~"]))]
        #_(draw grid)
        [(reduce + (vals x)) (x "~")]))))
;; [30737 24699]


;; 201818
(let [grid (->> (str/split-lines (slurp "src/y2018/input201818")) (map vec) vec)
      y (count grid)
      x (count (first grid))
      adj (fn [grid y0 x0]
            (-> (for [y [-1 0 1], x [-1 0 1]
                      :when (not= 0 y x)
                      :let [v (get-in grid [(+ y0 y) (+ x0 x)])]]
                  v)
                frequencies))
      f (fn [grid]
          (reduce (fn [r [y x v]] (assoc-in r [y x] v))
                  grid
                  (for [y0 (range y), x0 (range x)
                        :let [vmap (adj grid y0 x0)
                              v (case (get-in grid [y0 x0])
                                  \. (if (<= 3 (vmap \| 0)) \| \.)
                                  \| (if (<= 3 (vmap \# 0)) \# \|)
                                  \# (if (and (<= 1 (vmap \| 0)) (<= 1 (vmap \# 0))) \# \.))]]
                    [y0 x0 v])))
      g (fn [grid] (reduce * (-> grid flatten frequencies (select-keys [\| \#]) vals)))
      draw (fn [grid] (mapv #(println (str/join %)) grid))]
  [(g (-> (iterate f grid) (nth 10)))
   (loop [seen-set #{grid}
          seen-vec [grid]
          n 1]
     (let [grid0 (f (peek seen-vec))]
       (if (seen-set grid0)
         (let [idx (->> seen-vec (keep-indexed #(if (= grid0 %2) %1)) first)
               v (subvec seen-vec idx)]
           (g (v (mod (- 1000000000 n) (count v)))))
         (recur (conj seen-set grid0) (conj seen-vec grid0) (inc n)))))])
;; [486878 190836]


;; 201819
(let [parse (fn [s] (let [[op op1 op2 op3] (re-seq #"\w+" s)]
                      (cons (keyword op) (map edn/read-string [op1 op2 op3]))))
      [ip-binding & instructions] (str/split-lines (slurp "src/y2018/input201819"))
      ip-reg (-> (re-seq #"\d" ip-binding) first edn/read-string)
      instructions (mapv parse instructions)
      c-instructions (count instructions)
      fns {:addr (fn [r [_ a b c]] (assoc r c (+       (r a) (r b))))
           :addi (fn [r [_ a b c]] (assoc r c (+       (r a)    b)))
           :mulr (fn [r [_ a b c]] (assoc r c (*       (r a) (r b))))
           :muli (fn [r [_ a b c]] (assoc r c (*       (r a)    b)))
           :banr (fn [r [_ a b c]] (assoc r c (bit-and (r a) (r b))))
           :bani (fn [r [_ a b c]] (assoc r c (bit-and (r a)    b)))
           :borr (fn [r [_ a b c]] (assoc r c (bit-or  (r a) (r b))))
           :bori (fn [r [_ a b c]] (assoc r c (bit-or  (r a)    b)))
           :setr (fn [r [_ a b c]] (assoc r c (r a)))
           :seti (fn [r [_ a b c]] (assoc r c a))
           :gtir (fn [r [_ a b c]] (assoc r c (if (>    a  (r b)) 1 0)))
           :gtri (fn [r [_ a b c]] (assoc r c (if (> (r a)    b)  1 0)))
           :gtrr (fn [r [_ a b c]] (assoc r c (if (> (r a) (r b)) 1 0)))
           :eqir (fn [r [_ a b c]] (assoc r c (if (=    a  (r b)) 1 0)))
           :eqri (fn [r [_ a b c]] (assoc r c (if (= (r a)    b)  1 0)))
           :eqrr (fn [r [_ a b c]] (assoc r c (if (= (r a) (r b)) 1 0)))}]
  (loop [registers [0 0 0 0 0 0]
         ip 0]
    (if (<= c-instructions ip)
      (registers 0)
      (let [instruction (instructions ip)
            new-registers ((fns (first instruction)) (assoc registers ip-reg ip) instruction)
            new-ip (inc (new-registers ip-reg))]
        (recur new-registers new-ip)))))
;; 912


;; 201820
(let [dir-map {\N [-1 0], \S [1 0], \W [0 -1], \E [0 1]}
      input (-> (slurp "src/y2018/input201820") str/trim (str/escape {\^ \(, \$ \)}))
      d (loop [[c & more] input
               distance 0
               locs nil
               current-locs [0 0]
               distance-map {[0 0] 0}]
          (case c
            nil distance-map
            \( (recur more distance (cons [distance current-locs] locs) current-locs distance-map)
            \) (recur more distance (rest locs) current-locs distance-map)
            \| (recur more (ffirst locs) locs ((first locs) 1) distance-map)
            (let [new-distance (inc distance)
                  new-current (mapv + current-locs (dir-map c))
                  new-distance-map (update distance-map new-current #(if (nil? %) new-distance (min % new-distance)))]
              (recur more new-distance locs new-current new-distance-map))))]
  [(reduce max (vals d))
   (count (filter #(<= 1000 %) (vals d)))])
;; [3465 7956]


;; 201822
(let [[depth target-x target-y] (->> (slurp "src/y2018/input201822") (re-seq #"\d+") (map edn/read-string))
      ;;[depth x y] [510 10 10]
      height (inc target-y)
      width (inc target-x)
      f (fn [m n]
          (let [y (quot n width)
                x (mod n width)
                v (-> (cond (and (= x 0) (= y 0)) 0
                            (and (= x target-x) (= y target-y)) 0
                            (= x 0) (* 48271 y)
                            (= y 0) (* 16807 x)
                            :else (* (m [y (dec x)]) (m [(dec y) x])))
                      (+ depth)
                      (mod 20183))]
            (assoc m [y x] v)))
      geo-map (reduce f {} (range (* height width)))]
  (->> (vals geo-map) (map #(mod % 3)) frequencies sort (map val) (map * [0 1 2]) (reduce +)))
