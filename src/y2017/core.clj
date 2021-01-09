(ns y2017.core
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

;; 201701
(let [s (slurp "src/y2017/input201701")
      c (/ (count s) 2)
      f (fn f
          ([x y] (if (= x y) (- (int x) 48) 0))
          ([[x y]] (f x y)))]
  [(->> (partition 2 1 s s) (map f) (reduce +))
   (->> (map f s (drop c s)) (reduce +) (* 2))])
;; [1341 1348]


;; 201702
(let [f (fn [xs] (- (reduce max xs) (reduce min xs)))
      g (fn [xs]
          (->> (for [a xs, b xs :when (and (not= a b) (= 0 (mod a b)))]
                 (/ a b))
               first))
      xs (->> (slurp "src/y2017/input201702")
              (re-seq #".+")
              (map #(map edn/read-string (re-seq #"\d+" %))))]
  (map #(reduce + (map % xs)) [f g]))
;; (47623 312)


;; 201703
(let [n (->> "src/y2017/input201703" slurp edn/read-string)
      a (int (Math/sqrt n))
      b (- n (* a a))
      a-square-location (map + (map #(quot a %) [2 -2]) (if (odd? a) [0 0] [1 0]))
      directions (cycle [[1 0] [0 1] [-1 0] [0 -1]])
      edges (interleave (rest (range)) (rest (range)))
      spiral-directions (mapcat #(repeat % %2) edges directions)
      neighbor-offset (for [x [-1 0 1], y [-1 0 1] :when (not= [0 0] [x y])] [x y])
      neighbor-sum (fn [m xy] (->> neighbor-offset (map #(get m (map + xy %) 0)) (reduce +)))]
  [(->> (map #(map * % %2)
             (map #(repeat 2 %) [(min 1 b) (min a (max (- b 1) 0)) (min a (max (- b a 1) 0))])
             (drop (if (even? a) 2 0) directions))
        (reduce #(map + % %2) a-square-location)
        (reduce #(+ (Math/abs %) (Math/abs %2))))
   (loop [current-loc [0 0]
          m {[0 0] 1}
          [dir & more-dirs] spiral-directions]
     (let [next-loc (map + current-loc dir)
           next-val (neighbor-sum m next-loc)]
       (if (> next-val n)
         next-val
         (recur next-loc (assoc m next-loc next-val) more-dirs))))])
;; [419 295229]


;; 201704
(let [f (fn [xs] (if (= (count xs) (count (set xs))) 1 0))
      g (fn [xs] (f (map sort xs)))
      s (->> (slurp "src/y2017/input201704") (re-seq #".+") (map #(re-seq #"\w+" %)))]
  (map #(reduce + (map % s)) [f g]))
;; (325 119)


;; 201705
(let [xs (->> (slurp "src/y2017/input201705") (re-seq #"-?\d+") (mapv edn/read-string))
      c (count xs)]
  (map #(loop [xs xs, ip 0, steps 0]
          (if (>= ip c)
            steps
            (recur (update xs ip %) (+ ip (get xs ip)) (inc steps))))
       [inc #((if (>= % 3) dec inc) %)]))
;; (359348 27688760)


;; 201706
(let [xs (->> (slurp "src/y2017/input201706") (re-seq #"\d+") (mapv clojure.edn/read-string))
      g (fn [xs]
          (let [m (reduce max xs)]
            (loop [c 0] (if (= (xs c) m) [c m] (recur (inc c))))))
      f (fn [xs]
          (let [c (count xs)
                [idx v] (g xs)]
            (->> (concat (repeat (inc idx) 0) (repeat v 1))
                 (partition c c (repeat 0))
                 (cons (assoc xs idx 0))
                 (apply mapv +))))]
  (loop [xs xs, states #{}, states2 {}, steps 0]
    (if (states xs)
      [steps (- steps (states2 xs))]
      (recur (f xs) (conj states xs) (conj states2 [xs steps]) (inc steps)))))
;; [4074 2793]


;; 201707
(let [input (slurp "src/y2017/input201707")
      input1 (->> input str/split-lines (map #(re-seq #"\w+" %)))
      base-map (reduce #(assoc %1 (first %2) (edn/read-string (second %2))) {} input1)
      n (filter #(> (count %) 2) input1)
      subtowers (set (map first n))]
  [(->> (re-seq #"[a-z]+" input) frequencies (some #(if (= 1 (val %)) (key %))))
   (loop [[[base _ & subtowerids :as work] & more] n
          pending-towers subtowers
          m base-map]
     (if (some pending-towers subtowerids)
       (recur (concat more [work]) pending-towers m)
       (let [subtowervals-seq (map m subtowerids)
             subtowervals-set (set subtowervals-seq)]
         (if (> (count subtowervals-set) 1)
           (let [single-value (->> (frequencies subtowervals-seq) (filter #(= 1 (val %))) ffirst)
                 single-id (some #(if (= single-value (m %)) %) subtowerids)
                 common-value (first (disj subtowervals-set single-value))]
             (if (subtowers single-id)
               (+ (base-map single-id) (- common-value single-value))
               single-value))
           (recur more (disj pending-towers base) (update m base #(apply + % subtowervals-seq)))))))])
(let [input (slurp "src/y2017/input201707")
      parse (fn [r [base v & sub]]
              (assoc r base {:v (edn/read-string v) :sub sub}))
      m (->> input str/split-lines (map #(re-seq #"\w+" %)) (reduce parse {}))]
  [(->> (re-seq #"[a-z]+" input) frequencies (some #(if (= 1 (val %)) (key %))))
   (remove (set (mapcat :sub (vals m))) (keys m))])
;; ["aapssr" 1458]


;; 201708
(let [input (->> (slurp "src/y2017/input201708") (re-seq #"\S+") (partition 7))
      op {">" >, "<" <, ">=" >=, "<=" <=, "!=" not=, "==" =, "inc" +, "dec" -}
      f (fn [{:keys [registers highest] :as state} [op1 op0 op2 _ op4 op3 op5 :as t]]
          (if ((op op3) (get registers op4 0) (edn/read-string op5))
            (let [temp ((op op0) (get registers op1 0) (edn/read-string op2))
                  highest (max temp highest)]
              {:registers (assoc registers op1 temp) :highest highest})
            state))]
  (let [r (reduce f {:registers {} :highest 0} input)]
    [(->> (:registers r) vals (reduce max)) (:highest r)]))
;; [6343 7184]


;; 201709
(let [input (-> "src/y2017/input201709" slurp (str/replace #"!." ""))
      f (fn [{:keys [level total] :as state} ch]
          (cond
            (= ch \{) (update state :level inc)
            (= ch \}) {:level (dec level) :total (+ total level)}
            :else state))]
  [(:total (reduce f {:level 0 :total 0} (str/replace input #"<.*?>" "")))
   (let [garbages (map count (re-seq #"<.*?>" input))]
     (- (reduce + garbages) (* 2 (count garbages))))])
;; [12897 7031]


;; 201710
(let [s (slurp "src/y2017/input201710")
      addon [17 31 73 47 23]
      input1 (->> (re-seq #"\d+" s) (map edn/read-string))
      input2 (concat (map int s) addon)
      len 256
      f (fn [input pass]
          (loop [o (range len), [x & xs] input, head 0, skip 0, pass pass]
            (if x
              (let [offset (mod (+ x skip) len)
                    head' (+ head offset)
                    o' (concat (reverse (take x o)) (drop x o))
                    o'' (concat (drop offset o') (take offset o'))]
                (recur o'' xs head' (inc skip) pass))
              (if (= 1 pass)
                (let [head (- len (mod head len))]
                  (concat (drop head o) (take head o)))
                (recur o input head skip (dec pass))))))]
  [(->> (f input1 1) (take 2) (reduce *))
   (->> (f input2 64) (partition 16) (map #(format "%02x" (reduce bit-xor %))) str/join)])
;; [48705 "1c46642b6f2bc21db2a2149d0aeeae5d"]


;; 201711
(let [input (->> (slurp "src/y2017/input201711") (re-seq #"\w+"))
      m (frequencies input)
      f (fn [m]
          (let [[se sw nw ne w e] (map #(m % 0) ["se" "sw" "nw" "ne" "w" "e"])
                we (- w e)]
            (+ (Math/abs (- se nw we)) (Math/abs (- ne sw we)))))]
  [(f m)
   (-> (reduce #(let [m (update % %2 inc), x (f m)] (update m :furthest max x))
               {"sw" 0, "se" 0, "s" 0, "n" 0, "nw" 0, "ne" 0, :furthest 0}
               input)
       :furthest)])
;; [812 1603]


;; 201712
(let [s (->> (slurp "src/y2017/input201712") (re-seq #".+") (map #(re-seq #"\d+" %)))
      f (fn [m [k & vs]] (assoc m k (set vs)))
      m (reduce f {} s)]
  [(loop [[x & xs] (m "0"), seen #{"0"}]
     (if x
       (if (seen x)
         (recur xs seen)
         (recur (into xs (m x)) (conj seen x)))
       (count seen)))
   (loop [[node & more] (keys m)
          seen #{}
          group-count 0]
     (if node
       (let [neighbors (m node)
             seen' (into (conj seen node) neighbors)]
         (recur (concat (remove seen neighbors) more) seen' (if (seen node) group-count (inc group-count))))
       group-count))])
;; [134 193]


;; 201713
(let [input (->> (slurp "src/y2017/input201713")
                 (re-seq #"\d+")
                 (map edn/read-string)
                 (partition 2))
      f (fn [offset]
          (fn [[layer depth]]
            (let [round-trip (* 2 (dec depth))]
              (= 0 (mod (+ layer offset) round-trip)))))
      g (fn [r [layer depth]] (+ r (* layer depth)))]
  [(reduce g 0 (filter (f 0) input))
   (some #(if (every? false? (filter (f %) input)) %) (range))])
;; [1612 3907994]


;; 201714
(let [addon [17 31 73 47 23]
      two-p [128 64 32 16 8 4 2 1]
      input (slurp "src/y2017/input201714")
      len 256
      f (fn [input pass]
          (loop [o (range len), [x & xs] input, head 0, skip 0, pass pass]
            (if x
              (let [offset (mod (+ x skip) len)
                    head' (+ head offset)
                    o' (concat (reverse (take x o)) (drop x o))
                    o'' (concat (drop offset o') (take offset o'))]
                (recur o'' xs head' (inc skip) pass))
              (if (= 1 pass)
                (let [head (- len (mod head len))]
                  (concat (drop head o) (take head o)))
                (recur o input head skip (dec pass))))))
      g (fn [x] (map #(if (< 0 (bit-and % (reduce bit-xor x))) 1 0) two-p))
      bit-array (->> (for [i (range 128)]
                       (->> (f (concat (map int (str input "-" i)) addon) 64)
                            (partition 16)
                            (map g)))
                     flatten
                     vec)]
  [(reduce + bit-array)
   (loop [n 0
          xs ()
          [loc & _ :as remaining] (set (keep-indexed #(if (= 1 %2) %1) bit-array))]
     (cond
       (seq xs) (let [neighbors (->> (for [x xs]
                                       (list (if (not= 0   (mod x 128)) (- x 1))
                                             (if (not= 127 (mod x 128)) (+ x 1))
                                             (if (> x 127) (- x 128))
                                             (if (< x 16256) (+ x 128))))
                                     flatten
                                     distinct
                                     (filter remaining))]
                  (recur n neighbors (apply disj remaining xs)))
       loc (recur (inc n) (list loc) (disj remaining loc))
       :else n))])
;; [8214 1093]


;; 201715
(let [[a b] (->> (slurp "src/y2017/input201715") (re-seq #"\d+") (map edn/read-string))
      [factor-a factor-b] [16807 48271]
      f (fn [factor] #(mod (* factor %) 2147483647))
      g (fn [x] (bit-and 65535 x))]
  [(->> (map #(if (= % %2) 1 0)
             (->> (iterate (f factor-a) a) rest (take 40000000) (map g))
             (->> (iterate (f factor-b) b) rest (take 40000000) (map g)))
        (reduce +))
   (->> (map #(if (= % %2) 1 0)
             (->> (iterate (f factor-a) a) rest (filter #(= 0 (mod % 4))) (take 5000000) (map g))
             (->> (iterate (f factor-b) b) rest (filter #(= 0 (mod % 8))) (take 5000000) (map g)))
        (reduce +))])
;; [650 336]


;; 201716
(time(let [input (->> (slurp "src/y2017/input201716") (re-seq #"[^,]+"))
           s "abcdefghijklmnop"
           f (fn [s] (fn [c] (str/index-of (apply str s) c)))
           g (fn [s]
               (->> (reduce #(let [[a b] (re-seq #"\w+" (subs %2 1))]
                               (condp = (subs %2 0 1)
                                 "s" (let [a (- 16 (edn/read-string a))]
                                       (concat (drop a %) (take a %)))
                                 "x" (let [[a b] (map edn/read-string [a b])
                                           % (vec %)]
                                       (-> % (assoc a (get % b)) (assoc b (get % a))))
                                 "p" (let [[a b] (map (f %) [a b])
                                           % (vec %)]
                                       (-> % (assoc a (get % b)) (assoc b (get % a))))))
                            s
                            input)
                    str/join))]
       [(g s)
        (loop [[s1 & more] (iterate g s)
               cache []
               seen #{}]
          (if (seen s1)
            (let [cache' (drop-while #(not= s1 %) cache)]
              (nth cache' (mod (- 1000000000 (count cache)) (count cache'))))
            (recur more (conj cache s1) (conj seen s1))))]))
;; ["kbednhopmfcjilag" "fbmcgdnjakpioelh"]


;; 201717
(let [input (edn/read-string (slurp "src/y2017/input201717"))]
  [(->> (loop [s [0 1]
               p 1
               n 2]
          (if (= 2018 n)
            s
            (let [p (inc (mod (+ p input) n))]
              (recur (vec (concat (take p s) [n] (drop p s))) p (inc n)))))
        (drop-while #(not= 2017 %))
        second)
   (->> (loop [v 1
               p 1
               n 2]
          (if (= 50000000 n)
            v
            (let [p (inc (mod (+ p input) n))]
              (recur (if (= p 1) n v) p (inc n))))))])
;; [1282 27650600]


;; 201718
(let [input (->> (slurp "src/y2017/input201718") (re-seq #".+") (map #(re-seq #"\S+" %)) vec)
      f (fn [r op] (r op (edn/read-string op)))
      vm0 (loop [r {:freq nil}, ip 0]
            (if-let [[op0 op1 op2] (get input ip nil)]
              (if (and (= op0 "rcv") (not= 0 (f r op1)))
                (:freq r)
                (let [r (condp = op0
                          "set" (assoc r op1 (f r op2))
                          "add" (update r op1 (fnil #(+   % (f r op2)) 0))
                          "mul" (update r op1 (fnil #(*   % (f r op2)) 0))
                          "mod" (update r op1 (fnil #(mod % (f r op2)) 0))
                          "jgz" r
                          "snd" (assoc r :freq (f r op1))
                          "rcv" r)
                      ip (+ ip (if (and (= op0 "jgz") (> (f r op1) 0)) (f r op2) 1))]
                  (recur r ip)))))
      p0-sndq (atom ())
      p1-sndq (atom ())
      vm1 (fn [{:keys [r ip snd sndq rcvq status] :as m}]
            (if-let [[op0 op1 op2] (get input ip nil)]
              (if (and (= op0 "rcv") (nil? (first @rcvq)))
                (assoc m :status (if (= :rcv status) :rcv2 :rcv))
                (let [rcv (first @rcvq)
                      m (condp = op0
                          "set" (assoc-in m [:r op1] (f r op2))
                          "add" (update-in m [:r op1] (fnil #(+   % (f r op2)) 0))
                          "mul" (update-in m [:r op1] (fnil #(*   % (f r op2)) 0))
                          "mod" (update-in m [:r op1] (fnil #(mod % (f r op2)) 0))
                          "jgz" (update m :ip #(+ % -1 (if (> (f r op1) 0) (f r op2) 1)))
                          "snd" (do (swap! sndq concat [(f r op1)]) (update m :snd inc))
                          "rcv" (do (swap! rcvq rest) (assoc-in m [:r op1] rcv)))]
                  (recur (assoc (update m :ip inc) :status :running))))))]
  [vm0
   (loop [p0 (vm1 {:r {"p" 0} :ip 0 :snd 0 :status :running :sndq p0-sndq :rcvq p1-sndq})
          p1 (vm1 {:r {"p" 1} :ip 0 :snd 0 :status :running :sndq p1-sndq :rcvq p0-sndq})]
     (if (and (= (:status p0) (:status p1) :rcv2))
       (:snd p1)
       (recur (vm1 p0) (vm1 p1))))])
;; [9423 7620]


;; 201719
;; raw input has an empty border which makes program much simpler
(let [input (slurp "src/y2017/input201719")
      m (->> input (re-seq #".") (map-indexed #(vector %1 %2)) (into {}))
      start-pos (str/index-of input "|")
      width (str/index-of input "\n")
      offset-map {1 [width, (- width)]
                  -1 [width, (- width)]
                  width [1, -1]
                  (- width) [1, -1]}
      f (fn f [pos offset]
          (loop [pos pos, offset offset, r []]
            (condp = (m pos)
              "+" (->> (offset-map offset)
                       (some #(let [x (f (+ pos %) %)] (if (seq x) x)))
                       (into (conj r "+")))
              " " r
              nil r
              (recur (+ pos offset) offset (conj r (m pos))))))]
  (let [r (f start-pos width)]
    [(str/join (remove #{"|" "-" "+"} r))
     (count r)]))
;; ["ITSZCJNMUO" 17420]


;; 201720
;; prudent check is to iterate till all positions and accelerators have the same sign
;; and at that point iterate till sort by distance from [0,0,0] and sort by accelerators have the same result
(let [input (->> (slurp "src/y2017/input201720") (re-seq #"-?\d+") (map edn/read-string) (partition 9))
      f (fn [idx [_ _ _ _ _ _ p6 p7 p8]]
          [(+ (Math/abs p6) (Math/abs p7) (Math/abs p8)) idx])
      g (fn [idx p] [idx p])
      next-state (fn [[idx [p0 p1 p2 p3 p4 p5 p6 p7 p8]]]
                   (let [[p3 p4 p5 :as v] (map + [p3 p4 p5] [p6 p7 p8])
                         [p0 p1 p2 :as p] (map + [p3 p4 p5] [p0 p1 p2])]
                     [idx (concat p v [p6 p7 p8])]))
      h (fn [data]
          (let [new-data (map next-state data)
                all-pos (->> new-data
                             (map #(take 3 (second %)))
                             frequencies
                             (keep #(if (< 1 (val %)) (key %)))
                             set)]
            (remove #(all-pos (take 3 (second %))) new-data)))
      sorted-by-acceleration (->> (map-indexed f input) sort)
      lowest-acceleration (ffirst sorted-by-acceleration)]
  [(keep #(if (= (first %) lowest-acceleration) (second %)) sorted-by-acceleration)
   (->> (iterate h (map-indexed g input))
        (take 100)
        (map #(count %)))])
;; [(300) (1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 982 977 936 936 890 884 859 824 779 770 770 751 737 722 687 673 673 664 640 632 632 593 585 574 556 539 539 536 528 502 502 502 502 502 502 502 502 502 502 502 502 502 502 502 502 502 502 502 502 502 502 502 502 502 502 502 502 502 502 502 502 502 502 502 502 502 502 502 502 502 502 502 502 502 502 502 502 502 502 502 502 502 502 502 502 502 502 502 502 502)]


;; 201721
(let [input (->> (slurp "src/y2017/input201721") (re-seq #"[/#.]+") (partition 2))
      transpose (partial apply map #(vec %&))
      f (fn [[s d]]
          (let [[s d] (map #(->> (str/split % #"/") (map seq)) [s d])
                rotate-flipped-s (reduce #(let [f (if (even? %2) reverse transpose)]
                                            (cons (f (first %)) %))
                                         [s]
                                         (range 7))]
            (map #(vector % d) rotate-flipped-s)))
      rules (into {} (mapcat f input))
      start [\. \# \.
             \. \. \#
             \# \# \#]
      g (fn [{:keys [pattern size]}]
          (let [expand (if (even? size) 2 3)]
            {:pattern (->> (partition size pattern)
                           (partition expand)
                           (map (fn [[& xs]]
                                  (loop [xs xs, r []]
                                    (let [ys (map #(take expand %) xs)]
                                      (if (seq (first ys))
                                        (recur (map #(drop expand %) xs) (conj r (rules ys)))
                                        (apply map vector r))))))
                           flatten)
             :size (* (/ size expand) (inc expand))}))]
  (mapv #(-> (iterate g {:pattern start :size 3})
             (nth %)
             :pattern
             frequencies
             (get \#)) [5 18]))
;; [160 2271537]


;; 201722
;; dirs: [N E S W] = [0 1 2 3]
;; flags: [Clean, Weakened, Infected, Flagged] = [0 1 2 3]
(let [dir-map {0 [0 -1], 1 [1 0], 2 [0 1], 3 [-1 0]}
      s (slurp "src/y2017/input201722")
      input (->> s (re-seq #".+"))
      s (-> s (.replace "\n" "") (.replace "." "0") (.replace "#" "2"))
      [w h] [(count (first input)) (count input)]
      m (into {} (for [a (range h), b (range w) :let [o (+ (* a w) b)]]
                   [[b a] (edn/read-string (subs s o (inc o)))]))]
  [(loop [n 10000, m m, pos [(quot w 2) (quot h 2)], dirs 0, infected 0]
     (if (= n 0)
       infected
       (let [m' (update m pos #(if (nil? %) 2 (- 2 %)))
             dirs' ((if (> (m pos 0) 0) inc dec) dirs)
             pos' (map + pos (dir-map (mod dirs' 4)))
             infected' (if (= (m pos 0) 0) (inc infected) infected)]
         (recur (dec n) m' pos' dirs' infected'))))
   (loop [n 10000000, m m, pos [(quot w 2) (quot h 2)], dirs 0, infected 0]
     (if (= n 0)
       infected
       (let [m' (update m pos #(mod (if (nil? %) 1 (inc %)) 4))
             dirs' (condp = (mod (m pos 0) 4)
                     0 (dec dirs)
                     1 dirs
                     2 (inc dirs)
                     3 (+ 2 dirs))
             pos' (map + pos (dir-map (mod dirs' 4)))
             infected' (if (= (m pos 0) 1) (inc infected) infected)]
         (recur (dec n) m' pos' dirs' infected'))))])
;; [5352 2511475]


;; 201723
;; the program calc the number of composite between [b,c) or [109900,126900) with 17 stepping(line 31)
;; set b 99          line  1 : b = 99
;; set c b           line  2 : c = b
;; jnz a 2           line  3 : goto 5
;; jnz 1 5           line  4 : goto 9
;; mul b 100         line  5 : b = b * 100
;; sub b -100000     line  6 : b = b + 100000           . b = 109900
;; set c b           line  7 : c = b                    .
;; sub c -17000      line  8 : c = c + 17000            . c = 126900
;; set f 1           line  9 : f = 1
;; set d 2           line 10 : d = 2
;; set e 2           line 11 : e = 2
;; set g d           line 12 : g = d
;; mul g e           line 13 : g = d * e
;; sub g b           line 14 : g = g - b
;; jnz g 2           line 15 : if d*e != b goto 17
;; set f 0           line 16 : f = 0
;; sub e -1          line 17 : e = e + 1
;; set g e           line 18 : g = e
;; sub g b           line 19 : g = g - b
;; jnz g -8          line 20 : if   e != b goto 12
;; sub d -1          line 21 : d = d + 1
;; set g d           line 22 : g = d
;; sub g b           line 23 : g = g - b
;; jnz g -13         line 24 : if   d != b goto 11
;; jnz f 2           line 25 : if   f != 0 goto 27
;; sub h -1          line 26 : h = h + 1
;; set g b           line 27 : g = b
;; sub g c           line 28 : g = g - c
;; jnz g 2           line 29 : if   c != b goto 31
;; jnz 1 3           line 30 :             goto 33
;; sub b -17         line 31 : b = b + 17               . step = 17
;; jnz 1 -23         line 32 :             goto 9
(let [input (->> (slurp "src/y2017/input201723") (re-seq #".+") (map #(re-seq #"\S+" %)) vec)
      f (fn [n]
          (loop [[s & more] (range 2 (inc (Math/sqrt n)))]
            (if s
              (if (= 0 (mod n s))
                1
                (recur more))
              0)))]
  [(loop [r {"a" 0, "b" 0, "c" 0, "d" 0, "e" 0, "f" 0, "g" 0, "h" 0}
          mul 0
          ip 0]
     (if-let [[op0 op1 op2] (get input ip nil)]
       (let [r (condp = op0
                 "set" (assoc r op1 (r op2 (edn/read-string op2)))
                 "sub" (update r op1 #(- % (r op2 (edn/read-string op2))))
                 "mul" (update r op1 #(* % (r op2 (edn/read-string op2))))
                 "jnz" r)
             mul (if (= op0 "mul") (inc mul) mul)
             ip (if (and (= op0 "jnz") (not= (r op1 (edn/read-string op1)) 0))
                  (+ ip (edn/read-string op2))
                  (inc ip))]
         (recur r mul ip))
       mul))
   (reduce #(+ % (f %2)) 0 (range 109900 (+ 126900 17) 17))])
;; [9409 913]


;; 201724
(let [input (->> (slurp "src/y2017/input201724") (re-seq #"\d+") (map edn/read-string) (partition 2))
      f (fn [m [a b]]
          (-> (update m a #(if (nil? %) #{b} (conj % b)))
              (update b #(if (nil? %) #{a} (conj % a)))))
      g (fn [node]
          (fn [s]
            (disj s node)))
      m (reduce f {} input)
      h (fn h [m node cost]
          (loop [m m
                 node node
                 cost cost]
            (let [neighbors (m node)]
              (cond (empty? neighbors) (- cost node)
                    (neighbors node) (recur (update m node disj node) node (+ cost node node))
                    :else
                    (reduce max (map #(h (-> m
                                             (update % disj node)
                                             (update node disj %))
                                         %
                                         (+ cost % %))
                                     neighbors))))))
      i (fn i [m node cost depth]
          (loop [m m
                 node node
                 cost cost
                 depth depth]
            (let [neighbors (m node)]
              (cond (empty? neighbors) [depth (- cost node)]
                    (neighbors node) (recur (update m node disj node) node (+ cost node node) (inc depth))
                    :else
                    (last (sort (map #(i (-> m
                                             (update % (g node))
                                             (update node (g %)))
                                         %
                                         (+ cost % %)
                                         (inc depth))
                                     neighbors)))))))]
  [(h m 0 0)
   (second (i m 0 0 0))])
;; [1940 1928]


;; 201725
(let [f (fn [{:keys [ptr state tape]} _]
          (condp = [state (tape ptr 0)]
            [:a 0] {:tape (assoc tape ptr 1) :ptr (inc ptr) :state :b}
            [:a 1] {:tape (assoc tape ptr 0) :ptr (dec ptr) :state :c}
            [:b 0] {:tape (assoc tape ptr 1) :ptr (dec ptr) :state :a}
            [:b 1] {:tape (assoc tape ptr 1) :ptr (dec ptr) :state :d}
            [:c 0] {:tape (assoc tape ptr 1) :ptr (inc ptr) :state :d}
            [:c 1] {:tape (assoc tape ptr 0) :ptr (inc ptr) :state :c}
            [:d 0] {:tape (assoc tape ptr 0) :ptr (dec ptr) :state :b}
            [:d 1] {:tape (assoc tape ptr 0) :ptr (inc ptr) :state :e}
            [:e 0] {:tape (assoc tape ptr 1) :ptr (inc ptr) :state :c}
            [:e 1] {:tape (assoc tape ptr 1) :ptr (dec ptr) :state :f}
            [:f 0] {:tape (assoc tape ptr 1) :ptr (dec ptr) :state :e}
            [:f 1] {:tape (assoc tape ptr 1) :ptr (inc ptr) :state :a}))]
  (->> (reduce f {:ptr 0 :state :a :tape {0 0}} (range 12172063))
       :tape
       vals
       (reduce +)))
;; 2474
