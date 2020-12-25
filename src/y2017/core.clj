(ns y2017.core
  (:require [hashp.core]))

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
          (->> (for [a xs, b xs :when (not= a b)]
                 (if (= 0 (mod a b))
                   (/ a b)))
               (drop-while nil?)
               first))
      xs (->> (slurp "src/y2017/input201702")
              (re-seq #"[^\n]+")
              (map #(map clojure.edn/read-string (re-seq #"\d+" %))))]
  (map #(reduce + (map % xs)) [f g]))
;; (47623 312)


;; 201703
(let [n (clojure.edn/read-string (slurp "src/y2017/input201703"))
      a (int (Math/sqrt n))
      b (- n (* a a))
      a-square-location (map +
                             (map * (repeat 2 (quot a 2)) [1 -1] (if (odd? a) [1 1] [-1 -1]))
                             (if (odd? a) [0 0] [1 0]))
      directions (cycle [[1 0] [0 1] [-1 0] [0 -1]])
      edges (interleave (drop 1 (range)) (drop 1 (range)))
      spiral-directions (mapcat #(repeat % %2) edges directions)
      neighbor-offset (for [x (range -1 2 1), y (range -1 2 1) :when (not= [0 0] [x y])] [x y])
      neighbor-sum (fn [m xy]
                     (reduce + (map #(get m (map + xy %) 0) neighbor-offset)))]
  [(->> (map #(map * % %2)
             (map #(repeat 2 %) [(min 1 b) (min a (max (- b 1) 0)) (min a (max (- b a 1) 0))])
             (drop (if (even? a) 2 0) directions))
        (list* a-square-location)
        (reduce #(map + % %2))
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
      s (->> (slurp "src/y2017/input201704") (re-seq #"[^\n]+") (map #(re-seq #"\w+" %)))]
  (map #(reduce + (map % s)) [f g]))
;; (325 119)


;; 201705
(let [xs (->> (slurp "src/y2017/input201705") (re-seq #"-?\d+") (mapv clojure.edn/read-string))
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
          (let [max (reduce max xs)]
            (loop [c 0] (if (= (xs c) max) [c max] (recur (inc c))))))
      f (fn [xs]
          (let [c (count xs)
                [idx val] (g xs)]
            (->> (concat (repeat (inc idx) 0) (repeat val 1))
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
      input1 (->> input (re-seq #"[^\n]+") (map #(re-seq #"\w+" %)))
      base-map (reduce #(assoc %1 (first %2) (clojure.edn/read-string (second %2))) {} input1)
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
                 single-id (first (filter #(= single-value (m %)) subtowerids))
                 common-value (first (disj subtowervals-set single-value))]
             (if (subtowers single-id)
               (+ (base-map single-id) (- common-value single-value))
               single-value))
           (recur more (disj pending-towers base) (update m base #(apply + % subtowervals-seq)))))))])
;; ["aapssr" 1458]


;; 201708
(let [input (->> (slurp "src/y2017/input201708") (re-seq #"\S+") (partition 7))
      op {">" >, "<" <, ">=" >=, "<=" <=, "!=" not=, "==" =, "inc" +, "dec" -}
      f (fn [{:keys [registers highest] :as state} [op1 op0 op2 _ op4 op3 op5 :as t]]
          (if ((op op3) (get registers op4 0) (clojure.edn/read-string op5))
            (let [temp ((op op0) (get registers op1 0) (clojure.edn/read-string op2))
                  highest (max temp highest)]
              {:registers (assoc registers op1 temp) :highest highest})
            state))]
  (let [r (reduce f {:registers {} :highest 0} input)]
    [(second (apply max-key second (:registers r))) (:highest r)]))
;; [6343 7184]


;; 201709
(let [input (slurp "src/y2017/input201709")
      input' (clojure.string/replace input #"!." "")
      f (fn [{:keys [level total] :as state} ch]
          (cond
            (= ch \{) (update state :level inc)
            (= ch \}) {:level (dec level) :total (+ total level)}
            :else state))]
  [(:total (reduce f {:level 0 :total 0} (clojure.string/replace input' #"<.*?>" "")))
   (let [garbages (map count (re-seq #"<.*?>" input'))]
     (- (reduce + garbages) (* 2 (count garbages))))])
;; [12897 7031]


;; 201710
(let [addon [17 31 73 47 23]
      s (slurp "src/y2017/input201710")
      input1 (->> (re-seq #"\d+" s) (map clojure.edn/read-string))
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
   (->> (f input2 64) (partition 16) (map #(format "%02x" (reduce bit-xor %))) clojure.string/join)])
;; [48705 "1c46642b6f2bc21db2a2149d0aeeae5d"]


;; 201711
(let [input (->> (slurp "src/y2017/input201711") (re-seq #"\w+"))
      m (frequencies input)
      f (fn [m]
          (let [[a b c] [(- (m "sw") (m "ne")) (- (m "s") (m "n")) (- (m "se") (m "nw"))]
                [a' b' c'] (map #(Math/abs %) [a b c])]
            (cond
              (or (and (> a 0) (< b 0) (> c 0))
                  (and (< a 0) (> b 0) (< c 0)))
              (+ (max a' b' c') (min a' b' c'))
              (or (and (> a 0) (> b 0) (> c 0))
                  (and (< a 0) (< b 0) (< c 0)))
              (+ a' b' c' (- (min a' c')))
              (or (and (> a 0) (> b 0))
                  (and (< a 0) (< b 0)))
              (+ a' b' c' (- (min b' c')))
              :else
              (+ a' b' c' (- (min a' b'))))))]
  [(f m)
   (loop [[dir & more] input
          m {"sw" 0, "se" 0, "s" 0, "n" 0, "nw" 0, "ne" 0}
          furthest 0]
     (if dir
       (let [m (update m dir inc)]
         (recur more m (max furthest (f m))))
       furthest))])
;; [812 1603]


;; 201712
(let [s (->> (slurp "src/y2017/input201712") (re-seq #"[^\n]+") (map #(re-seq #"\d+" %)))
      f (fn [m [k & vs]]
          (assoc m k (set vs)))
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
                 (map clojure.edn/read-string)
                 (partition 2))
      f (fn [offset]
          (fn [[layer depth]]
            (let [round-trip (* 2 (dec depth))]
              (= 0 (mod (+ layer offset) round-trip)))))
      g (fn [r [layer depth]]
          (+ r (* layer depth)))
      h (fn [input offset]
          (map + (repeat offset)))]
  [(reduce g 0 (filter (f 0) input))
   (first (drop-while nil? (map #(if (every? false? (filter (f %) input)) %) (range))))])
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
(let [[a b] (->> (slurp "src/y2017/input201715") (re-seq #"\d+") (map clojure.edn/read-string))
      [factor-a factor-b] [16807 48271]
      f (fn [factor] #(mod (* factor %) 2147483647))
      g (fn [x] (bit-and 65535 x))]
  [(loop [r 0, [a b & more]
          (interleave
           (->> (iterate (f factor-a) a) (drop 1) (take 40000000) (map g))
           (->> (iterate (f factor-b) b) (drop 1) (take 40000000) (map g)))]
     (if a
       (recur (if (= a b) (inc r) r) more)
       r))
   (loop [r 0, [a b & more]
          (interleave
           (->> (iterate (f factor-a) a) (drop 1) (filter #(= 0 (mod % 4))) (take 5000000) (map g))
           (->> (iterate (f factor-b) b) (drop 1) (filter #(= 0 (mod % 8))) (take 5000000) (map g)))]
     (if a
       (recur (if (= a b) (inc r) r) more)
       r))])
;; [650 336]


;; 201716
(let [input (->> (slurp "src/y2017/input201716") (re-seq #"[^,]+"))
      s "abcdefghijklmnop"
      f (fn [s] (fn [c] (.indexOf (apply str s) c)))
      g (fn [s]
          (loop [[ops & more] input, o s]
            (if ops
              (let [[a b] (re-seq #"\w+" (subs ops 1))]
                (condp = (subs ops 0 1)
                  "s" (let [a (- 16 (clojure.edn/read-string a))]
                        (recur more (concat (drop a o) (take a o))))
                  "x" (let [[a b] (map clojure.edn/read-string [a b])
                            o (vec o)]
                        (recur more (-> o (assoc a (get o b)) (assoc b (get o a)))))
                  "p" (let [[a b] (map (f o) [a b])
                            o (vec o)]
                        (recur more (-> o (assoc a (get o b)) (assoc b (get o a)))))))
              (apply str o))))]
  [(g s)
   (loop [[s1 & more] (iterate g s)
          cache []
          seen #{}]
     (if (seen s1)
       (let [cache' (drop-while #(not= s1 %) cache)]
         (nth cache (mod (- 1000000000 (count cache)) (count cache))))
       (recur more (conj cache s1) (conj seen s1))))])
;; ["kbednhopmfcjilag" "fbmcgdnjakpioelh"]


;; 201717
(let [input (clojure.edn/read-string (slurp "src/y2017/input201717"))]
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
(let [input (->> (slurp "src/y2017/input201718") (re-seq #"[^\n]+") (map #(re-seq #"\S+" %)) vec)
      f (fn [r op] (r op (clojure.edn/read-string op)))
      vm0 (loop [r {}, freq nil, ip 0]
            (if-let [[op0 op1 op2] (get input ip nil)]
              (if (and (= op0 "rcv") (not= 0 (f r op1)))
                freq
                (let [r (condp = op0
                          "set" (assoc r op1 (f r op2))
                          "add" (update r op1 (fnil #(+   % (f r op2)) 0))
                          "mul" (update r op1 (fnil #(*   % (f r op2)) 0))
                          "mod" (update r op1 (fnil #(mod % (f r op2)) 0))
                          "jgz" r
                          "snd" r
                          "rcv" r)
                      freq (if (= op0 "snd") (f r op1) freq)
                      ip (+ ip (if (and (= op0 "jgz") (> (f r op1) 0)) (f r op2) 1))]
                  (recur r freq ip)))))
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
                          "rcv" (do (swap! rcvq rest) (assoc-in m [:r op1] rcv)))
                      m (assoc (update m :ip inc) :status :running)]
                  (recur m)))))]
  [vm0
   (loop [p0 (vm1 {:r {"p" 0} :ip 0 :snd 0 :status :running :sndq p0-sndq :rcvq p1-sndq})
          p1 (vm1 {:r {"p" 1} :ip 0 :snd 0 :status :running :sndq p1-sndq :rcvq p0-sndq})]
     (if (and (= (:status p0) (:status p1) :rcv2))
       (:snd p1)
       (recur (vm1 p0) (vm1 p1))))])
;; [9423 7620]


;; 201719
(let [input (slurp "src/y2017/input201719")
      target (into #{\+} (map char (range 65 91))) ;; \+ and A-Z
      positions (loop [n 0, [c & more] (.replace input "\n" ""), m {}]
                  (if c
                    (recur (inc n) more (into m (if (target c) [[n c]] [])))
                    m))
      input (re-seq #"[^\n]+" input)
      entry-line (first input)
      [w h] [(count entry-line) (count input)]
      entry (.indexOf entry-line "|")]

  )


;; 201720
(let [input (->> (slurp "src/y2017/input201720") (re-seq #"-?\d+") (map clojure.edn/read-string) (partition 9))
      f (fn [idx [p0 p1 p2 p3 p4 p5 p6 p7 p8 :as p]]
          [(+ (Math/abs p6) (Math/abs p7) (Math/abs p8)) idx p])
      v-a-same-sign? (fn [_ _ [_ _ _ p3 p4 p5 p6 p7 p8]]
                       (every? pos? (apply map * [p3 p4 p5] [p6 p7 p8])))
      sorted-by-acceleration (->> (map-indexed f input) sort)
      lowest-acceleration (take-while #(= (first %) (ffirst sorted-by-acceleration))
                                      sorted-by-acceleration)]
  (if (= 1 (count lowest-acceleration))
    (->> lowest-acceleration first second)
    (loop [lowest-acceleration lowest-acceleration]
      (if (every? v-a-same-sign? lowest-acceleration)
        
        (recur (map evolve lowest-acceleration))))))


;; 201721
(let [start ".#./..#/###"])


;; 201722
;; dirs: [N E S W] = [0 1 2 3
;; flags: [Clean, Weakened, Infected, Flagged] = [0 1 2 3]
(let [dir-map {0 [0 -1], 1 [1 0], 2 [0 1], 3 [-1 0]}
      s (slurp "src/y2017/input201722")
      input (->> s (re-seq #"[^\n]+"))
      s (-> s (.replace "\n" "") (.replace "." "0") (.replace "#" "2"))
      [w h] [(count (first input)) (count input)]
      m (into {} (for [a (range h), b (range w) :let [o (+ (* a w) b)]]
                   [[b a] (clojure.edn/read-string (subs s o (inc o)))]))]
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
(let [input (->> (slurp "src/y2017/input201723") (re-seq #"[^\n]+") (map #(re-seq #"\S+" %)) vec)
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
                 "set" (assoc r op1 (r op2 (clojure.edn/read-string op2)))
                 "sub" (update r op1 #(- % (r op2 (clojure.edn/read-string op2))))
                 "mul" (update r op1 #(* % (r op2 (clojure.edn/read-string op2))))
                 "jnz" r)
             mul (if (= op0 "mul") (inc mul) mul)
             ip (if (and (= op0 "jnz") (not= (r op1 (clojure.edn/read-string op1)) 0))
                  (+ ip (clojure.edn/read-string op2))
                  (inc ip))]
         (recur r mul ip))
       mul))
   (loop [[x & more] (range 109900 (+ 126900 17) 17)
          c 0]
     (if x
       (recur more (+ c (f x)))
       c))])
;; [9409 913]


;; 201724
(let [input (->> (slurp "src/y2017/input201724") (re-seq #"\d+") (map clojure.edn/read-string) (partition 2))
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
                    (neighbors node) (recur (update m node (g node)) node (+ cost node node))
                    :else
                    (reduce max (map #(h (-> m
                                             (update % (g node))
                                             (update node (g %)))
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
                    (neighbors node) (recur (update m node (g node)) node (+ cost node node) (inc depth))
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
