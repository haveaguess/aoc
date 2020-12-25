(ns y2016.core
  (:import [java.security MessageDigest]))

;; 201601
(let [dir-map {0   [0  1],  90 [ 1 0]
               180 [0 -1], 270 [-1 0]}
      temp (reduce #(let [{:keys [xy dir]} %1
                          [next-dir step] %2
                          step (Integer/parseInt step)
                          dir (mod (+ dir (if (= "L" next-dir) 270 90)) 360)
                          new-xy (map + (map * (dir-map dir) [step step]) xy)]
                      {:dir dir
                       :xy new-xy
                       :r (conj (:r %1) new-xy)})
                   {:xy [0 0] :dir 0 :r [[0 0]]}
                   (->> (re-seq #"\d+|L|R" (slurp "src/y2016/input201601"))
                        (partition 2)))
      f (fn [[[x0 y0] [x1 y1]]]
          (if (= x0 x1)
            (for [b (range y0 y1 (if (< y0 y1) 1 -1))]
              [x0 b])
            (for [a (range x0 x1 (if (< x0 x1) 1 -1))]
              [a y0])))
      g (fn [[x y]] (+ (Math/abs x) (Math/abs y)))]
  [(g (:xy temp))
   (loop [paths (concat (mapcat f (partition 2 1 (:r temp))) [(:xy temp)])
          seen #{}]
     (if (seen (first paths))
       (g (first paths))
       (recur (rest paths) (conj seen (first paths)))))])
;; [226 79]


;; 201602
(let [trans-map1 {1 [1 4 1 2]
                  2 [2 5 1 3]
                  3 [3 6 2 3]
                  4 [1 7 4 5]
                  5 [2 8 4 6]
                  6 [3 9 5 6]
                  7 [4 7 7 8]
                  8 [5 8 7 9]
                  9 [6 9 8 9]}
      trans-map2 {1 [1 3 1 1]
                  2 [2 6 2 3]
                  3 [1 7 2 4]
                  4 [4 8 3 4]
                  5 [5 5 5 6]
                  6 [2 \A 5 7]
                  7 [3 \B 6 8]
                  8 [4 \C 7 9]
                  9 [9 9 8 9]
                  \A [6 \A \A \B]
                  \B [7 \D \A \C]
                  \C [8 \C \B \C]
                  \D [\B \D \D \D]}
      code-map {\U 0 \D 1 \L 2 \R 3 \newline \space}
      f (fn [trans-map]
          (fn [value item]
            (let [{:keys [r curr]} value]
              (if (= \space item)
                (assoc value :r (conj r curr))
                (assoc value :curr (get-in trans-map [curr item]))))))
      g (f trans-map1)
      h (f trans-map2)]
  (map #(->> (slurp "src/y2016/input201602")
             (map code-map)
             (reduce % {:r [] :curr 5})
             :r
             (clojure.string/join))
       [g h]))
;; ("48584" "563B6")


;; 201603
(let [input0 (map #(Integer/parseInt %) (re-seq #"\d+" (slurp "src/y2016/input201603")))
      input1 (partition 3 input0)
      input2 (partition 9 input0)
      valid?3 (fn [[a b c]]
                (and (< c (+ a b))
                     (< b (+ a c))
                     (< a (+ b c))))
      valid?9 (fn [[a b c d e f g h i]]
                [(valid?3 [a d g])
                 (valid?3 [b e h])
                 (valid?3 [c f i])])]
  [(->> (map    valid?3 input1) (remove false?) count)
   (->> (mapcat valid?9 input2) (remove false?) count)])
;; [869 1544]


;; 201604
(let [inputs (re-seq #"([a-z-]+)(\d+)\[(\w+)]" (slurp "src/y2016/input201604"))
      f (fn [[_ strs id crc]]
          (let [fmap (frequencies (remove #{\-} strs))
                crc' (->> (into (sorted-map-by (fn [key1 key2]
                                                 (compare [(get fmap key2) key1]
                                                          [(get fmap key1) key2])))
                                fmap)
                          keys
                          (take 5))]
            (if (= crc' (seq crc)) (Integer/parseInt id) 0)))
      g (fn [[_ strs id _]]
          (let [id (Integer/parseInt id)
                f #(char (+ 97 (mod (+ (int %) -97 id) 26)))
                decrypted (clojure.string/join " " (map #(apply str (map f %)) (re-seq #"\w+" strs)))]
            (if (re-seq #"northpole" decrypted)
              id)))]
  [(reduce + (map f inputs))
   (remove nil? (map g inputs))])
;; [245102 (324)]


;; 201605
(let [input (first (re-seq #"\w+" (slurp "src/y2016/input201605")))
      algorithm (MessageDigest/getInstance "MD5")
      five0 [\0 \0 \0 \0 \0]
      md5 (fn [^String s]
            (let [raw (.digest algorithm (.getBytes s))]
              (format "%032x" (BigInteger. 1 raw))))
      xseq (->> (range)
                (map (fn [n]
                       (let [hash (md5 (str input n))]
                         (if (= five0 (take 5 hash)) hash))))
                (remove nil?))]
  [(apply str (map #(nth % 5) (take 8 xseq)))
   (loop [xseq xseq
          r [0 0 0 0 0 0 0 0]
          remaining #{\0 \1 \2 \3 \4 \5 \6 \7}]
     (if (empty? remaining)
       (apply str r)
       (let [n (first xseq)
             n5 (nth n 5)
             n6 (nth n 6)]
         (recur (rest xseq)
                (if (remaining n5)
                  (assoc r (- (int n5) 48) n6)
                  r)
                (disj remaining n5)))))])
;; ["2414bc77" "437e60fc"]


;; 201606
(let [f (fn [func] (->> (re-seq #"\w+" (slurp "src/y2016/input201606"))
                        (apply map #(key (apply func val (frequencies %&))))
                        (apply str)))]
  (map f [max-key min-key]))
;; ("umcvzsmw" "rwqoacfz")


;; 201607
(let [input (re-seq #"[^\n]+" (slurp "src/y2016/input201607"))
      abba? (fn [s]
              (some #(and (re-seq #"(.)(.)\2\1" (apply str %))
                          (= 2 (count (set %))))
                    (partition 4 1 s)))
      aba? (fn [s]
             (filter #(and (re-seq #"(.).\1" (apply str %))
                           (= 2 (count (set %))))
                     (partition 3 1 s)))
      f (fn [s]
          (let [s1 (re-seq #"(?<=^|])\w+" s)
                s2 (re-seq #"(?<=\[)\w+"  s)]
            (and (some abba? s1) (not-any? abba? s2))))
      g (fn [s]
          (let [s1 (re-seq #"(?<=^|])\w+" s)
                s2 (re-seq #"(?<=\[)\w+"  s)]
            (seq (clojure.set/intersection
                  (set (map rest        (mapcat aba? s1)))
                  (set (map #(take 2 %) (mapcat aba? s2)))))))]
  (map #(count (filter identity (map % input))) [f g]))
;; (118 260)


;; 201608
(let [f (fn [board [ops a b]]
          (let [[a b] (map #(Integer/parseInt %) [a b])]
            (cond
              (= ops "rect") (reduce #(assoc % %2 1)
                                     board
                                     (for [x (range a), y (range b)]
                                       (+ (* y 50) x)))
              (= ops "x=") (reduce #(assoc % %2 (nth board (if (>= %2 (* 50 b))
                                                             (- %2 (* 50 b))
                                                             (- (+ 300 %2) (* 50 b)))))
                                   board
                                   (for [y (range 6)]
                                     (+ (* y 50) a)))
              (= ops "y=") (reduce #(assoc % %2 (nth board (if (>= (mod %2 50) b) (- %2 b) (- (+ 50 %2) b))))
                                   board
                                   (for [x (range 50)]
                                     (+ (* a 50) x))))))
      temp (reduce f
                   (vec (repeat 300 0))
                   (->> (slurp "src/y2016/input201608")
                        (re-seq #"rect|x=|y=|\d+")
                        (partition 3)))]
  [(count (filter pos? temp))
   (dorun (map #(prn (apply str %)) (partition 50 (map #(if (= 1 %) "▓" "░") temp))))])
;; [116 nil]
;; "▓░░▓░▓▓▓░░░▓▓░░░░▓▓░▓▓▓▓░▓░░░░▓▓▓░░░▓▓░░▓▓▓▓░▓▓▓▓░"
;; "▓░░▓░▓░░▓░▓░░▓░░░░▓░▓░░░░▓░░░░▓░░▓░▓░░▓░▓░░░░░░░▓░"
;; "▓░░▓░▓░░▓░▓░░▓░░░░▓░▓▓▓░░▓░░░░▓▓▓░░▓░░░░▓▓▓░░░░▓░░"
;; "▓░░▓░▓▓▓░░▓░░▓░░░░▓░▓░░░░▓░░░░▓░░▓░▓░░░░▓░░░░░▓░░░"
;; "▓░░▓░▓░░░░▓░░▓░▓░░▓░▓░░░░▓░░░░▓░░▓░▓░░▓░▓░░░░▓░░░░"
;; "░▓▓░░▓░░░░░▓▓░░░▓▓░░▓░░░░▓▓▓▓░▓▓▓░░░▓▓░░▓▓▓▓░▓▓▓▓░"


;; 201609
(let [f (fn f [s]
          (loop [text s, r1 0, r2 0]
            (cond
              (= text "")
              [r1 r2]

              (= (first text) \()
              (let [[s a b] (first (re-seq #"\((\d+)x(\d+)\)" text))
                    [a b] (map #(Integer/parseInt %) [a b])
                    new-text (subs text (+ (count s) a))]
                (recur new-text
                       (+ r1 (* a b))
                       (+ r2 (* ((f (subs text (count s) (+ (count s) a))) 1) b))))

              :else
              (recur (subs text 1) (inc r1) (inc r2)))))]
  (f (slurp "src/y2016/input201609")))
;; [138735 11125026826]


;; 201610
(let [f (fn [r [v1 v2 v3 v4 v5 v6]]
          (cond
            (= "\n" v1) r
            (= "value" v1) (update-in r [:value-chips v4] #(conj % (Integer/parseInt v2)))
            :else (assoc-in r [:bots-logic v2] [v3 v4 v5 v6])))
      {:keys [bots-logic value-chips]} (->> (slurp "src/y2016/input201610")
                                            (re-seq #"[^\n]+")
                                            (map #(re-seq #"\d+|bot|output|value" %))
                                            (reduce f {}))]
  (loop [value-chips value-chips
         q1-bot nil
         q2-product []]
    (let [[ready-bot values] (first (filter #(= 2 (count (second %))) value-chips))
          q1-bot (if (and (nil? q1-bot) (= (set values) #{17 61})) ready-bot q1-bot)
          [v1 v2] (sort values)
          [lowtarget lowvalue hightarget highvalue] (bots-logic ready-bot)
          q2-product (if (and (= "output" lowtarget) (#{"0" "1" "2"} lowvalue))
                       (conj q2-product v1)
                       q2-product)]
      (cond
        (and q1-bot (= 3 (count q2-product))) [q1-bot (reduce * q2-product)]
        :else (let [value-chips (if (= "output" lowtarget)
                                  value-chips
                                  (update value-chips lowvalue #(conj % v1)))
                    value-chips (if (= "output" hightarget)
                                  value-chips
                                  (update value-chips highvalue #(conj % v2)))]
                (recur (dissoc value-chips ready-bot) q1-bot q2-product))))))
;; ["181" 12567]


;; 201611
;; 4
;; 3 PG PM RG RM
;; 2 PLM SM
;; 1 TG TM PLG SG

;; 201612
(let [instructions (->> (slurp "src/y2016/input201612") (re-seq #"[^\n]+") vec)
      f (fn [v memory]
          (if (#{"a" "b" "c" "d"} v) (memory (keyword v)) (Integer/parseInt v)))
      g (fn [memory]
          (loop [ip 0
                 memory memory]
            (if-let [instruction (get instructions ip)]
              (let [[op v1 v2] (re-seq #"\S+" instruction)]
                (cond
                  (= "cpy" op) (recur (inc ip) (update memory (keyword v2) (constantly (f v1 memory))))
                  (= "inc" op) (recur (inc ip) (update memory (keyword v1) inc))
                  (= "dec" op) (recur (inc ip) (update memory (keyword v1) dec))
                  (= "jnz" op) (recur (+ ip (if (not= 0 (f v1 memory)) (Integer/parseInt v2) 1)) memory)))
              (:a memory))))]
  (map g [{:a 0 :b 0 :c 0 :d 0} {:a 0 :b 0 :c 1 :d 0}]))
;; (318020 9227674)

;; 201613
(let [fav (->> (slurp "src/y2016/input201613") (re-seq #"\d+") first Integer/parseInt)
      goal [31 39]
      open? (fn [x y]
              (->> (Integer/toBinaryString (+ (* (+ x y) (+ x y 1)) (* 2 x) fav))
                   (filter #{\1})
                   count
                   even?))
      neighbors (fn [[x y]]
                  (for [[offset-x offset-y] [[-1 0] [1 0] [0 1] [0 -1]]
                        :let [x' (+ x offset-x), y' (+ y offset-y)]
                        :when (and (>= x' 0) (>= y' 0) (open? x' y'))]
                    [x' y']))
      estimate-cost (fn [xy] (reduce + (map #(Math/abs (- %1 %2)) xy goal)))]
  [(loop [work-todo (sorted-set [0 [1 1]])
          cost-table {[1 1] 0}]
     (let [[_ current-xy :as work-item] (first work-todo)
           cost-so-far (cost-table current-xy)]
       (if (= current-xy goal)
         cost-so-far
         (let [rest-work-todo (disj work-todo work-item)
               nbr-xys (neighbors current-xy)]
           (recur
            (into rest-work-todo (for [nbr nbr-xys
                                       :let [cost (get cost-table nbr 999999)]
                                       :when (or (= 999999 cost) (< cost-so-far cost))]
                                   [(+ cost-so-far 1 (estimate-cost nbr)) nbr]))
            (into cost-table (for [nbr nbr-xys
                                   :let [cost (get cost-table nbr 999999)]]
                               [nbr (min cost (inc cost-so-far))])))))))
   (->> (iterate #(reduce (partial into) #{} (for [xy %] (neighbors xy))) #{[1 1]})
        (take 51)
        (reduce clojure.set/union)
        count)])
;; [86 127]

;; 201614
(map (fn [q]
       (let [algorithm (MessageDigest/getInstance "MD5")
             md5' (fn [^String s]
                    (format "%032x" (BigInteger. 1 (.digest algorithm (.getBytes s)))))
             md5 (if (= q 1)
                   md5'
                   (fn [^String s]
                     (reduce (fn [s _] (md5' s)) (md5' s) (range 2016))))
             salt (->> (slurp "src/y2016/input201614") (re-seq #"\w+") first)
             hash (fn hash [n]
                    (lazy-seq
                     (cons {:n n :md5 (md5 (str salt n))} (hash (inc n)))))
             hashes (hash 0)
             triples    (keep #(if-let [xs (re-seq #"(.)\1\1"  (:md5 %))]
                                 (assoc % :d (second (first xs))))
                              hashes)
             quintuples (keep #(if-let [xs (re-seq #"(.)\1{4}" (:md5 %))]
                                 (assoc % :ds (set (map second xs))))
                              hashes)
             f (fn [n d]
                 (->> quintuples
                      (drop-while #(<= (:n %) n))
                      (take-while #(<= (:n %) (+ n 1000)))
                      (filter #((:ds %) d))
                      seq))]
         (loop [[hash3 & more-hash3] triples
                n 0]
           (if (f (:n hash3) (:d hash3))
             (if (= n 63)
               (:n hash3)
               (recur more-hash3 (inc n)))
             (recur more-hash3 n)))))
     [1 2])
;; (35186 22429)

;; 201615
(let [input1 (->> (slurp "src/y2016/input201615") (re-seq #"\d+") (map #(Integer/parseInt %)) (partition 4))
      input2 (concat input1 '((0 11 0 0)))
      f (fn [input]
          (let [c (count input)
                f (fn [offset [_ total-positions _ start-position]]
                    [total-positions (+ total-positions offset start-position)])
                g (fn [ellipse [multiplier remainder]]
                    (= 0 (mod (+ remainder ellipse) multiplier)))
                adjusted-positions (map f (range 1 (inc c)) input)
                max-disc (apply max-key first adjusted-positions)
                step (first max-disc)
                offset (- step (mod (second max-disc) step))]
            (loop [total offset]
              (if (= #{true} (set (map #(g total %) adjusted-positions)))
                total
                (recur (+ step total))))))]
  (map f [input1 input2]))
;; (203660 2408135)


;; 201616
(map (fn [len]
       (let [f #(apply str (map {\0 \1, \1 \0} (reverse %)))
             g #(repeat (seq %))
             input-x (->> (slurp "src/y2016/input201616") (re-seq #"\w+") first)
             input-X (f input-x)
             interpolation-len (/ len (inc (count input-x)))
             interpolate (fn [s]
                           (if (> (count s) interpolation-len)
                             s
                             (recur (str s "0" (f s)))))
             interpolation (interpolate "001")
             interpolation-1 (take-nth 2 interpolation)
             interpolation-2 (take-nth 2 (drop 1 interpolation))
             x (->> (interleave (g input-x) interpolation-1 (g input-X) interpolation-2)
                    flatten
                    (take len)
                    (apply str))
             checksum (fn [s]
                        (if (odd? (count s))
                          s
                          (recur (apply str (map (fn [[x y]] (if (= x y) \1 \0)) (partition 2 s))))))]
         (checksum x))) [272 35651584])
;; ("11100111011101111" "10001110010000110")

;; 201617
(let [input (first (re-seq #"\w+" (slurp "src/y2016/input201617")))
      dirs {0 [\U 0 -1], 1 [\D 0 1], 2 [\L 1 0], 3 [\R -1 0]}
      goal [3 3]
      algorithm (MessageDigest/getInstance "MD5")
      md5 (fn [^String s]
            (let [raw (.digest algorithm (.getBytes s))]
              (format "%032x" (BigInteger. 1 raw))))
      walk (fn [s] (keep-indexed #(if (#{\b \c \d \e \f} %2) (dirs %)) (subs (md5 s) 0 4)))
      neighbors (fn [[x y] [_ offset-x offset-y]]
                  (let [x' (+ x offset-x), y' (+ y offset-y)]
                    (when (and (>= x' 0) (>= y' 0))
                      [x' y'])))
      estimate-cost (fn [xy] (reduce + (map #(Math/abs (- %1 %2)) xy goal)))]
  (loop [passcode input
         pos [0 0]]
    (let [open-doors (map #(neighbors pos %) (walk input))]
      open-doors)))

;; 201618
(map (fn [rows]
       (let [first-row (first (re-seq #"\S+" (slurp "src/y2016/input201618")))
             trap #{[\^ \^ \.] [\. \^ \^] [\. \. \^] [\^ \. \.]}
             f (fn [s] (apply str (map #(if (trap %) \^ \.) (partition 3 1 (str "." s ".")))))]
         (->> (iterate f first-row) (take rows) (apply str) (reduce #(if (= %2 \.) (inc %) %) 0))))
     [40 400000])
;; (1951 20002936)


;; 201619
;; 12345678901234567890123456  knock=13,n1=12,n2=12,total-crossed=9
;;              -- -- -- -- -
;; 1234567890123456789012345   knock=12,n1=12,n2=13,total-crossed=9
;;             - -- -- -- --
(let [input (->> (slurp "src/y2016/input201619") (re-seq #"\d+") first Integer/parseInt)
      input 3014603
      f (fn [r [a b]]
          (if (nil? b)
            (cons a r)
            (conj r a)))
      g (fn [n elves]
          (let [knock (quot n 2)
                total-crossed (quot (+ n 2) 3)
                n1 (- (quot n 2) (if (even? n) 1 0))
                n2 (if (even? n) n1 (inc n1))
                part1 (take n1 elves)
                part2 (drop n2 elves)]
            [(- n total-crossed)
             (concat (drop total-crossed part1) (take-nth 3 part2) (take total-crossed part1))]))]
  [(loop [[a b c :as elves] (range 1 (inc input))]
     (if (nil? c)
       a
       (recur (reduce f [] (partition 2 2 [nil] elves)))))
   (loop [[n [a b c d e :as elves]] [input (range 1 (inc input))]]
     (cond
       (nil? c) a
       (nil? d) c
       (nil? e) a
       :else (recur (g n elves))))])
;; [1834903 1420280]


;; 201620
(let [input (->> (slurp "src/y2016/input201620")
                 (re-seq #"\d+")
                 (map clojure.edn/read-string)
                 (partition 2)
                 vec
                 (sort-by first))
      f (fn [[total low high] [a b]]
          (cond
            (= -1 low) [0 a b]
            (or (and (<= low b) (<= a high))
                (and (>= low b) (>= a high))) [total (min low a) (max high b)]
            :else [(+ total (- high low -1)) a b]))]
  [(loop [lower-bound 0
          [[start end] :as input] input]
     (if (<= start lower-bound)
       (recur (max lower-bound (inc end)) (rest input))
       lower-bound))
   (let [[total low high] (reduce f [0 -1 -1] input)]
     (- 4294967296 total (- high low -1)))])
;; [22887907 109]


;; 201621
(let [input (->> (slurp "src/y2016/input201621") (re-seq #"[^\n]+") (map #(re-seq #"\S+" %)))
      rotate (fn [s offset]
               (let [offset (mod offset (count s))] (str (subs s offset) (subs s 0 offset))))
      rotate2 (fn [s [letter]]
                (let [index (->> (keep-indexed #(if (= letter %2) %1) s) first)]
                  (rotate s (- (count s) (mod (+ 1 index (if (>= index 4) 1 0)) (count s))))))
      rotate3 (fn [s [letter]]
                (let [index (->> (keep-indexed #(if (= letter %2) %1) s) first)
                      l (count s)
                      m (zipmap (map #(mod (+ % % 1 (if (>= % 4) 1 0)) l) (range l)) (range l))]
                  (rotate s (- index (m index)))))
      f-swap (fn [s p1 p2 p5]
               (cond
                 (= "position" p1) (let [s (vec s)
                                         [p2 p5] (map #(Integer/parseInt %) [p2 p5])]
                                     (apply str (-> s (assoc p2 (s p5)) (assoc p5 (s p2)))))
                 (= "letter" p1)   (let [[p2 p5] (map first [p2 p5])
                                         m {p2 p5, p5 p2}]
                                     (apply str (map #(get m % %) s)))))
      f-reverse (fn [s p2 p4]
                  (let [s (apply str s)
                        [p2 p4] (map #(Integer/parseInt %) [p2 p4])]
                    (str (subs s 0 p2)
                         (clojure.string/reverse (subs s p2 (inc p4)))
                         (subs s (inc p4)))))
      f-move (fn [s p2 p5 & [rev?]]
               (let [[p2 p5] (map #(Integer/parseInt %) [p2 p5])
                     [p2 p5 rev?] (if (< p2 p5) [p2 p5 rev?] [p5 p2 (not rev?)])]
                 (str (subs s 0 p2)
                      (apply str (rotate (subs s p2 (inc p5)) (if rev? -1 1)))
                      (subs s (inc p5)))))
      f (fn [s [p0 p1 p2 p3 p4 p5 p6]]
          (cond
            (= "swap" p0) (f-swap s p1 p2 p5)
            (= "rotate" p0) (cond
                              (= "left"  p1) (rotate s (Integer/parseInt p2))
                              (= "right" p1) (rotate s (- (count s) (Integer/parseInt p2)))
                              (= "based" p1) (rotate2 s p6))
            (= "reverse" p0) (f-reverse s p2 p4)
            (= "move" p0) (f-move s p2 p5)))
      g (fn [s [p0 p1 p2 p3 p4 p5 p6 :as op]]
          (cond
            (= "swap" p0) (f-swap s p1 p5 p2)
            (= "rotate" p0) (cond
                              (= "right" p1) (rotate s (Integer/parseInt p2))
                              (= "left"  p1) (rotate s (- (count s) (Integer/parseInt p2)))
                              (= "based" p1) (rotate3 s p6))
            (= "reverse" p0) (f-reverse s p2 p4)
            (= "move" p0) (f-move s p2 p5 :rev)))]
  [(reduce f "abcdefgh" input)
   (reduce g "fbgdceah" (reverse input))])
;; ["bdfhgeca" "gdfcabeh"]


;; 201622
;; root@ebhq-gridcenter# df -h
;; Filesystem              Size  Used  Avail  Use%
;; /dev/grid/node-x0-y0     94T   73T    21T   77%
(let [input (->> (slurp "src/y2016/input201622") (re-seq #"\d+") (map #(Integer/parseInt %)) (partition 6))]
  (count (for [[x1 y1 s1 u1 a1 _] input
               [x2 y2 s2 u2 a2 _] input
               :when (and (not= [x1 y1] [x2 y2]) (<= u1 a2) (< 0 u1))]
           1)))


;; 201623
(let [instructions (->> (slurp "src/y2016/input201623") (re-seq #"[^\n]+") vec)
      f (fn [v memory]
          (if (#{"a" "b" "c" "d"} v) (memory (keyword v)) (Integer/parseInt v)))
      g (fn [memory]
          (loop [ip 0
                 memory memory]
            (if-let [instruction (get instructions ip)]
              (let [[op v1 v2] (re-seq #"\S+" instruction)]
                (cond
                  (= "cpy" op) (recur (inc ip) (update memory (keyword v2) (constantly (f v1 memory))))
                  (= "inc" op) (recur (inc ip) (update memory (keyword v1) inc))
                  (= "dec" op) (recur (inc ip) (update memory (keyword v1) dec))
                  (= "jnz" op) (recur (+ ip (if (not= 0 (f v1 memory)) (Integer/parseInt v2) 1)) memory)))
              (:a memory))))]
  (map g [{:a 0 :b 0 :c 0 :d 0} {:a 0 :b 0 :c 1 :d 0}])
  instructions)

;; 201624
(let [input (->> (slurp "src/y2016/input201624") (re-seq #"[^\n]+"))]
  input)

;; 201625
(let [input (->> (slurp "src/y2016/input201625") (re-seq #"[^\n]+"))]
  input)
