(ns y2015.core
  (:require [clojure.edn :as edn]
            [clojure.string :as str])
  (:import [java.security MessageDigest]))

(defn- permutations [s]
  (lazy-seq
   (if (next s)
     (for [head s
           tail (permutations (disj s head))]
       (cons head tail))
     [s])))

;; 201501
(let [input (->> (slurp "src/y2015/input201501") (map {\( 1, \) -1}))]
  [(reduce + input)
   (->> (map #(if (= -1 %) %2) (reductions + input) (rest (range)))
        (some identity))])
;; [232 1783]


;; 201502
(let [f (fn [[w l h]]
          (let [n0 (* w l)
                n1 (* w h)
                n2 (* l h)
                n3 (min n0 n1 n2)
                n4 (max w l h)
                n (+ w l h)]
            [(+ n0 n1 n2 n0 n1 n2 n3)
             (+ (* w l h) (* 2 (- n n4)))]))]
  (->> (slurp "src/y2015/input201502")
       (re-seq #"\d+")
       (map edn/read-string)
       (partition 3)
       (map f)
       (apply map +)))
;; (1598415 3812909)


;; 201503
(let [dirs {\^ [0 1], \v [0 -1], \< [-1 0], \> [1 0]}
      input (->> (slurp "src/y2015/input201503") (map dirs))
      f (fn [input] (->> (reductions #(map + % %2) [0 0] input) (into #{})))]
  [(->> (f input) count)
   (->> [(take-nth 2 input) (take-nth 2 (rest input))] (map f) (apply into) count)])
;; [2565 2639]


;; 201504
(let [input (slurp "src/y2015/input201504")
      algorithm (MessageDigest/getInstance "MD5")
      five0 [\0 \0 \0 \0 \0]
      six0  [\0 \0 \0 \0 \0 \0]
      md5 (fn [^String s] (format "%032x" (BigInteger. 1 (.digest algorithm (.getBytes s)))))
      f (fn [regex] (->> (range) rest (some #(if (re-seq regex (md5 (str input %))) %))))]
  (map f [#"^00000" #"^000000"]))
;; (254575 1038736)


;; 201505
(let [vowels #{\a \e \i \o \u}
      nice1? (fn [s]
               (and (<= 3 (count (filter vowels s)))
                    (some #(= (first %) (second %)) (partition 2 1 s))
                    (not-any? #{[\a \b] [\c \d] [\p \q] [\x \y]} (partition 2 1 s))))
      nice2? (fn [s] (and (re-seq #"(..).*\1" s) (re-seq #"(.).\1" s)))
      input (->> (slurp "src/y2015/input201505") (re-seq #".+"))]
  (->> [nice1? nice2?] (map #(count (filter % input)))))
;; (258 53)


;; 201506
(let [f (fn [[action & nums]]
          (cons action (map edn/read-string nums)))
      input (map f (->> (slurp "src/y2015/input201506") (re-seq #"toggle|on|off|\d+") (partition 5)))
      q1 (let [arr (int-array 1000000)]
           (dorun (for [[action a b c d] input
                        x (range a (inc c))
                        y (range b (inc d))
                        :let [idx (+ (* 1000 x) y)]]
                    (cond (= action "on")  (aset ^"[I" arr idx 1)
                          (= action "off") (aset ^"[I" arr idx 0)
                          :else            (aset ^"[I" arr idx (- 1 (aget ^"[I" arr idx))))))
           (reduce + arr))
      q2 (let [arr (int-array 1000000)]
           (dorun (for [[action a b c d] input
                        x (range a (inc c))
                        y (range b (inc d))
                        :let [idx (+ (* 1000 x) y)
                              tmp (aget ^"[I" arr idx)]]
                    (cond (= action "on")  (aset ^"[I" arr idx (inc tmp))
                          (= action "off") (aset ^"[I" arr idx (if (= 0 tmp) 0 (dec tmp)))
                          :else            (aset ^"[I" arr idx (+ 2 tmp)))))
           (reduce + arr))]
  [q1 q2])
;; [543903 14687245]


;; 201507
(let [r (loop [inputs (->> (slurp "src/y2015/input201507") (re-seq #"\S+"))
               r ()]
          (if (seq inputs)
            (let [[op1 op2 op3 op4 op5] inputs]
              (cond
                (= "NOT"    op1) (recur (drop 4 inputs) (conj r [:not op4 op2 "NIL"]))
                (= "AND"    op2) (recur (drop 5 inputs) (conj r [:and op5 op1 op3]))
                (= "OR"     op2) (recur (drop 5 inputs) (conj r [:or  op5 op1 op3]))
                (= "RSHIFT" op2) (recur (drop 5 inputs) (conj r [:rsh op5 op1 op3]))
                (= "LSHIFT" op2) (recur (drop 5 inputs) (conj r [:lsh op5 op1 op3]))
                :else (if (re-seq #"\d+" op1)
                        (recur (drop 3 inputs) (conj r [op3 (edn/read-string op1)])) ;; 4 -> a
                        (recur (drop 3 inputs) (conj r [:eq op3 op1 "NIL"])))))      ;; a -> b
            r))
      f #(loop [r %
                s {"NIL" 1}]
           (or (s "a")
               (let [[op0 op1 op2 op3 :as item] (first r)]
                 (if (keyword? op0)
                   (let [op2 (or (s op2) (and (re-seq #"\d+" op2) (edn/read-string op2)))
                         op3 (or (s op3) (and (re-seq #"\d+" op3) (edn/read-string op3)))]
                     (if (and op2 op3)
                       (recur (rest r)
                              (condp = op0
                                :not (assoc s op1 (- 65535 op2))
                                :and (assoc s op1 (bit-and op2 op3))
                                :or  (assoc s op1 (bit-or  op2 op3))
                                :rsh (assoc s op1 (bit-shift-right op2 op3))
                                :lsh (assoc s op1 (bit-and 65535 (bit-shift-left op2 op3)))
                                :eq  (assoc s op1 op2)))
                       (recur (concat (rest r) [item]) s)))
                   (recur (rest r) (assoc s op0 op1))))))
      q1 (f r)
      q2 (f (-> (remove #(= "b" (first %)) r) (conj ["b" q1])))]
  [q1 q2])
;; [3176 14710]


;; 201508
(let [inputs (->> (slurp "src/y2015/input201508") (re-seq #".+"))
      f (fn [s]
          (let [d1 (re-seq #"\\\\" s)
                s' (str/replace s #"\\\\" "")
                d2 (re-seq #"\\\"" s')
                d3 (re-seq #"\\x.." s')]
            (reduce + 2 (->> [d1 d2 d3] (map count) (map * [1 1 3])))))
      g (fn [s]
          (let [d1 (re-seq #"\"" s)
                d2 (re-seq #"\\" s)]
            (+ 2 (count d1) (count d2))))]
  (map #(reduce + (map % inputs)) [f g]))
;; (1371 2117)


;; 201509
(let [inputs (->> (slurp "src/y2015/input201509") (re-seq #"\S+") (partition 5))
      [vertices edges] (reduce (fn [[vertices edges] [a _ b _ c]]
                                 [(conj vertices a b) (conj edges [#{a b} (edn/read-string c)])])
                               [#{} {}]
                               inputs)
      perm (permutations vertices)
      distances (map #(reduce + (map (fn [x] (edges (set x))) (partition 2 1 %))) perm)]
  (map #(reduce % distances) [min max]))
;; (117 909)


;; 201510
(let [input (->> (slurp "src/y2015/input201510") (re-seq #"\d"))
      f (fn [xs] (->> (partition-by identity xs) (mapcat #(list (count %) (first %))) (apply str)))
      xs (->> (iterate f input) (drop 40))]
  [(count (first xs))
   (count (nth xs 10))])
;; [492982 6989950]


;; 201511
(let [input (->> (slurp "src/y2015/input201511")
                 (re-seq #"\w")
                 (map (comp #(- % 97) int first))
                 (reduce #(+ (* 26 %1) %2)))
      crit1 (set (partition 3 1 (range 0 26)))
      crit2 (set (map (comp #(- % 97) int) [\i \o \l]))
      crit3 (fn [coll] (->> (partition 2 1 coll) (filter #(= 1 (count (set %)))) set count (< 1)))
      f (fn [n] (loop [n n, r ()] (if (< n 26) (conj r n) (recur (quot n 26) (conj r (mod n 26))))))
      g (fn [n]
          (let [coll (f n)]
            (and (not-any? crit2 coll)
                 (some crit1 (partition 3 1 coll))
                 (crit3 coll))))
      h (fn [coll] (->> coll (map (comp char #(+ 97 %))) (apply str)))]
  (map (comp h f) (take 2 (filter g (iterate inc (inc input))))))
;; ("hxbxxyzz" "hxcaabcc")


;; 201512
(let [input (slurp "src/y2015/input201512")
      omit (fn [q target]
             (loop [[x & more] q
                    stack 0]
               (if (and (= x target) (= 0 stack))
                 more
                 (cond (#{"[" "{"} x) (recur more (inc stack))
                       (#{"]" "}"} x) (recur more (dec stack))
                       :else (recur more stack)))))
      ans1 (reduce + (map edn/read-string (re-seq #"-?\d+" input)))
      ans2 (loop [[x & tail] (re-seq #"[{}\[\]]|-?\d+|:\"red" input)
                  cache ()
                  brackets ()]
             (cond (= x "{")      (recur tail (conj cache x) (conj brackets :brace))
                   (= x "[")      (recur tail (conj cache x) (conj brackets :bracket))
                   (#{"]" "}"} x) (recur tail (conj cache x) (pop brackets))
                   (= x ":\"red") (if (= (peek brackets) :brace)
                                    (recur (omit tail "}") (omit cache "{") (pop brackets))
                                    (recur tail x brackets))
                   (= x nil)      (reduce + (filter number? cache))
                   :else          (recur tail (conj cache (edn/read-string x)) brackets)))]
  [ans1 ans2])
;; [156366 96852]


;; 201513
(let [input (->> (slurp "src/y2015/input201513") (re-seq #"(\w+) would (\w+) (\d+) .*? next to (\w+)"))
      gl-map {"gain" 1, "lose" -1}
      f (fn [[v e] [_ name1 action pts name2]]
          [(conj v name1 name2) (assoc e [name1 name2] (* (edn/read-string pts) (gl-map action)))])
      [vertices edges] (reduce f [#{} {}] input)
      ;; turn 1-2-3 into 1-2-3-1, then into 1-2/2-3/3-1 and then get the happiness value between two seats
      g (fn [x] (map #(+ (edges %) (edges (reverse %))) (partition 2 1 (concat x [(first x)]))))
      ;; list of happiness values of all possible permutations
      all-list (map g (permutations vertices))]
  [(reduce max (map #(reduce + %) all-list))
   (reduce max (map #(- (reduce + %) (reduce min %)) all-list))])
;; [709 668]


;; 201514
(let [input (->> (slurp "src/y2015/input201514") (re-seq #"\d+") (map edn/read-string) (partition 3))
      f (fn [[speed fly-time rest-time]]
          (let [total-time (+ fly-time rest-time)
                cycles (quot 2503 total-time)
                odd-slot (mod 2503 total-time)]
            (* speed (+ (* cycles fly-time) (min odd-slot fly-time)))))
      g (fn [[speed fly-time rest-time]]
          (let [xs (cycle (concat (repeat fly-time speed) (repeat rest-time 0)))]
            (reductions + (first xs) (rest xs))))
      h (fn [& xs]
          (let [top (reduce max xs)]
            (map #(if (= top %) 1 0) xs)))]
  [(reduce max (map f input))
   (reduce max (apply map + (take 2503 (apply map h (map g input)))))])
;; [2660 1256]


;; 201515
(let [input (->> (slurp "src/y2015/input201515") (re-seq #"-?\d+") (map edn/read-string) (partition 5))
      q1 (for [a (range 101)
               b (range (- 101 a))
               c (range (- 101 a b))
               d (range (- 101 a b c))
               :when (= 100 (+ a b c d))    ]
           (->> (apply map #(reduce + (map * [a b c d] %&)) input)
                (take 4)
                (map #(if (> 0 %) 0 %))
                (reduce *)))
      q2 (->> (for [a (range 101)
                    b (range (- 101 a))
                    c (range (- 101 a b))
                    d (range (- 101 a b c))
                    :when (= 100 (+ a b c d))    ]
                (->> (apply map #(reduce + (map * [a b c d] %&)) input)
                     (map #(if (> 0 %) 0 %))))
              (filter #(= 500 (nth % 4)))
              (map #(apply * (take 4 %))))]
  [(reduce max q1) (reduce max q2)])
;; [222870 117936]


;; 201516
(let [target {"children"    3
              "cats"        7
              "samoyeds"    2
              "pomeranians" 3
              "akitas"      0
              "vizslas"     0
              "goldfish"    5
              "trees"       3
              "cars"        2
              "perfumes"    1}
      d (fn [[_ n itm1 val1 itm2 val2 itm3 val3]]
          (let [[val1 val2 val3] (map edn/read-string [val1 val2 val3])]
            [n itm1 val1 itm2 val2 itm3 val3]))
      e (fn [x]
          (cond (#{"cats" "trees"} x)           >
                (#{"pomeranians" "goldfish"} x) <
                :else                           =))
      f (fn [[n itm1 val1 itm2 val2 itm3 val3]]
          (and (= val1 (target itm1))
               (= val2 (target itm2))
               (= val3 (target itm3))))
      g (fn [[n itm1 val1 itm2 val2 itm3 val3]]
          (and ((e itm1) val1 (target itm1))
               ((e itm2) val2 (target itm2))
               ((e itm3) val3 (target itm3))))
      input (->> (slurp "src/y2015/input201516") (re-seq #"\w+|\d+") (partition 8) (map d))]
  (map #(ffirst (filter % input)) [f g]))
;; ("213" "323")


;; 201517
(let [input (->> (slurp "src/y2015/input201517") (re-seq #"\d+") (map edn/read-string) sort)
      f (fn f [n cnt [head & tail]]
          (lazy-seq
           (cond (nil? head) ()
                 (< n head)  ()
                 (= n head)  (cons cnt (f n cnt tail))
                 :else       (concat (f n cnt tail) (f (- n head) (inc cnt) tail)))))
      ans1 (f 150 1 input)
      temp (frequencies ans1)
      ans2 (temp (reduce min (keys temp)))]
  [(count ans1) ans2])
;; [1304 18]


;; 201518
(let [grid (->> (slurp "src/y2015/input201518") (re-seq #"[#.]") vec)
      neighbors (fn [[x y]]
                  (for [a [-1 0 1]
                        b [-1 0 1]
                        :let [newx (+ x a)
                              newy (+ y b)]
                        :when (and (not= a b 0)
                                   (<= 0 newx 99)
                                   (<= 0 newy 99))]
                    [newx newy]))
      lights-on (set (for [x (range 100)
                           y (range 100)
                           :when (= "#" (grid (+ (* x 100) y)))]
                       [x y]))
      f (fn [coll]
          (set (for [[loc n] (frequencies (mapcat neighbors coll))
                     :when (or (= 3 n) (and (= 2 n) (coll loc)))]
                 loc)))
      g (fn [coll] (into (f coll) [[0 0] [0 99] [99 0] [99 99]]))]
  (map #(count (nth (iterate % lights-on) 100)) [f g]))
;; (1061 1006)


;; 201519
(let [input (->> (slurp "src/y2015/input201519") (re-seq #"\w+"))
      molecule (last input)
      replacements (partition 2 input)
      f (fn [[s d]]
          (let [l (count s)
                positions (->> (iterate #(str/index-of molecule s (inc %)) -1) rest (take-while nat-int?))]
            (map #(str (subs molecule 0 %) d (subs molecule (+ % l))) positions)))]
  [(count (set (mapcat f replacements)))
   (- (count (re-seq #"[A-Z]" molecule))
      (* 2 (count (re-seq #"Rn" molecule)))
      (* 2 (count (re-seq #"Y" molecule)))
      1)])
;; [535 212]


;; 201520
(let [input (->> (slurp "src/y2015/input201520") edn/read-string)
      target1 (/ input 10)
      target2 (/ input 11)
      f (fn [[pos l]]
          [pos (reduce + (map second l))])]
  [(->> (for [i (range 1 (inc target1))
              :let [j (range 1 (inc (Math/sqrt i)))]]
          [i (reduce #(+ % (if (= 0 (mod i %2)) (+ %2 (if (= %2 (/ i %2)) 0 (/ i %2))) 0)) 0 j)])
        (filter #(>= (second %) target1))
        ffirst)
   (->> (for [i (range 1 (inc target2))
              :let [j (range 1 (min 51 (inc (Math/sqrt i))))]]
          [i (reduce #(+ % (if (= 0 (mod i %2)) (/ i %2) 0)) 0 j)])
        (filter #(>= (second %) target2))
        ffirst)])
;; [776160 786240]


;; 201521
;; Weapons:    Cost  Damage  Armor
;; Dagger        8     4       0
;; Shortsword   10     5       0
;; Warhammer    25     6       0
;; Longsword    40     7       0
;; Greataxe     74     8       0
;;
;; Armor:      Cost  Damage  Armor
;; Leather      13     0       1
;; Chainmail    31     0       2
;; Splintmail   53     0       3
;; Bandedmail   75     0       4
;; Platemail   102     0       5
;;
;; Rings:      Cost  Damage  Armor
;; Damage +1    25     1       0
;; Damage +2    50     2       0
;; Damage +3   100     3       0
;; Defense +1   20     0       1
;; Defense +2   40     0       2
;; Defense +3   80     0       3
(let [[b-hit-point b-damage b-armor] (->> (slurp "src/y2015/input201521") (re-seq #"\d+") (map edn/read-string))
      ;; [number-of-ring-used damage/armor-value cost-of-weapon/armor]
      ;; [0 0 0] when item is optional like armon/ring. weapon is not
      damage [[[0 4 8] [0 5 10] [0 6 25] [0 7 40]  [0 8 74]]
              [[0 0 0] [1 1 25] [1 2 50] [1 3 100] [2 3 75] [2 4 125] [2 5 150]]]
      armor  [[[0 0 0] [0 1 13] [0 2 31] [0 3 53]  [0 4 75] [0 5 102]]
              [[0 0 0] [1 1 20] [1 2 40] [1 3 80]  [2 3 60] [2 4 100] [2 5 120]]]
      [damage armor] (map #(for [i (first %) j (second %)] (map + i j)) [damage armor])]
  (map (fn [[f1 f2]]
         (->> (for [[d-ring p-damage d-cost] damage
                    [a-ring p-armor  a-cost] armor
                    :when (and (<= (+ d-ring a-ring) 2)
                               (f1 (Math/ceil (/ b-hit-point (max (- p-damage b-armor) 1)))
                                   (Math/ceil (/ 100         (max (- b-damage p-armor) 1)))))]
                (+ d-cost a-cost))
              (reduce f2)))
       [[<= min] [> max]]))
;; (78 148)


;; 201522
(let [actions #{:missile :drain :shield :poison :recharge}
      magics {:missile  {:cost 53  :damage 4 :armor 0 :hp 0 :mana 0   :turns 0}
              :drain    {:cost 73  :damage 2 :armor 0 :hp 2 :mana 0   :turns 0}
              :shield   {:cost 113 :damage 0 :armor 7 :hp 0 :mana 0   :turns 6}
              :poison   {:cost 173 :damage 3 :armor 0 :hp 0 :mana 0   :turns 6}
              :recharge {:cost 229 :damage 0 :armor 0 :hp 0 :mana 101 :turns 5}}
      status {:magic {:missile 0, :drain 0, :shield 0, :poison 0, :recharge 0}
              :player {:hp 50, :armor 0, :mana 500, :mana-spent 0}
              :boss (zipmap [:hp :damage]
                            (->> (slurp "src/y2015/input201522") (re-seq #"\d+") (map edn/read-string)))}
      fn-existing-magic
      (fn [status]
        (reduce (fn [status [magic turns]]
                  (cond (> turns 0) (-> (cond
                                          (= magic :shield)   (-> status (assoc-in  [:player :armor] 7))
                                          (= magic :poison)   (-> status (update-in [:boss :hp]      #(- % 3)))
                                          (= magic :recharge) (-> status (update-in [:player :mana]  #(+ % 101))))
                                        (update-in [:magic magic] dec))
                        (= magic :shield) (assoc-in status [:player :armor] 0)
                        :else status))
                status
                (:magic status)))
      fn-player-turn
      (fn [status action]
        (-> (cond
              (= action :missile)  (update-in status [:boss :hp] #(- % 4))
              (= action :drain)    (-> status (update-in [:boss :hp] #(- % 2)) (update-in [:player :hp] #(+ % 2)))
              (= action :shield)   (assoc-in  status [:magic action] 6)
              (= action :poison)   (assoc-in  status [:magic action] 6)
              (= action :recharge) (assoc-in  status [:magic action] 5))
            (update-in [:player :mana-spent] #(+ % (get-in magics [action :cost])))
            (update-in [:player :mana]       #(- % (get-in magics [action :cost])))))
      fn-boss-turn
      (fn [status]
        (update-in status [:player :hp] #(- % (max 1 (- (get-in status [:boss :damage]) (get-in status [:player :armor]))))))
      f (fn [mode]
          (let [min-mana-spent (atom 999999999)
                g (fn g [action status]
                    (let [status (if (= :hard mode) (update-in status [:player :hp] dec) status)]
                      (cond
                        (<= (get-in status [:boss :hp]) 0) ;; boss's hp<=0 and player wins
                        (list (swap! min-mana-spent min (get-in status [:player :mana-spent])))

                        (or (<= @min-mana-spent (get-in status [:player :mana-spent])) ;; skip when it costs more mana than min found
                            (< 1 (get-in status [:magic action])) ;; magic remains in effect hence not valid
                            (<= (get-in status [:player :hp]) 0) ;; player's hp<=0 and player loses
                            (< (get-in status [:player :mana]) (get-in magics [action :cost]))) ;; player out of mana
                        nil

                        :else
                        (let [status (-> status
                                         fn-existing-magic
                                         (fn-player-turn action)
                                         fn-existing-magic
                                         fn-boss-turn)]
                          (lazy-seq (mapcat #(g % status) actions))))))]
            (dorun (mapcat #(g % status) actions))
            @min-mana-spent))]
  (map f [:easy :hard]))
;; (900 1216)


;; 201523
(let [code (->> (slurp "src/y2015/input201523") str/split-lines vec)
      l (count code)
      f (fn [r]
          (loop [pc 0, r r]
            (if (< pc l)
              (let [[s1 s2 s3] (re-seq #"[a-z0-9+-]+" (code pc))]
                (condp = s1
                  "hlf" (recur (inc pc) (assoc r s2 (bit-shift-right (r s2) 1)))
                  "tpl" (recur (inc pc) (assoc r s2 (* 3 (r s2))))
                  "inc" (recur (inc pc) (assoc r s2 (inc (r s2))))
                  "jmp" (recur (+ pc (edn/read-string s2)) r)
                  "jie" (recur (if (even? (r s2)) (+ pc (edn/read-string s3)) (inc pc)) r)
                  "jio" (recur (if (= 1 (r s2))   (+ pc (edn/read-string s3)) (inc pc)) r)
                  r))
              (r "b"))))]
  (map f [{"a" 0 "b" 0} {"a" 1 "b" 0}]))
;; (170 247)


;; 201524
(let [input (->> (slurp "src/y2015/input201524") (re-seq #"\d+") (map edn/read-string))
      find-groups (fn f
                    ([sum input] (f sum input ()))
                    ([sum [head & more] r]
                     (lazy-seq
                      (cond
                        (< sum 0) ()
                        head      (if (= sum head)
                                    (list (cons head r))
                                    (concat (f (- sum head) more (cons head r)) (f sum more r)))
                        :else     ()))))
      one-pass-filter
      (fn [packages-count goal]
        (fn [first-group]
          (and (= packages-count (count first-group))
               (seq (find-groups goal (remove (set first-group) input))))))
      two-passes-filter
      (fn [packages-count goal]
        (fn [first-group]
          (if (= packages-count (count first-group))
            (if-let [second-groups (seq (find-groups goal (remove (set first-group) input)))]
              (->> second-groups
                   (mapcat (fn [second-group]
                             (find-groups goal (remove (set (concat first-group second-group)) input))))
                   seq)))))
      g (fn [[n f]]
          (let [goal (/ (reduce + input) n)
                list-of-groups (find-groups goal input)
                [min-packages-count max-packages-count] ((juxt first last) (sort (map count list-of-groups)))]
            (->> (range min-packages-count (inc max-packages-count))
                 (map (fn [packages-count] (filter (f packages-count goal) list-of-groups)))
                 (some seq)
                 (map #(reduce * %))
                 (reduce min))))]
  (pmap g [[3 one-pass-filter] [4 two-passes-filter]]))
;;(11846773891 80393059)


;; 201525
(let [[x y] (->> (slurp "src/y2015/input201525") (re-seq #"\d+") (map edn/read-string))
      level (+ x y)
      last-in-level-n (/ (* level (dec level)) 2)
      n (+ (- last-in-level-n level) y)
      xseq (iterate #(mod (* % 252533) 33554393) 20151125)]
  (nth xseq n))
;; 9132360
