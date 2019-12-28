;;        1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
;; part 1 * * * * * * * *
;; part 2 * * * * * * * *

(ns y2015.core
  (:import [java.security MessageDigest]))

;; 200501
[(->> (slurp "src/y2015/input201501")
      (reduce #(if (= \( %2) (inc %1) (dec %1)) 0))
 (loop [input (slurp "src/y2015/input201501")
        s 0
        n 0]
   (if (= -1 s)
     n
     (recur (rest input) (if (= \( (first input)) (inc s) (dec s)) (inc n))))]
;; [232 1783]


;; 200502
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
       (map #(Integer/parseInt %))
       (partition 3)
       (map f)
       (apply map +)))
;; (1598415 3812909)


;; 200503
(let [dirs {\^ [0 -1]
            \v [0  1]
            \< [-1 0]
            \> [1  0]}
      input (slurp "src/y2015/input201503")]
  [(loop [input input
          seen #{[0 0]}
          pos [0 0]]
     (if (empty? input)
       (count seen)
       (let [new-pos (map + (dirs (first input)) pos)]
         (recur (rest input) (conj seen new-pos) new-pos))))
   (loop [input input
          seen #{[0 0]}
          pos1 [0 0]
          pos2 [0 0]]
     (if (empty? input)
       (count seen)
       (let [[new-pos1 new-pos2] (map #(map + (dirs %1) %2) (take 2 input) [pos1 pos2])]
         (recur (drop 2 input) (conj seen new-pos1 new-pos2) new-pos1 new-pos2))))])
;; [2565 2639]


;; 200504
(let [input (slurp "src/y2015/input201504")
      algorithm (MessageDigest/getInstance "MD5")
      five0 [\0 \0 \0 \0 \0]
      six0  [\0 \0 \0 \0 \0 \0]
      md5 (fn [^String s]
            (let [raw (.digest algorithm (.getBytes s))]
              (format "%032x" (BigInteger. 1 raw))))]
  [(->> (map #(if (= five0 (take 5 (md5 (str input %)))) %) (drop 1 (range)))
        (drop-while nil?)
        first)
   (->> (map #(if (= six0  (take 6 (md5 (str input %)))) %) (drop 1 (range)))
        (drop-while nil?)
        first)])
;; [254575 1038736]


;; 200505
(let [vowels #{\a \e \i \o \u}
      nice1? (fn [s]
               (and (<= 3 (reduce + (map #(if (vowels %) 1 0) s)))
                    (some #{1} (map #(count (set %)) (partition 2 1 s)))
                    (not (some #{[\a \b] [\c \d] [\p \q] [\x \y]} (partition 2 1 s)))))
      nice2? (fn [s]
               (and (re-seq #"(..).*\1" s)
                    (re-seq #"(.).\1" s)))
      input (re-seq #"[^\n]+" (slurp "src/y2015/input201505"))]
  [(->> input
        (map #(if (nice1? %) 1 0))
        (reduce +))
   (->> input
        (map #(if (nice2? %) 1 0))
        (reduce +))])
;; [258 53]


;; 200506
(let [f (fn [[action & nums]]
          (into [action] (map #(Integer/parseInt %) nums)))
      input (map f (partition 5 (re-seq #"toggle|on|off|\d+" (slurp "src/y2015/input201506"))))
      q1 (let [arr (make-array Integer/TYPE 1000000)]
           (dorun (for [[action a b c d] input
                        x (range a (inc c))
                        y (range b (inc d))
                        :let [idx (+ (* 1000 x) y)]]
                    (cond (= action "on")  (aset ^"[I" arr idx 1)
                          (= action "off") (aset ^"[I" arr idx 0)
                          :else            (aset ^"[I" arr idx (- 1 (aget ^"[I" arr idx))))))
           (reduce + (map #(aget ^"[I" arr %) (range 1000000))))
      q2 (let [arr (make-array Integer/TYPE 1000000)]
           (dorun (for [[action a b c d] input
                        x (range a (inc c))
                        y (range b (inc d))
                        :let [idx (+ (* 1000 x) y)]]
                    (cond (= action "on")  (aset ^"[I" arr idx (+ 1 (aget ^"[I" arr idx)))
                          (= action "off") (aset ^"[I" arr idx (if (= 0 (aget ^"[I" arr idx))
                                                                 0
                                                                 (- (aget ^"[I" arr idx) 1)))
                          :else            (aset ^"[I" arr idx (+ 2 (aget ^"[I" arr idx))))))
           (reduce + (map #(aget ^"[I" arr %) (range 1000000))))]
  [q1 q2])
;; [543903 14687245]


;; 200507
(let [r1 (loop [inputs (re-seq #"\S+" (slurp "src/y2015/input201507"))
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
                         (recur (drop 3 inputs) (conj r [op3 (Integer/parseInt op1)]))
                         (recur (drop 3 inputs) (conj r [:eq op3 op1 "NIL"])))))
             r))
      f #(loop [r %
                s {"NIL" 1}]
           (let [[op0 op1 op2 op3 :as item] (first r)]
             (or (s "a")
                 (if (keyword? op0)
                   (if (and (or (s op2) (re-seq #"\d+" op2))
                            (or (s op3) (re-seq #"\d+" op3)))
                     (condp = op0
                       :not (recur (rest r) (assoc s op1 (- 65535 (s op2))))
                       :and (recur (rest r) (assoc s op1 (bit-and (or (s op2) (Integer/parseInt op2))
                                                                  (or (s op3) (Integer/parseInt op3)))))
                       :or  (recur (rest r) (assoc s op1 (bit-or  (or (s op2) (Integer/parseInt op2))
                                                                  (or (s op3) (Integer/parseInt op3)))))
                       :rsh (recur (rest r) (assoc s op1 (bit-shift-right (s op2) (Integer/parseInt op3))))
                       :lsh (recur (rest r) (assoc s op1 (bit-and 65535
                                                                  (bit-shift-left (s op2) (Integer/parseInt op3)))))
                       :eq  (recur (rest r) (assoc s op1 (s op2))))
                     (recur (concat (rest r) [item]) s))
                   (recur (rest r) (assoc s op0 op1))))))
      q1 (f r1)
      r2 (-> (remove #(= "b" (first %)) r1) (conj ["b" q1]))
      q2 (f r2)] 
  [q1 q2])
;; [3176 14710]


;; 201508
(let [inputs (re-seq #"[^\n]+" (slurp "src/y2015/input201508"))
      f (fn [s]
          (let [d1 (re-seq #"\\\\" s)
                s' (clojure.string/replace s #"\\\\" "")
                d2 (re-seq #"\\\"" s')
                d3 (re-seq #"\\x.." s')]
            (reduce + 2 (map * (map count [d1 d2 d3]) [1 1 3]))))
      g (fn [s]
          (let [d1 (re-seq #"\"" s)
                d2 (re-seq #"\\" s)]
            (+ 2 (count d1) (count d2))))]
  (map #(reduce + (map % inputs)) [f g]))
;; (1371 2117)
