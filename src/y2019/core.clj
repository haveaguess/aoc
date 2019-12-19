(ns y2019.core)

;; 201901
[(->> (re-seq #"\d+" (slurp "src/y2019/input201901"))
      (map #(Integer/parseInt %))
      (map #(/ (- % (mod % 3) 6) 3))
      (reduce +))
 (->> (re-seq #"\d+" (slurp "src/y2019/input201901"))
      (map #(Integer/parseInt %))
      (map #(iterate (fn [x] (/ (- x (mod x 3) 6) 3)) %))
      (map #(drop 1 %))
      (map #(take-while pos? %))
      flatten
      (reduce +))]
;; [3216868 4822435]


;; 201902
(let [f (fn [start end]
          (loop [pc 0
                 program (assoc (->> (re-seq #"\d+" (slurp "src/y2019/input201902"))
                                     (mapv #(Integer/parseInt %)))
                                1 start 2 end)]
            (let [[op op1 op2 op3] (drop pc program)]
              (cond
                (= op 1)
                (recur (+ pc 4) (assoc program op3 (+ (nth program op1) (nth program op2))))
                (= op 2)
                (recur (+ pc 4) (assoc program op3 (* (nth program op1) (nth program op2))))
                (= op 99)
                [(first program) (+ (* start 100) end)]))))]
  [(first (f 12 2))
   (->> (for [start (range 100)
              end   (range 100)]
          (f start end))
        (filter #(= 19690720 (first %)))
        first
        second)])
;; [3716293 6429]


;; 201903
(let [f (fn [{[x y] :o r :r d :d} [direction & distance]]
          (let [distance (Integer/parseInt (apply str distance))]
            (assoc (cond
                     (= direction \U)
                     {:o [x (+ y distance)]
                      :r (clojure.set/union
                          (set (for [n (range distance)] (with-meta [x (+ y n 1)] {:d (+ d n 1)})))
                          r)}
                     (= direction \D)
                     {:o [x (- y distance)]
                      :r (clojure.set/union
                          (set (for [n (range distance)] (with-meta [x (- y n 1)] {:d (+ d n 1)})))
                          r)}
                     (= direction \R)
                     {:o [(+ x distance) y]
                      :r (clojure.set/union
                          (set (for [n (range distance)] (with-meta [(+ x n 1) y] {:d (+ d n 1)})))
                          r)}
                     (= direction \L)
                     {:o [(- x distance) y]
                      :r (clojure.set/union
                          (set (for [n (range distance)] (with-meta [(- x n 1) y] {:d (+ d n 1)})))
                          r)}) :d (+ d distance))))
      [line1 line2] (clojure.string/split (slurp "src/y2019/input201903") #"\n")
      l1 (:r (reduce f {:d 0 :o [0 0] :r #{}} (re-seq #"[^,]+" line1)))
      l2 (:r (reduce f {:d 0 :o [0 0] :r #{}} (re-seq #"[^,]+" line2)))
      marks (clojure.set/intersection l1 l2)]
  [(reduce min (map #(+ (Math/abs (first %)) (Math/abs (second %))) marks))
   (reduce min (map #(+ (:d (meta (l1 %))) (:d (meta (l2 %)))) marks))])
;; [245 48262]


;; 201904
(let [[start end] (->> (re-seq #"\d+" (slurp "src/y2019/input201904"))
                       (mapv #(Integer/parseInt %)))
      f (fn [s]
          (and
           (apply <= s)
           (some #(= (first %) (second %)) (partition 2 1 s))))
      g (fn [s]
          (and
           (apply <= s)
           (some #(and (= (first %) (second %)) (= 2 (count %))) (partition-by identity s))))]
  (map #(->> (for [n (range start (inc end))]
               (% (map int (seq (str n)))))
             (filter true?)
             (count)) [f g]))
;; (511 316)


;; 201905
(let [intcode-computer-5
      (fn [file]
        (fn [input]
          (loop [input input
                 output []
                 pc 0
                 program (->> (re-seq #"-?\d+" (slurp file))
                              (mapv #(Integer/parseInt %)))]
            (let [[op op1 op2 op3] (drop pc program)
                  eop (mod op 100)
                  [op1mode op2mode] (map #(mod (int (/ op %)) 10) [100 1000])
                  op1 (if (or (#{3   99} eop) (= 1 op1mode)) op1 (nth program op1))
                  op2 (if (or (#{3 4 99} eop) (= 1 op2mode)) op2 (nth program op2))]
              (cond
                (= eop 1) (recur input output (+ pc 4) (assoc program op3 (+ op1 op2)))
                (= eop 2) (recur input output (+ pc 4) (assoc program op3 (* op1 op2)))      
                (= eop 3) (recur (drop 1 input) output (+ pc 2) (assoc program op1 (first input)))
                (= eop 4) (recur input (conj output op1) (+ pc 2) program)
                (= eop 5) (recur input output (if (not= 0 op1) op2 (+ pc 3)) program)
                (= eop 6) (recur input output (if (zero? op1)  op2 (+ pc 3)) program)
                (= eop 7) (recur input output (+ pc 4) (assoc program op3 (if (< op1 op2) 1 0)))
                (= eop 8) (recur input output (+ pc 4) (assoc program op3 (if (= op1 op2) 1 0)))
                (= eop 99) (last output))))))
      f (intcode-computer-5 "src/y2019/input201905")]
  [(f [1]) (f [5])])
;; [12234644 3508186]


;; 201906
(let [lines (->> (re-seq #"[^\n)]+" (slurp "src/y2019/input201906"))
                 (partition 2))
      youpath (loop [r '("YOU")]
                (let [temp (ffirst (filter #(= (second %) (first r)) lines))]
                  (if (= temp "COM")
                    (conj r "COM")
                    (recur (conj r temp)))))
      youset (set youpath)]
  [(->> (loop [r {"COM" 0}
               lines lines]
          (let [[v1 v2] ((juxt filter remove) #(r (first %)) lines)]
            (if (empty? v1)
              r
              (recur (apply merge r (map (fn [x] {(second x) (inc (r (first x)))}) v1)) v2))))
        (map val)
        (reduce +))
   (loop [r '("SAN")]
     (let [temp (ffirst (filter #(= (second %) (first r)) lines))]
       (if (youset temp)
         (+ (count r) (count (drop-while #(not= temp %) youpath)) -3)
         (recur (conj r temp)))))])
;; [194721 316]


;; 201907
(let [intcode-computer-7
      (fn [file]
        (fn [input]
          (loop [input input
                 output []
                 pc 0
                 program (->> (re-seq #"-?\d+" (slurp file))
                              (mapv #(Integer/parseInt %)))]
            (let [[op op1 op2 op3] (drop pc program)
                  eop (mod op 100)
                  [op1mode op2mode] (map #(mod (int (/ op %)) 10) [100 1000])
                  op1 (if (or (#{3   99} eop) (= 1 op1mode)) op1 (nth program op1))
                  op2 (if (or (#{3 4 99} eop) (= 1 op2mode)) op2 (nth program op2))]
              (cond
                (= eop 1) (recur input output (+ pc 4) (assoc program op3 (+ op1 op2)))
                (= eop 2) (recur input output (+ pc 4) (assoc program op3 (* op1 op2)))      
                (= eop 3) (recur (drop 1 input) output (+ pc 2) (assoc program op1 (first input)))
                (= eop 4) (recur input (conj output op1) (+ pc 2) program)
                (= eop 5) (recur input output (if (not= 0 op1) op2 (+ pc 3)) program)
                (= eop 6) (recur input output (if (zero? op1)  op2 (+ pc 3)) program)
                (= eop 7) (recur input output (+ pc 4) (assoc program op3 (if (< op1 op2) 1 0)))
                (= eop 8) (recur input output (+ pc 4) (assoc program op3 (if (= op1 op2) 1 0)))
                (= eop 99) (last output))))))
      intcode-computer-7-feedback-loop-mode
      (fn [file]
        (fn [input program]
          (loop [input input
                 pc (:pc program 0)
                 program (if (:program program)
                           (:program program)
                           (->> (re-seq #"-?\d+" (slurp file))
                                (mapv #(Integer/parseInt %))))]
            (let [[op op1 op2 op3] (drop pc program)
                  eop (mod op 100)
                  [op1mode op2mode] (map #(mod (int (/ op %)) 10) [100 1000])
                  op1 (if (or (#{3   99} eop) (= 1 op1mode)) op1 (nth program op1))
                  op2 (if (or (#{3 4 99} eop) (= 1 op2mode)) op2 (nth program op2))]
              (cond
                (= eop 1) (recur input (+ pc 4) (assoc program op3 (+ op1 op2)))
                (= eop 2) (recur input (+ pc 4) (assoc program op3 (* op1 op2)))      
                (= eop 3) (recur (drop 1 input) (+ pc 2) (assoc program op1 (first input)))
                (= eop 4) {:output op1 :program program :pc (+ pc 2)}
                (= eop 5) (recur input (if (not= 0 op1) op2 (+ pc 3)) program)
                (= eop 6) (recur input (if (zero? op1)  op2 (+ pc 3)) program)
                (= eop 7) (recur input (+ pc 4) (assoc program op3 (if (< op1 op2) 1 0)))
                (= eop 8) (recur input (+ pc 4) (assoc program op3 (if (= op1 op2) 1 0)))
                (= eop 99) {:output nil :program program :pc nil})))))
      f (intcode-computer-7 "src/y2019/input201907")
      g (intcode-computer-7-feedback-loop-mode "src/y2019/input201907")]
  [(->> (for [a (range 5)
              b (range 5)
              c (range 5)
              d (range 5)
              e (range 5)
              :when (= 5 (count (hash-set a b c d e)))]
          (loop [phases [a b c d e]
                 signal 0]
            (if (seq phases)
              (recur (rest phases) (f [(first phases) signal]))
              signal)))
        (reduce max))
   (->> (for [a (range 5 10)
              b (range 5 10)
              c (range 5 10)
              d (range 5 10)
              e (range 5 10)
              :when (= 5 (count (hash-set a b c d e)))
              :let [phases [a b c d e]
                    amps (repeat 5 nil)]]
          (->> (iterate
                (fn [x]
                  (if (nil? x)
                    nil
                    (let [{:keys [signal amps cnt]} x
                          amp (first amps)
                          {:keys [output program pc]} (if (> 5 cnt)
                                                        (g [(nth phases cnt) signal] amp)
                                                        (g [signal] amp))]
                      (if (nil? output)
                        nil
                        {:signal output
                         :amps (concat (rest amps) [{:program program :pc pc}])
                         :cnt (inc cnt)}))))
                {:signal 0 :amps amps :cnt 0})
               (take-nth 5)
               (take-while identity)
               (map :signal)
               last))
        (reduce max))])
;; [24405 8271623]


;; 201908
[(->> (re-seq #"\d" (slurp "src/y2019/input201908"))
      (map #(Integer/parseInt %))
      (partition (* 25 6))
      (map (fn [x] [(count (filter zero? x)) x]))
      (sort-by first)
      first
      second
      (filter #{1 2})
      frequencies
      (map val)
      (reduce *))
 (->> (re-seq #"\d" (slurp "src/y2019/input201908"))
      (partition (* 25 6))
      (apply map list)
      (map #(first (drop-while #{"2"} %)))
      (map #(if (= "1" %) "▓" "░"))
      (partition 25)
      (map #(str (apply str %) "\n"))
      (apply str)
      println)]
;; [2440 nil]
;; 0110011110011000011001100
;; 1001000010100100001010010
;; 1001000100100000001010000
;; 1111001000100000001010000
;; 1001010000100101001010010
;; 1001011110011000110001100
;; OR "AZCJC"
;; ░▓▓░░▓▓▓▓░░▓▓░░░░▓▓░░▓▓░░
;; ▓░░▓░░░░▓░▓░░▓░░░░▓░▓░░▓░
;; ▓░░▓░░░▓░░▓░░░░░░░▓░▓░░░░
;; ▓▓▓▓░░▓░░░▓░░░░░░░▓░▓░░░░
;; ▓░░▓░▓░░░░▓░░▓░▓░░▓░▓░░▓░
;; ▓░░▓░▓▓▓▓░░▓▓░░░▓▓░░░▓▓░░


;; 201909
(let [intcode-computer-9
      (fn [file]
        (fn [input]
          (loop [state {:input input :output [] :pc 0 :base 0}
                 program (->> (re-seq #"-?\d+" (slurp file))
                              (map #(BigInteger. %))
                              (map-indexed (fn [idx itm] [idx itm]))
                              (into (sorted-map)))]
            (let [base (:base state)
                  pc (:pc state)
                  [op op1 op2 op3] (map #(program % 0) (range pc (+ pc 4)))
                  eop (mod op 100)
                  [op1mode op2mode op3mode] (map #(mod (int (/ op %)) 10) [100 1000 10000])
                  op1 (cond
                        (and (= 2 op1mode) (= 3 eop))                (+ base op1)
                        (and (= 0 op1mode) (#{1 2 4 5 6 7 8 9} eop)) (program op1 0)
                        (and (= 2 op1mode) (#{1 2 4 5 6 7 8 9} eop)) (program (+ base op1) 0)
                        :else                                        op1)
                  op2 (cond
                        (and (= 0 op2mode) (#{1 2 5 6 7 8} eop))     (program op2 0)
                        (and (= 2 op2mode) (#{1 2 5 6 7 8} eop))     (program (+ base op2) 0)
                        :else                                        op2)
                  op3 (cond                                          
                        (and (#{1 2 7 8} eop) (= 2 op3mode))         (+ base op3)
                        :else                                        op3)]
              (cond
                (= eop 1) (recur (update state :pc #(+ % 4))
                                 (assoc program op3 (+ op1 op2)))
                (= eop 2) (recur (update state :pc #(+ % 4))
                                 (assoc program op3 (* op1 op2)))      
                (= eop 3) (recur (-> (update state :pc #(+ % 2)) (update :input #(drop 1 %)))
                                 (assoc program op1 (first (:input state))))
                (= eop 4) (recur (-> (update state :output #(conj % op1)) (update :pc #(+ % 2)))
                                 program)
                (= eop 5) (recur (if (not= 0 op1) (assoc state :pc op2) (update state :pc #(+ % 3)))
                                 program)
                (= eop 6) (recur (if (zero? op1)  (assoc state :pc op2) (update state :pc #(+ % 3)))
                                 program)
                (= eop 7) (recur (update state :pc #(+ % 4))
                                 (assoc program op3 (if (< op1 op2) 1 0)))
                (= eop 8) (recur (update state :pc #(+ % 4))
                                 (assoc program op3 (if (= op1 op2) 1 0)))
                (= eop 9) (recur (-> (update state :base #(+ % op1)) (update :pc #(+ % 2)))
                                 program)
                (= eop 99) (:output state))))))
      f (intcode-computer-9 "src/y2019/input201909")]
  [(last (f [1])) (last (f [2]))])
;; [[2789104029N] [32869N]]


;; 201910
(let [lines (re-seq #"[^\n]+" (slurp "src/y2019/input201910"))
      cords (mapcat
             #(map vector (remove nil? (map-indexed (fn [idx itm] (if (= itm \#) idx)) %1)) (repeat %2))
             lines (range))
      f (fn [[basex basey :as base]]
          (fn [m]
            {:base base
             :detect (reduce + (cond
                                 (empty? (:vertical m))                                       0
                                 (and (some #(< (second %) basey) (map first (:vertical m)))
                                      (some #(> (second %) basey) (map first (:vertical m)))) 2
                                 :else                                                        1)
                             (map
                              (fn [x]
                                (if (and (some #(< (first %) basex) (map first x))
                                         (some #(> (first %) basex) (map first x))) 2 1))
                              (vals (dissoc m :vertical))))}))
      r (reduce
         #(if (> (:detect %1) (:detect %2)) %1 %2)
         (for [[x0 y0 :as a] cords]
           (->> (for [[x1 y1 :as b] cords
                      :when (not= a b)]
                  (if (= x0 x1) [b :vertical] [b (/ (- y1 y0) (- x1 x0))]))
                (group-by second)
                ((f a)))))
      g (fn [[basex basey]]
          (fn [m]
            (let [vert-points (->> (:vertical m)
                                   (map first)
                                   (sort-by second)
                                   (split-with #(> basey (second %))))
                  other-points (->> (vals (into (sorted-map) (dissoc m :vertical)))
                                    (map (fn [x]
                                           (->> (map first x)
                                                (sort-by first)
                                                (split-with #(> basex (first %)))))))]
              (concat [(reverse (first vert-points))] (map second other-points)
                      [(second vert-points)] (map (comp reverse first) other-points)))))]
  [(:detect r)
   (let [[x0 y0 :as base] (:base r)
         lines (->> (for [[x1 y1 :as b] cords
                          :when (not= base b)]
                      (if (= x0 x1) [b :vertical] [b (/ (- y1 y0) (- x1 x0))]))
                    (group-by second)
                    ((g base)))]
     (->> (loop [r ()
                 lines (remove empty? lines)]
            (if (empty? lines)
              r
              (recur (concat r (map first lines)) (remove empty? (map rest lines)))))
          (drop 199)
          first
          (#(+ (* (first %) 100) (second %)))))])
;; [288 616]


;; 201911
(let [dirs {0   [0 1]
            180 [0 -1]
            90  [1 0]
            270 [-1 0]}
      intcode-computer-11
      (fn [file]
        (fn [input color]
          (loop [state {:input input :output [] :pc 0 :base 0 :pos [0 0] :dir 0}
                 program (->> (re-seq #"-?\d+" (slurp file))
                              (map #(BigInteger. %))
                              (map-indexed (fn [idx itm] [idx itm]))
                              (into (sorted-map)))]
            (let [base   (state :base)
                  pc     (state :pc)
                  output (state :output)
                  [op op1 op2 op3] (map #(program % 0) (range pc (+ pc 4)))
                  eop (mod op 100)
                  [op1mode op2mode op3mode] (map #(mod (int (/ op %)) 10) [100 1000 10000])
                  op1 (cond
                        (and (= 2 op1mode) (= 3 eop))                (+ base op1)
                        (and (= 0 op1mode) (#{1 2 4 5 6 7 8 9} eop)) (program op1 0)
                        (and (= 2 op1mode) (#{1 2 4 5 6 7 8 9} eop)) (program (+ base op1) 0)
                        :else                                        op1)
                  op2 (cond
                        (and (= 0 op2mode) (#{1 2 5 6 7 8} eop))     (program op2 0)
                        (and (= 2 op2mode) (#{1 2 5 6 7 8} eop))     (program (+ base op2) 0)
                        :else                                        op2)
                  op3 (cond                                          
                        (and (#{1 2 7 8} eop) (= 2 op3mode))         (+ base op3)
                        :else                                        op3)]
              (cond
                (= eop 1) (recur (update state :pc #(+ % 4))
                                 (assoc program op3 (+ op1 op2)))
                (= eop 2) (recur (update state :pc #(+ % 4))
                                 (assoc program op3 (* op1 op2)))      
                (= eop 3) (recur (update state :pc #(+ % 2))
                                 (assoc program op1 (get (:input state) (:pos state) color)))
                (= eop 4) (recur (let [state1 (-> (update state :pc #(+ % 2))
                                                  (update :output #(conj % op1))
                                                  (update :input (if (even? (count output))
                                                                   #(assoc % (:pos state) op1)
                                                                   identity)))
                                       dir (if (odd? (count output))
                                             (cond (= 0 op1) (mod (+ (:dir state) 270) 360)
                                                   (= 1 op1) (mod (+ (:dir state) 90)  360))
                                             (:dir state))
                                       pos (if (odd? (count output))
                                             (map + (dirs dir) (:pos state))
                                             (:pos state))]
                                   (assoc state1 :dir dir :pos pos))
                                 program)
                (= eop 5) (recur (if (not= 0 op1) (assoc state :pc op2) (update state :pc #(+ % 3)))
                                 program)
                (= eop 6) (recur (if (zero? op1)  (assoc state :pc op2) (update state :pc #(+ % 3)))
                                 program)
                (= eop 7) (recur (update state :pc #(+ % 4))
                                 (assoc program op3 (if (< op1 op2) 1 0)))
                (= eop 8) (recur (update state :pc #(+ % 4))
                                 (assoc program op3 (if (= op1 op2) 1 0)))
                (= eop 9) (recur (-> (update state :base #(+ % op1)) (update :pc #(+ % 2)))
                                 program)
                (= eop 99) state)))))
      f (intcode-computer-11 "src/y2019/input201911")
      g (fn [coll]
          [(reduce min coll) (reduce max coll)])
      r0 (:input (f {} 0))
      r1 (:input (f {} 1))
      [[x0 x1] [y0 y1]] [(g (map first (keys r1))) (g (map second (keys r1)))]
      [x y] [(- x1 x0) (- y1 y0)]
      r2 (vec (repeat (inc y) (vec (repeat (inc x) " "))))
      _ (dorun (map println (map #(apply str %)
                                 (reduce #(let [[[a b] v] %2]
                                            (assoc-in %1 [(- y (- b y0)) (- a x0) ] (if (pos? v) "▓" "░")))
                                         r2 r1))))]
  [(count r0)])
;; [1732]
;; ABCLFUHJ
;; ░░▓▓░░▓▓▓░░░▓▓░░▓░░░░▓▓▓▓░▓░░▓░▓░░▓░░░▓▓░░ 
;;  ▓░░▓░▓░░▓░▓░░▓░▓░░░░▓░░░░▓░░▓░▓░░▓░░░░▓░░░
;;  ▓░░▓░▓▓▓░░▓░░░░▓░░░░▓▓▓░░▓░░▓░▓▓▓▓░░░░▓░░░
;; ░▓▓▓▓░▓░░▓░▓░░░░▓░░░░▓░░░░▓░░▓░▓░░▓░░░░▓░░ 
;; ░▓░░▓░▓░░▓░▓░░▓░▓░░░░▓░░░░▓░░▓░▓░░▓░▓░░▓░  
;;  ▓░░▓░▓▓▓░░░▓▓░░▓▓▓▓░▓░░░░░▓▓░░▓░░▓░░▓▓░░  


;; 201912
(let [pos (->> (re-seq #"[\d-]+" (slurp "src/y2019/input201912"))
               (map #(Integer/parseInt %))
               (partition 3))
      vel (repeat (repeat 0))
      evolve (iterate
              (fn [{:keys [pos vel]}]
                (let [v-delta (for [a pos]
                                (->> (for [b pos]
                                       (map #(cond (< %1 %2) 1
                                                   (= %1 %2) 0
                                                   :else    -1) a b))
                                     (reduce #(map + %1 %2))))
                      new-v (map #(map + %1 %2) vel v-delta)
                      new-pos (map #(map + %1 %2) pos new-v)]
                  {:pos new-pos :vel new-v}))
              {:pos pos :vel vel})
      pos1 (->> (re-seq #"[\d-]+" (slurp "src/y2019/input201912"))
                (map #(Integer/parseInt %))
                (partition 3)
                (apply map vector))
      vel1 (repeat (count (first pos1)) 0)
      evolve1 (fn [x]
                (iterate
                 (fn [{:keys [pos vel]}]
                   (let [v-delta (for [a pos]
                                   (->> (for [b pos]
                                          (cond (< a b) 1
                                                (= a b) 0
                                                :else  -1))
                                        (reduce +)))
                         new-v   (map + vel v-delta)
                         new-pos (map + pos new-v)]
                     {:pos new-pos :vel new-v}))
                 {:pos x :vel vel1}))
      gcd (fn [a b] (if (zero? b) a (recur b (mod a b))))
      lcm (fn [a b] (/ (* a b) (gcd a b)))]
  [(->> (drop 1000 evolve)
        first
        ((fn [{:keys [pos vel]}]
           (let [p (map #(reduce (fn [total item] (+ total (Math/abs item))) 0 %) pos)
                 v (map #(reduce (fn [total item] (+ total (Math/abs item))) 0 %) vel)]
             (reduce + (map #(* %1 %2) p v))))))
   (reduce lcm (map #(loop [t (evolve1 %)
                            seen #{}
                            n 0]
                       (if (seen (first t))
                         n
                         (recur (rest t) (conj seen (first t)) (inc n))))
                    pos1))])
;; [6423 327636285682704]


;; TODO 201913


;; 201914
(let [d (fn [[x n] [y m]]
          [[x y] [n m]])
      e (fn [[_ n x]]
          [x (Integer/parseInt n)])
      f (fn [coll]
          (let [coll1 (map e coll)
                inputs (butlast coll1)
                output (last coll1)]
            (into {} (map d (repeat output) inputs))))
      data (->> (re-seq #"[^\n]+" (slurp "src/y2019/input201914"))
                (map #(f (re-seq #"(\d+) (\w+)" %)))
                (reduce merge {}))
      depth-tree (loop [data-keys (keys data)
                        r {"ORE" 0}]
                   (if (empty? data-keys)
                     r
                     (let [directs (filter #(r (second %)) data-keys)
                           directs-list (map #(hash-map (first %) (inc (r (second %)))) directs)]
                       (recur (remove #(and (r (second %))
                                            (not (some (fn [x] (= (second %) (first x))) data-keys)))
                                      data-keys)
                              (apply merge-with max r directs-list)))))
      g (fn [x]
          (loop [demand {"FUEL" x}
                 process-order (reverse (map first (sort-by #(second %) depth-tree)))]
            (if (and (= 1 (count demand)) (demand "ORE"))
              (demand "ORE")
              (let [process-item (first process-order)
                    quantity (demand process-item)
                    newmap (->> (filter #(= process-item (ffirst %)) data)
                                (map (fn [[[_ che] [a b]]] {che (* (Math/ceil (/ quantity a)) b)})))
                    newdemand (dissoc demand process-item)]
                (recur (apply merge-with + newdemand newmap) (rest process-order))))))]
  [(g 1)
   (let [n (iterate #(* 2 %) 1)]
     (loop [base 0]
       (let [c (->> (map #(vector (g %) %) (map + n (repeat base)))
                    (take-while #(< (first %) 1000000000000)))]
         (if (empty? c)
           base
           (recur (second (last c)))))))])
;; [899155.0 2390226]


;; TODO 201915


;; 201916
;; part2 credit to https://gitlab.com/a4j/advent-of-code-2019/blob/master/src/adventofcode2019/day16.clj
(let [input (->> (re-seq #"\d" (slurp "src/y2019/input201916") #_"03036732577212944063491565474664")
                 (map #(Integer/parseInt %)))
      len (count input)
      base-pattern '(0 1 0 -1)
      f (let [length (drop 1 (range (inc len)))
              patterns (map (fn [n] (drop 1 (cycle (flatten (map #(repeat n %) base-pattern))))) length)]
          (fn [coll]
            (for [pattern patterns]
              (Math/abs (rem (reduce + (map * coll pattern)) 10)))))
      skip (reduce #(+ (* 10 %1) %2) 0 (take 7 input))
      input1 (reverse (drop skip (flatten (repeat 10000 input))))
      g (partial reductions #(mod (+ %1 %2) 10))]
  [(apply str (take 8 (first (drop 100 (iterate f input)))))
   (apply str (take 8 (reverse (first (drop 100 (iterate g input1))))))])
;; ["40580215" "22621597"]


;; TODO 201917


;; TODO 201918


;; TODO for day 13
(let [intcode-computer-13
      (fn [file]
        (fn []
          (loop [state {:output [] :pc 0 :base 0}
                 program (->> (re-seq #"-?\d+" (slurp file))
                              (mapv #(BigInteger. %))
                              (map-indexed (fn [idx itm] [idx itm]))
                              (into (sorted-map)))]
            (let [base (:base state)
                  pc (:pc state)
                  [op op1 op2 op3] (map #(program % 0) (range pc (+ pc 4)))
                  eop (mod op 100)
                  [op1mode op2mode op3mode] (map #(mod (int (/ op %)) 10) [100 1000 10000])
                  op1 (cond
                        (and (= 2 op1mode) (= 3 eop))                (+ base op1)
                        (and (= 0 op1mode) (#{1 2 4 5 6 7 8 9} eop)) (program op1 0)
                        (and (= 2 op1mode) (#{1 2 4 5 6 7 8 9} eop)) (program (+ base op1) 0)
                        :else                                        op1)
                  op2 (cond
                        (and (= 0 op2mode) (#{1 2 5 6 7 8} eop))     (program op2 0)
                        (and (= 2 op2mode) (#{1 2 5 6 7 8} eop))     (program (+ base op2) 0)
                        :else                                        op2)
                  op3 (cond                                          
                        (and (#{1 2 7 8} eop) (= 2 op3mode))         (+ base op3)
                        :else                                        op3)]
              (cond
                (= eop 1) (recur (update state :pc #(+ % 4))
                                 (assoc program op3 (+ op1 op2)))
                (= eop 2) (recur (update state :pc #(+ % 4))
                                 (assoc program op3 (* op1 op2)))      
                (= eop 3) (recur (-> (update state :pc #(+ % 2)) (update :input #(drop 1 %)))
                                 (assoc program op1 (:input state)))
                (= eop 4) (recur (-> (update state :output #(conj % op1)) (update :pc #(+ % 2)))
                                 program)
                (= eop 5) (recur (if (not= 0 op1) (assoc state :pc op2) (update state :pc #(+ % 3)))
                                 program)
                (= eop 6) (recur (if (zero? op1)  (assoc state :pc op2) (update state :pc #(+ % 3)))
                                 program)
                (= eop 7) (recur (update state :pc #(+ % 4))
                                 (assoc program op3 (if (< op1 op2) 1 0)))
                (= eop 8) (recur (update state :pc #(+ % 4))
                                 (assoc program op3 (if (= op1 op2) 1 0)))
                (= eop 9) (recur (-> (update state :base #(+ % op1)) (update :pc #(+ % 2)))
                                 program)
                (= eop 99) (:output state))))))
      f (intcode-computer-13 "src/y2019/input201913")]
  (->> (partition 3 (f))
       (reduce (fn [r [x y v]] (conj r [[x y] v])) {})
       (filter #(= 2 (second %)))
       count))


;; TODO 201919
(let [intcode-computer-19
      (fn [file]
        (fn [input]
          (loop [state {:input input :output [] :pc 0 :base 0}
                 program (->> (re-seq #"-?\d+" (slurp file))
                              (map #(BigInteger. %))
                              (map-indexed (fn [idx itm] [idx itm]))
                              (into (sorted-map)))]
            (let [base (:base state)
                  pc (:pc state)
                  [op op1 op2 op3] (map #(program % 0) (range pc (+ pc 4)))
                  eop (mod op 100)
                  [op1mode op2mode op3mode] (map #(mod (int (/ op %)) 10) [100 1000 10000])
                  op1 (cond
                        (and (= 2 op1mode) (= 3 eop))                (+ base op1)
                        (and (= 0 op1mode) (#{1 2 4 5 6 7 8 9} eop)) (program op1 0)
                        (and (= 2 op1mode) (#{1 2 4 5 6 7 8 9} eop)) (program (+ base op1) 0)
                        :else                                        op1)
                  op2 (cond
                        (and (= 0 op2mode) (#{1 2 5 6 7 8} eop))     (program op2 0)
                        (and (= 2 op2mode) (#{1 2 5 6 7 8} eop))     (program (+ base op2) 0)
                        :else                                        op2)
                  op3 (cond                                          
                        (and (#{1 2 7 8} eop) (= 2 op3mode))         (+ base op3)
                        :else                                        op3)]
              (cond
                (= eop 1) (recur (update state :pc #(+ % 4))
                                 (assoc program op3 (+ op1 op2)))
                (= eop 2) (recur (update state :pc #(+ % 4))
                                 (assoc program op3 (* op1 op2)))      
                (= eop 3) (recur (-> (update state :pc #(+ % 2)) (update :input #(drop 1 %)))
                                 (assoc program op1 (first (:input state))))
                (= eop 4) (recur (-> (update state :output #(conj % op1)) (update :pc #(+ % 2)))
                                 program)
                (= eop 5) (recur (if (not= 0 op1) (assoc state :pc op2) (update state :pc #(+ % 3)))
                                 program)
                (= eop 6) (recur (if (zero? op1)  (assoc state :pc op2) (update state :pc #(+ % 3)))
                                 program)
                (= eop 7) (recur (update state :pc #(+ % 4))
                                 (assoc program op3 (if (< op1 op2) 1 0)))
                (= eop 8) (recur (update state :pc #(+ % 4))
                                 (assoc program op3 (if (= op1 op2) 1 0)))
                (= eop 9) (recur (-> (update state :base #(+ % op1)) (update :pc #(+ % 2)))
                                 program)
                (= eop 99) (:output state))))))
      f (intcode-computer-19 "src/y2019/input201919")]
  (count (filter #{1} (flatten (for [a (range 0 50)
                                     b (range 0 50)]
                                 (f [a b]))))))
