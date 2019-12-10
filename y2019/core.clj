(ns y2019.core)

(defn aoc201901 []
  [(->> (re-seq #"\d+" (slurp "y2019/input201901"))
        (map #(Integer/parseInt %))
        (map #(/ (- % (mod % 3) 6) 3))
        (reduce +))
   (->> (re-seq #"\d+" (slurp "y2019/input201901"))
        (map #(Integer/parseInt %))
        (map #(iterate (fn [x] (/ (- x (mod x 3) 6) 3)) %))
        (map #(drop 1 %))
        (map #(take-while pos? %))
        flatten
        (reduce +))])
;; (aoc201901)
;; [3216868 4822435]

(defn aoc201902 []
  (let [f (fn [start end]
            (loop [pc 0
                   program (assoc (->> (re-seq #"\d+" (slurp "y2019/input201902"))
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
          second)]))
;; (aoc201902)
;; [3716293 6429]

(defn aoc201903 []
  (let [f (fn [{[x y] :o r :r d :d} [direction & distance]]
            (let [distance (Integer/parseInt (apply str distance))]
              (merge {:d (+ d distance)}
                     (cond
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
                            r)}))))
        [line1 line2] (clojure.string/split (slurp "y2019/input201903") #"\n")
        l1 (:r (reduce f {:d 0 :o [0 0] :r #{}} (re-seq #"[^,]+" line1)))
        l2 (:r (reduce f {:d 0 :o [0 0] :r #{}} (re-seq #"[^,]+" line2)))
        marks (clojure.set/intersection l1 l2)]
    [(reduce min (map #(+ (Math/abs (first %)) (Math/abs (second %))) marks))
     (reduce min (map #(+ (:d (meta (l1 %))) (:d (meta (l2 %)))) marks))]))
;; (aoc201903)
;; [245 48262]

(defn aoc201904 []
  (let [[start end] (->> (re-seq #"\d+" (slurp "y2019/input201904"))
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
               (count)) [f g])))
;; (aoc201904)
;; (511 316)

(defn aoc201905 []
  (let [intcode-computer
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
        f (intcode-computer "y2019/input201905")]
    [(f [1]) (f [5])]))
;; (aoc201905)
;; [12234644 3508186]

(defn aoc201906 []
  (let [lines (->> (re-seq #"[^\n)]+" (slurp "y2019/input201906"))
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
           (recur (conj r temp)))))]))
;; (aoc201906)
;; [194721 316]

(defn aoc201907 []
  (let [intcode-computer
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
        intcode-computer-feedback-loop-mode
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
        f (intcode-computer "y2019/input201907")
        g (intcode-computer-feedback-loop-mode "y2019/input201907")]
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
          (reduce max))]))
;; (aoc201907)
;; [24405 8271623]

(defn aoc201908 []
  [(->> (re-seq #"\d" (slurp "y2019/input201908"))
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
   (->> (re-seq #"\d" (slurp "y2019/input201908"))
        (partition (* 25 6))
        (apply map list)
        (map #(first (drop-while #{"2"} %)))
        (map #(if (= "1" %) "#" "."))
        (partition 25)
        (map #(str (apply str %) "\n"))
        (apply str)
        println)])
;; (aoc201908)
;; [2440 nil]
;; 0110011110011000011001100
;; 1001000010100100001010010
;; 1001000100100000001010000
;; 1111001000100000001010000
;; 1001010000100101001010010
;; 1001011110011000110001100

(defn aoc201909 []
  (let [intcode-computer
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
                  (= eop 3) (recur (-> (update state :input #(drop 1 %)) (update :pc #(+ % 2)))
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
        f (intcode-computer "y2019/input201909")]
    [(last (f 1)) (last (f 2))]))
;; (aoc201909)
;; [[2789104029N] [32869N]]

