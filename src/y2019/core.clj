(ns y2019.core
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

;; A program can be in these states: :init, :running, :paused or :done
;; When a program is created it's in :init state
;; fn/run-program puts a (:init|:paused) program in :running state and returns when program pauses due to IO(:paused) or halts(:done), unless program is already in :done state when program will simple be returned unchanged and any extra input fed to the program wouldn't have any effect and is ignored.

(defn- read-computer-program [f]
  {:pc     0
   :base   0
   :input  []
   :output []
   :status :init
   :code (->> (slurp f)
              (re-seq #"-?\d+")
              (map-indexed (fn [idx itm] [idx (edn/read-string itm)]))
              (into (sorted-map)))})

(defn- run-program* [{:keys [input output pc code base] :as p}]
  (let [[op op1 op2 op3] (map #(code % 0) (range pc (+ pc 4)))
        eop (mod op 100)
        [op1mode op2mode op3mode] (map #(mod (quot op %) 10) [100 1000 10000])
        op1 (cond
              (and (= 2 op1mode) (= 3 eop))                (+ base op1)
              (and (= 0 op1mode) (#{1 2 4 5 6 7 8 9} eop)) (code op1 0)
              (and (= 2 op1mode) (#{1 2 4 5 6 7 8 9} eop)) (code (+ base op1) 0)
              :else                                        op1)
        op2 (cond
              (and (= 0 op2mode) (#{1 2 5 6 7 8} eop))     (code op2 0)
              (and (= 2 op2mode) (#{1 2 5 6 7 8} eop))     (code (+ base op2) 0)
              :else                                        op2)
        op3 (cond
              (and (#{1 2 7 8} eop) (= 2 op3mode))         (+ base op3)
              :else                                        op3)]
    (->> (cond
           (= eop 1) {:pc (+ pc 4) :code (assoc code op3 (+ op1 op2))}
           (= eop 2) {:pc (+ pc 4) :code (assoc code op3 (* op1 op2))}
           (= eop 3) (if (< 0 (count input))
                       {:pc (+ pc 2) :code (assoc code op1 (first input)) :input (subvec input 1)}
                       {:status :paused})
           (= eop 4) {:pc (+ pc 2) :output (conj output op1)}
           (= eop 5) {:pc (if (not= 0 op1) op2 (+ pc 3))}
           (= eop 6) {:pc (if (   = 0 op1) op2 (+ pc 3))}
           (= eop 7) {:pc (+ pc 4) :code (assoc code op3 (if (< op1 op2) 1 0))}
           (= eop 8) {:pc (+ pc 4) :code (assoc code op3 (if (= op1 op2) 1 0))}
           (= eop 9) {:pc (+ pc 2) :base (+ base op1)}
           (= eop 99) {:status :done})
         (merge p))))

(defn- run-program
  [program]
  (if (= (:status program) :done)
    program
    (->> (iterate run-program* (assoc program :status :running))
         (some #(if (not= :running (:status %)) %)))))

;; 201901
(let [input (->> (slurp "src/y2019/input201901")
                 (re-seq #"\d+")
                 (map edn/read-string))]
  [(->> input (map #(- (quot % 3) 2)) (reduce +))
   (->> input
        (mapcat #(->> (iterate (fn [x] (- (quot x 3) 2)) %)
                      rest
                      (take-while pos?)))
        (reduce +))])
;; [3216868 4822435]


;; 201902
(let [program (read-computer-program "src/y2019/input201902")
      f #((:code %) 0)]
  [(f (run-program (update program :code merge {1 12, 2 2})))
   (first (for [start (range 100)
                end   (range 100)
                :when (= 19690720 (f (run-program (update program :code merge {1 start, 2 end}))))]
            (+ (* start 100) end)))])
;; [3716293 6429]


;; 201903
(let [f (fn [{[x y] :o r :r d :d} [direction & distance]]
          (let [distance (edn/read-string (apply str distance))]
            (assoc (cond
                     (= direction \U)
                     {:o [x (+ y distance)]
                      :r (clojure.set/union
                          (set (for [n (range distance)] ^{:d (+ d n 1)} [x (+ y n 1)]))
                          r)}
                     (= direction \D)
                     {:o [x (- y distance)]
                      :r (clojure.set/union
                          (set (for [n (range distance)] ^{:d (+ d n 1)} [x (- y n 1)]))
                          r)}
                     (= direction \R)
                     {:o [(+ x distance) y]
                      :r (clojure.set/union
                          (set (for [n (range distance)] ^{:d (+ d n 1)} [(+ x n 1) y]))
                          r)}
                     (= direction \L)
                     {:o [(- x distance) y]
                      :r (clojure.set/union
                          (set (for [n (range distance)] ^{:d (+ d n 1)} [(- x n 1) y]))
                          r)})
                   :d
                   (+ d distance))))
      [line1 line2] (re-seq #"\S+" (slurp "src/y2019/input201903"))
      l1 (:r (reduce f {:d 0 :o [0 0] :r #{}} (re-seq #"\w+" line1)))
      l2 (:r (reduce f {:d 0 :o [0 0] :r #{}} (re-seq #"\w+" line2)))
      marks (clojure.set/intersection l1 l2)]
  [(reduce min (map #(+ (Math/abs (first %)) (Math/abs (second %))) marks))
   (reduce min (map #(+ (:d (meta (l1 %))) (:d (meta (l2 %)))) marks))])
;; [245 48262]


;; 201904
(let [[start end] (->> (slurp "src/y2019/input201904") (re-seq #"\d+") (mapv edn/read-string))
      f (fn [s] (and
                 (apply <= s)
                 (some #(= (first %) (second %)) (partition 2 1 s))))
      g (fn [s] (and
                 (apply <= s)
                 (some #(and (= (first %) (second %)) (= 2 (count %))) (partition-by identity s))))]
  (map #(count (for [n (range start (inc end))
                     :when (% (map int (seq (str n))))] true))
       [f g]))
;; (511 316)


;; 201905
(let [program (read-computer-program "src/y2019/input201905")]
  (map #(->> (run-program (conj program [:input [%]])) :output last) [1 5]))
;; (12234644 3508186)


;; 201906
(let [lines (->> (slurp "src/y2019/input201906") (re-seq #"\w+") (partition 2))
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
              (recur (into r (map (fn [x] [(second x) (inc (r (first x)))]) v1)) v2))))
        vals
        (reduce +))
   (loop [r '("SAN")]
     (let [temp (ffirst (filter #(= (second %) (first r)) lines))]
       (if (youset temp)
         (+ (count r) (count (drop-while #(not= temp %) youpath)) -3)
         (recur (conj r temp)))))])
;; [194721 316]


;; 201907
(let [program (read-computer-program "src/y2019/input201907")]
  [(->> (for [a (range 5), b (range 5), c (range 5), d (range 5), e (range 5)
              :when (= 5 (count (hash-set a b c d e)))]
          (reduce #(last (:output (run-program (conj program [:input [%2 %]]))))
                  0
                  [a b c d e]))
        (reduce max))
   (->> (for [a (range 5 10), b (range 5 10), c (range 5 10), d (range 5 10), e (range 5 10)
              :when (= 5 (count (hash-set a b c d e)))
              :let [programs (mapv #(run-program (conj program [:input [%]])) [a b c d e])]]
          (->> {:programs programs :signal 0 :cnt 0}
               (iterate
                (fn [{:keys [programs signal cnt]}]
                  (if signal
                    (let [program (programs cnt)
                          new-program (run-program (conj program [:input [signal]]))
                          new-signal (first (:output new-program))
                          new-programs (assoc programs cnt (update new-program :output #(vec (rest %))))]
                      {:programs new-programs :signal new-signal :cnt (mod (inc cnt) 5)}))))
               (take-nth 5)
               (take-while identity)
               last
               :signal))
        (reduce max))])
;; [24405 8271623]


;; 201908
[(->> (re-seq #"\d" (slurp "src/y2019/input201908"))
      (map edn/read-string)
      (partition (* 25 6))
      (map (fn [x] [(count (filter zero? x)) x]))
      (sort-by first)
      first
      second
      (remove #{0})
      frequencies
      vals
      (reduce *))
 (->> (re-seq #"\d" (slurp "src/y2019/input201908"))
      (partition (* 25 6))
      (apply map list)
      (map #(if (= "1" (first (drop-while #{"2"} %))) "▓" "░"))
      (partition 25)
      (mapv #(println (str/join %))))]
;; [2440 [nil nil nil nil nil nil]]
;; 0110011110011000011001100
;; 1001000010100100001010010
;; 1001000100100000001010000
;; 1111001000100000001010000
;; 1001010000100101001010010
;; 1001011110011000110001100
;; Or "AZCJC"
;; ░▓▓░░▓▓▓▓░░▓▓░░░░▓▓░░▓▓░░
;; ▓░░▓░░░░▓░▓░░▓░░░░▓░▓░░▓░
;; ▓░░▓░░░▓░░▓░░░░░░░▓░▓░░░░
;; ▓▓▓▓░░▓░░░▓░░░░░░░▓░▓░░░░
;; ▓░░▓░▓░░░░▓░░▓░▓░░▓░▓░░▓░
;; ▓░░▓░▓▓▓▓░░▓▓░░░▓▓░░░▓▓░░


;; 201909
(let [program (read-computer-program "src/y2019/input201909")]
  (map #(->> (run-program (conj program [:input [%]]))
             :output
             last) [1 2]))
;; (2789104029 32869)


;; 201910
(let [lines (re-seq #"\S+" (slurp "src/y2019/input201910"))
      cords (mapcat
             #(map vector (keep-indexed (fn [idx itm] (if (= itm \#) idx)) %1) (repeat %2))
             lines
             (range))
      f (fn [[basex basey :as base]]
          (fn [map-by-slope]
            {:base base
             :map map-by-slope
             :detect (reduce +
                             (cond
                               (empty? (:vertical map-by-slope))                                       0
                               (and (some #(< (second %) basey) (map first (:vertical map-by-slope)))
                                    (some #(> (second %) basey) (map first (:vertical map-by-slope)))) 2
                               :else                                                                   1)
                             (map (fn [cord-and-slopes]
                                    (let [xs (map ffirst cord-and-slopes)]
                                      (if (and (some #(< % basex) xs)
                                               (some #(> % basex) xs)) 2 1)))
                                  (vals (dissoc map-by-slope :vertical))))}))
      base (reduce
            #(max-key :detect %1 %2)
            (for [[x0 y0 :as a] cords]
              (->> (for [[x1 y1 :as b] cords
                         :when (not= a b)]
                     (if (= x0 x1) [b :vertical] [b (/ (- y1 y0) (- x1 x0))]))
                   (group-by second)
                   ((f a)))))
      g (fn [[basex basey]]
          (fn [maps-by-slope]
            (let [vert-points (->> (:vertical maps-by-slope)
                                   (map first)
                                   (sort-by second)
                                   (split-with #(> basey (second %))))
                  other-points (->> (vals (into (sorted-map) (dissoc maps-by-slope :vertical)))
                                    (map (fn [cord-and-slopes]
                                           (->> (map first cord-and-slopes)
                                                (sort-by first)
                                                (split-with #(> basex (first %)))))))]
              (concat [(reverse (first vert-points))]
                      (map second other-points)
                      [(second vert-points)]
                      (map (comp reverse first) other-points)))))]
  [(:detect base)
   (->> (loop [r []
               lines (remove empty? ((g (:base base)) (:map base)))]
          (if (< 199 (count r))
            (->> (r 199) (map * [100 1]) (reduce +))
            (recur (into r (map first lines)) (remove empty? (map rest lines))))))])
;; [288 616]


;; 201911
(let [dirs {0   [0 1]
            180 [0 -1]
            90  [1 0]
            270 [-1 0]}
      program (read-computer-program "src/y2019/input201911")
      f (fn [default-color]
          (loop [program program
                 panels  {[0 0] default-color}
                 dir     0
                 pos     [0 0]]
            (let [{:keys [output] :as new-program} (run-program (conj program [:input [(panels pos 0)]]))]
              (if (= :done (:status new-program))
                panels
                (let [new-dir (-> (if (= 0 (output 1)) 270 90) (+ dir) (mod 360))]
                  (recur (assoc new-program :output [])
                         (assoc panels pos (output 0))
                         new-dir
                         (map + (dirs new-dir) pos)))))))
      ans1 (f 0)
      ans2 (f 1)
      _ (let [[[x0 x1] [y0 y1]] (map #((juxt first last) (sort (map % (keys ans2)))) [first second])
              [x y] [(- x1 x0) (- y1 y0)]]
          (->> (reduce #(let [[[a b] v] %2]
                          (assoc-in %1 [(- y (- b y0)) (- a x0)] (if (= 1 v) "▓" "░")))
                       (vec (repeat (inc y) (vec (repeat (inc x) "░"))))
                       ans2)
               (mapv #(println (apply str %)))))]
  (count ans1))
;; 1732
;; ABCLFUHJ
;; ░░▓▓░░▓▓▓░░░▓▓░░▓░░░░▓▓▓▓░▓░░▓░▓░░▓░░░▓▓░░░
;; ░▓░░▓░▓░░▓░▓░░▓░▓░░░░▓░░░░▓░░▓░▓░░▓░░░░▓░░░
;; ░▓░░▓░▓▓▓░░▓░░░░▓░░░░▓▓▓░░▓░░▓░▓▓▓▓░░░░▓░░░
;; ░▓▓▓▓░▓░░▓░▓░░░░▓░░░░▓░░░░▓░░▓░▓░░▓░░░░▓░░░
;; ░▓░░▓░▓░░▓░▓░░▓░▓░░░░▓░░░░▓░░▓░▓░░▓░▓░░▓░░░
;; ░▓░░▓░▓▓▓░░░▓▓░░▓▓▓▓░▓░░░░░▓▓░░▓░░▓░░▓▓░░░░


;; 201912
(let [pos (->> (re-seq #"[\d-]+" (slurp "src/y2019/input201912"))
               (map edn/read-string)
               (partition 3)
               (apply map vector))
      vel (repeat (count (first pos)) 0)
      evolve (fn [x]
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
                {:pos x :vel vel}))
      gcd (fn [a b] (if (= 0 b) a (recur b (mod a b))))
      lcm (fn [a b] (/ (* a b) (gcd a b)))]
  [(->> (map #(nth (evolve %) 1000) pos)
        (map (juxt :pos :vel))
        flatten
        (map #(Math/abs %))
        (partition 8)
        (apply map #(reduce + %&))
        (partition 4)
        (apply map *)
        (reduce +))
   (reduce lcm (map #(loop [[t & more] (evolve %)
                            seen #{}
                            n 0]
                       (if (seen t)
                         n
                         (recur more (conj seen t) (inc n))))
                    pos))])
;; [6423 327636285682704]


;; 201913
(let [state {:program    (read-computer-program "src/y2019/input201913")
             :video      {}
             :score      0
             :paddle-pos nil
             :input      [0]}
      f (fn [init]
          (->> (update-in state [:program :code] #(merge % init))
               (iterate
                (fn [{:keys [program video paddle-pos input] :as state}]
                  (let [{:keys [output] :as new-program} (run-program (conj program [:input input]))]
                    (reduce
                     (fn [state [x y v]]
                       (if (and (= -1 x) (= 0 y))
                         (assoc state :score v)
                         (let [state (assoc-in state [:video [x y]] v)]
                           (cond (= v 3)
                                 (assoc state :paddle-pos x)
                                 (and (= v 4) (:paddle-pos state))
                                 (assoc state :input [(cond (= x (:paddle-pos state)) 0
                                                            (> x (:paddle-pos state)) 1
                                                            :else -1)])
                                 :else state))))
                     (assoc state :program (assoc new-program :output []))
                     (partition 3 output)))))
               (some #(if (= :done (get-in % [:program :status])) %))))]
  [(-> (f {}) :video vals frequencies (get 2))
   (-> (f {0 2}) :score)])
;; [318 16309]


;; 201914
(let [d (fn [[x n] [y m]]
          [[x y] [n m]])
      e (fn [[_ unit chemical]]
          [chemical (edn/read-string unit)])
      f (fn [coll]
          (let [coll1 (map e coll)
                inputs (butlast coll1)
                output (last coll1)]
            (map d (repeat output) inputs)))
      data (->> (re-seq #".+" (slurp "src/y2019/input201914"))
                (mapcat #(f (re-seq #"(\d+) (\w+)" %)))
                (into {}))
      ;; line 31 SVCQ, 1 TXCR => 8 VMDT becomes {["VMDT" "TXCR"] [8 1], ["VMDT" "SVCQ"] [8 31]
      dep-tree (loop [data-keys (keys data)
                      r {"ORE" 0}]
                 (if (empty? data-keys)
                   r
                   (let [directs (filter #(r (second %)) data-keys)
                         directs-list (map #(hash-map (first %) (inc (r (second %)))) directs)]
                     (recur (remove #(and (r (second %))
                                          (not-any? (fn [x] (= (second %) (first x))) data-keys))
                                    data-keys)
                            (apply merge-with max r directs-list)))))
      ;; Ingredients and their levels in a dependency tree
      g (fn [x]
          (loop [demand {"FUEL" x}
                 [output & more] (map first (sort-by #(val %) > dep-tree))]
            (if (and (= 1 (count demand)) (demand "ORE"))
              (demand "ORE")
              (let [quantity (demand output)
                    input-map (->> (filter #(= output (ffirst %)) data)
                                   (map (fn [[[_ che] [a b]]] {che (* (Math/ceil (/ quantity a)) b)})))
                    newdemand (dissoc demand output)]
                (recur (apply merge-with + newdemand input-map) more)))))
      n (iterate #(* 2 %) 1)]
  [(g 1)
   (loop [n 0 base 1]
     (let [c (g (+ n base))]
       (cond (<= c 1000000000000) (recur n (* base 2))
             (= base 1) n
             :else (recur (+ n (/ base 2)) 1))))])
;; [899155.0 2390226]


;; 201915
(let [dirs {1 [0 1], 2 [0 -1], 3 [-1 0], 4 [1 0]}
      commands (range 1 5)
      back-track {1 2, 2 1, 3 4, 4 3}
      neighbors (fn [pos] (map #(mapv + pos (dirs %)) commands))
      oxygen-loc (fn [plan] (first (keep #(if (= 2 (val %)) (key %)) plan)))
      next-move (fn [plan pos from]
                  (let [back (back-track from)
                        moves (->> commands
                                   (remove #{back})
                                   (filter #(= -1 (get plan (mapv + pos (dirs %)) -1))))]
                    (or (first moves) back)))
      f (fn [plan]
          (let [goal (oxygen-loc plan)]
            (loop [step 0
                   neighbors #{}
                   new-neighbors #{[0 0]}]
              (if (new-neighbors goal)
                step
                (recur (inc step)
                       (clojure.set/union neighbors new-neighbors)
                       (clojure.set/difference
                        (set (mapcat (fn [neighbor]
                                       (keep #(let [loc (mapv + neighbor (dirs %))]
                                                (if (not= 0 (get plan loc)) loc)) commands))
                                     new-neighbors))
                        neighbors))))))
      g (fn [plan]
          (loop [step -1
                 oxygen-locations (conj #{} (oxygen-loc plan))
                 seen-locations oxygen-locations]
            (if (empty? oxygen-locations)
              step
              (let [new-locations (clojure.set/difference
                                   (set (mapcat (fn [oxygen]
                                                  (keep #(let [loc (mapv + oxygen (dirs %))]
                                                           (if (= 1 (get plan loc)) loc)) commands))
                                                oxygen-locations))
                                   seen-locations)]
                (recur (inc step)
                       new-locations
                       (clojure.set/union oxygen-locations new-locations))))))
      plan (loop [program (read-computer-program "src/y2019/input201915")
                  plan    {[0 0] 1}
                  pos     [0 0]
                  input   (rand-nth commands)
                  track   '(:start)
                  open-cell #{}]
             (let [{:keys [output] :as new-program} (run-program (conj program [:input [input]]))
                   status (output 0)
                   probed-pos (mapv + (dirs input) pos)
                   new-program (assoc new-program :output [])
                   new-plan (assoc plan probed-pos status)
                   new-pos (if (= 0 status) pos probed-pos)
                   open-cell ((if (some #(= -1 (get new-plan (mapv + new-pos (dirs %)) -1)) commands)
                                conj
                                disj) open-cell new-pos)
                   new-track (cond (= 0 status) track
                                   (= (back-track input) (peek track)) (rest track)
                                   :else (conj track input))
                   new-input (next-move new-plan new-pos (first new-track))]
               (if (empty? open-cell)
                 new-plan
                 (recur new-program new-plan new-pos new-input new-track open-cell))))
      _ (let [[[x0 x1] [y0 y1]] (map #((juxt first last) (sort (map % (keys plan))))
                                     [first second])
              r (reduce #(assoc-in % (map - (key %2) [x0 y0]) (case (val %2), 0 "▓", 1 "░", 2 "X"))
                        (vec (repeat (- x1 x0 -1) (vec (repeat (- y1 y0 -1) " "))))
                        plan)
              r (assoc-in r (map - [0 0] [x0 y0]) "O")]
          (dorun (for [row r] (println (apply str row)))))]
  [(f plan) (g plan)])
;; [220 334]
;; O:Start location X:oxygen system
;;  ▓▓▓ ▓▓▓ ▓▓▓▓▓ ▓▓▓ ▓▓▓▓▓▓▓ ▓▓▓ ▓▓▓▓▓▓▓ ▓
;; ▓░░░▓░░░▓░░░░░▓░░░▓░░░░░░░▓░░░▓░░░░░░░▓░▓
;;  ▓▓░▓░▓░▓░▓░▓▓▓░▓░▓░▓▓▓░▓░▓░▓░▓░▓░▓▓▓░▓░▓
;; ▓░░░▓░▓░░░▓░░░░░▓░▓░░░▓░▓░▓░▓░░░▓░░░▓░░░▓
;; ▓░▓▓▓░▓▓▓▓▓▓▓▓▓▓▓░▓▓▓░▓░▓▓▓░▓▓▓▓ ▓▓░▓▓▓░▓
;; ▓░▓░░░▓░░░░░▓░░░▓░░░▓░▓░░░▓░▓░░░▓░░░▓░░░▓
;; ▓░▓░▓▓▓░▓▓▓░▓░▓░▓▓▓░▓░▓▓▓░▓░▓░▓░▓░▓▓▓░▓▓
;; ▓░▓░░░▓░▓░▓░░░▓░░░▓░░░░░▓░▓░░░▓░▓░▓░░░▓░▓
;; ▓░▓░▓▓▓░▓░▓▓▓▓▓▓▓░▓▓▓▓▓▓▓░▓▓▓▓▓░▓░▓░▓▓▓░▓
;; ▓░░░▓░░░▓░░░░░░░▓░░░▓░░░▓░░░▓░░░▓░▓░░░░░▓
;;  ▓▓▓▓░▓▓▓░▓▓▓░▓▓▓▓▓░▓░▓░▓▓▓░▓░▓▓▓░▓▓▓▓▓░▓
;; ▓░░░▓░░░░░▓░░░▓░░░░░▓░▓░░░░░▓░░░▓░▓░░░░░▓
;; ▓░▓░▓▓▓▓▓▓▓░▓▓▓░▓▓▓▓▓░▓▓▓▓▓▓▓▓▓░▓░▓▓▓░▓▓
;; ▓░▓░░░░░░░▓░░░▓░▓░░░░░▓░░░░░░░▓░▓░░░▓░░░▓
;; ▓░▓▓▓▓▓▓▓░▓▓▓░▓░▓░▓▓▓░▓░▓▓▓░▓▓▓░▓▓▓░▓▓▓░▓
;; ▓░░░▓░░░░░░░▓░▓░▓░▓░░░▓░░░▓░▓░░░▓░░░▓░░░▓
;;  ▓▓░▓░▓▓▓▓▓░▓░▓░▓░▓▓▓░▓▓▓░▓░▓░▓▓▓░▓▓▓▓▓▓
;; ▓░░░▓░░░░░▓░▓░▓░▓░░░▓░▓░░░▓░░░░░▓░▓░░░░░▓
;; ▓░▓▓▓▓▓▓▓░▓▓▓░▓░▓░▓░▓▓▓░▓▓ ▓▓▓▓▓▓░▓░▓▓▓░▓
;; ▓░▓░░░▓░▓░░░░░▓░▓░▓░░░▓░░░▓░░░░░▓░▓░░░▓░▓
;; ▓░▓░▓░▓░▓▓▓▓▓▓▓░▓░▓▓▓░▓░▓▓▓░▓▓▓░▓░▓▓▓░▓░▓
;; ▓░▓░▓░▓░░░░░▓░▓░▓░▓O▓░░░▓░░░▓░▓░▓░░░░░▓░▓
;; ▓░▓░▓░▓▓▓░▓░▓░▓░▓▓▓░▓░▓▓▓░▓▓▓░▓░▓▓▓▓▓▓▓░▓
;; ▓░░░▓░░░▓░▓░░░▓░░░░░▓░▓░░░▓░░░▓░▓░░░░░░░▓
;;  ▓▓▓▓▓▓░▓░▓▓▓░▓▓▓▓▓▓▓░▓░▓▓▓░▓▓▓░▓░▓▓▓▓▓░▓
;; ▓░░░░░▓░▓░▓░░░▓░░░▓░░░▓░▓░░░▓░░░▓░▓░░░▓░▓
;; ▓░▓▓▓░▓░▓░▓░▓▓▓▓▓░▓░▓▓▓░▓▓▓░▓░▓▓▓░▓░▓░▓░▓
;; ▓░▓░░░▓░▓░▓░░░░░▓░░░▓░▓░░░▓░░░▓░░░▓░▓░▓░▓
;;  ▓▓░▓▓▓░▓░▓▓▓▓▓░▓▓▓░▓░▓▓▓░▓░▓▓▓░▓▓▓░▓░▓░▓
;; ▓░░░▓░░░▓░▓░░░░░░░▓░░░░░▓░▓░░░░░▓░░░▓░░░▓
;; ▓░▓▓▓░▓▓▓░▓▓▓▓▓▓▓░▓░▓▓▓▓▓░▓▓▓▓▓▓▓░▓▓▓▓▓▓
;; ▓░▓░░░▓░░░▓░░░▓░▓░▓░▓░░░░░▓░░░░░▓░▓░░░▓░▓
;; ▓░▓░▓▓▓░▓░▓░▓░▓░▓░▓░▓░▓▓▓▓▓░▓▓▓░▓░▓░▓░▓░▓
;; ▓░▓░░░▓░▓░░░▓░▓░▓░▓░▓░▓░▓░░░▓░░░▓░░░▓░░░▓
;; ▓░▓▓▓░▓░▓▓▓▓▓░▓░▓░▓▓▓░▓░▓░▓▓▓░▓▓ ▓▓▓▓▓▓░▓
;; ▓░░░░░▓░░░▓░░░▓░░░░░░░▓░░░▓░▓░░░▓░░░░░▓░▓
;; ▓░▓▓▓▓ ▓▓░▓░▓▓▓▓▓▓▓▓▓▓▓░▓▓▓░▓▓▓░▓░▓▓▓░▓░▓
;; ▓░▓░░░▓X░░▓░░░░░░░░░▓░▓░▓░░░▓░▓░░░▓░▓░▓░▓
;; ▓░▓░▓░▓▓▓▓▓▓▓▓▓▓▓▓▓░▓░▓░▓░▓░▓░▓▓▓▓▓░▓░▓░▓
;; ▓░░░▓░░░░░░░░░░░░░░░▓░░░░░▓░░░░░░░░░▓░░░▓
;;  ▓▓▓ ▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓ ▓▓▓▓▓ ▓▓▓▓▓▓▓▓▓ ▓▓▓


;; 201916
;; part2 credit to https://gitlab.com/a4j/advent-of-code-2019/blob/master/src/adventofcode2019/day16.clj
(let [input0 (->> (re-seq #"\d" (slurp "src/y2019/input201916"))
                  (map edn/read-string))
      base-pattern '(0 1 0 -1)
      f (let [length (range 1 (inc (count input0)))
              patterns (map #(->> (map (partial repeat %) base-pattern) cycle flatten rest) length)]
          (fn [coll]
            (for [pattern patterns]
              (Math/abs (rem (reduce + (map * coll pattern)) 10)))))
      skip (reduce #(+ (* 10 %1) %2) 0 (take 7 input0))
      input1 (reverse (drop skip (flatten (repeat 10000 input0))))
      g (partial reductions #(rem (+ %1 %2) 10))]
  [(apply str (take 8 (identity (first (drop 100 (iterate f input0))))))
   (apply str (take 8 (reverse  (first (drop 100 (iterate g input1))))))])
;; ["40580215" "22621597"]


;; 201917
(let [program (read-computer-program "src/y2019/input201917")
      cells (->> (run-program program)
                 :output
                 (partition-by #{10})
                 (keep #(if (not= 10 (first %)) (vec %)))
                 vec)
      max-y (count cells)
      max-x (count (first cells))
      offsets [[0 0] [1 0] [-1 0] [0 1] [0 -1]]
      neighbor (fn [loc] (for [offset offsets
                               :let [[y x] (map + loc offset)]
                               :when (and (<= 0 x (dec max-x))
                                          (<= 0 y (dec max-y))
                                          (= 35 (get-in cells [y x])))]
                           [y x]))]
  (reduce + (for [y (range max-y)
                  x (range max-x)
                  :when (= 5 (count (neighbor [y x])))]
              (* y x)))
  (run-program (update program :code merge {0 2})))


;; 201919
;; 0 on the top, then 1, then 2, and so on, all the way through to 10006 on the bottom.
;;   what number is on the card that ends up in position 2020
(let [program (read-computer-program "src/y2019/input201919")
      f #(->> (run-program (assoc program :input [% %2])) :output first)
      ans1 (count (for [a (range 50), b (range 50) :when (= 1 (f a b ))] 1))]
  ans1)


;; 201922
;; https://topaz.github.io/paste/#XQAAAQAgBQAAAAAAAAAzHIoib6pENkSmUIKIED8dy140D1lKWSMhNhZz+hjKgIgfJKPuwdqIBP14lxcYH/qI+6TyUGZUnsGhS4MQYaEtf9B1X3qIIO2JSejFjoJr8N1aCyeeRSnm53tWsBtER8F61O2YFrnp7zwG7y303D8WR4V0eGFqtDhF/vcF1cQdZLdxi/WhfyXZuWC+hs8WQCBmEtuId6/G0PeMA1Fr78xXt96Um/CIiLCievFE2XuRMAcBDB5We73jvDO95Cjg0CF2xgF4yt3v4RB9hmxa+gmt6t7wRI4vUIGoD8kX2k65BtmhZ7zSZk1Hh5p1obGZ6nuuFIHS7FpuSuv1faQW/FuXlcVmhJipxi37mvPNnroYrDM3PFeMw/2THdpUwlNQj0EDsslC7eSncZQPVBhPAHfYojh/LlqSf4DrfsM926hSS9Fdjarb9xBYjByQpAxLDcmDCMRFH5hkmLYTYDVguXbOCHcY+TFbl+G/37emZRFh/d+SkeGqbFSf64HJToM2I7N2zMrWP7NDDY5FWehD5gzKsJpEg34+sG7x2O82wO39qBlYHcYg1Gz4cLBrH1K1P+KWvEdcdj/NBtrl6yftMlCu6pH4WTGUe9oidaiRuQZOGtw71QsTQUuhpdoWO4mEH0U9+CiPZCZLaQolFDSky1J9nDhZZHy3+ETcUeDOfSu+HI3WuKC0AtIRPdG8B9GhtxZQKAx+5kyi/ek7A2JAY9SjrTuvRADxx5AikbHWXIsegZQkupAc2msammSkwY8dRMk0ilf5vh6kR0jHNbSi0g0KJLCJfqggeX24fKk5Mdh8ULZXnMfMZOmwEGfegByYbu91faLijfW4hoXCB1nlsWTPZEw2PCZqqhl9oc1q25H2YkkvKLxEZWl6a9eFuRzxhB840I1zdBjUVgfKd9/V4VdodzU2Z2e+VEh7RbJjQNFC/rG8dg==

(let [input (->> (slurp "src/y2019/input201922")
                 (re-seq #"(\S+) (\S+)\n")
                 (map (fn [[_ op1 op2]] [op1 (if (= "new" op1) 0 (edn/read-string op2))])))
      f (fn [total slot]
          (reduce (fn [pos [op1 op2]]
                    (cond
                      (= op1 "new")       (- total 1 pos)
                      (= op1 "increment") (mod (* pos op2) total)
                      (= op1 "cut")       (mod (- pos op2) total)))
                  slot
                  input))
      ;;ans2 (g 119315717514047 2020 101741582076661)
      ]
  [ans1 #_ans2]
  )


(let [prime 119315717514047
      prime' (dec prime)
      find-original-pos (fn [n pos]
                          (let [mod-pos (mod pos n)]
                            (if (= 0 mod-pos)
                              (/ pos n)
                              (let [mod-p  (- n mod-pos)
                                    mod-pn (mod prime n)
                                    x (loop [x (range)]
                                        (if (= mod-p (mod (* (first x) mod-pn) n))
                                          (first x)
                                          (recur (rest x))))]
                                (/ (+ pos (* x prime)) n)))))
      ops (reverse (re-seq #"(\w+) ([-\w]+)\n" (slurp "src/y2019/input201922")))]
  (loop [x (range 101741582076661)
         last-pos 2020]
    (when (= 0 (mod (first x) 10000)) (prn (first x) last-pos))
    (if (empty? x)
      last-pos
      (recur (rest x)
             (loop [ops ops
                    pos last-pos]
               (if-let [op (first ops)]
                 (cond
                   (= (second op) "new")
                   (recur (rest ops) (- prime' pos))
                   (= (second op) "increment")
                   (recur (rest ops)
                          (find-original-pos (Integer/parseInt (nth op 2)) pos))
                   (= (second op) "cut")
                   (recur (rest ops)
                          (let [n (Integer/parseInt (nth op 2))
                                n (if (< n 0) (+ prime n) n)]
                            (if (< pos n)
                              (+ pos (- n prime))
                              (+ pos n)))))
                 pos))))))



(map #(println % (mod % 8)) (sort [509848
510705
557460
558576
]))
