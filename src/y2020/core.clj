(ns y2020.core
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

;; 202001
(let [input (->> (slurp "src/y2020/input202001") (re-seq #"\d+") (map edn/read-string) sort)
      f (fn [sum xs]
          (loop [[x & morex :as x'] xs
                 [y & morey :as y'] (reverse xs)]
            (when (and x morex)
              (cond
                (= sum (+ x y)) (* x y)
                (< sum (+ x y)) (recur x' morey)
                :else           (recur morex y')))))]
  [(f 2020 input)
   (loop [[a & more] input]
     (if-let [ans (f (- 2020 a) more)]
       (* a ans)
       (recur more)))])
;; [793524 61515678]


;; 202002
(let [input (->> (slurp "src/y2020/input202002") (re-seq #"[\d\w]+") (partition 4))
      f (fn [[a b c s]]
          (let [[a b] (map edn/read-string [a b])]
            (if (<= a (count (filter (set c) s)) b) true)))
      g (fn [[a b c s]]
          (let [[a b] (map #(dec (edn/read-string %)) [a b])
                c (first c)]
            (if (not= (= c (get s a)) (= c (get s b))) true)))]
  (map #(count (keep % input)) [f g]))
;; (515 711)


;; 202003
(let [input (->> (slurp "src/y2020/input202003") (re-seq #".+"))
      len (count (first input))
      f (fn [step]
          (fn [line n]
            (get line (mod (* step n) len))))
      g (fn [[x y]] ((frequencies (map (f x) (rest (take-nth y input)) (rest (range)))) \#))]
  [(g [3 1]) (reduce * (map g [[1 1] [3 1] [5 1] [7 1] [1 2]]))])
;; [167 736527114]


;; 202004
(let [input (->> (slurp "src/y2020/input202004") (re-seq #"[\w#]+|\n\n") (partition-by #(= "\n\n" %)))
      f (fn [xs]
          (clojure.set/subset? #{"byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"} (set (take-nth 2 xs))))
      g (fn [xs]
          (let [{:strs [byr iyr eyr hgt hcl ecl pid]} (into {} (map vec (partition 2 xs)))]
            (and byr (<= 1920 (edn/read-string byr) 2002)
                 iyr (<= 2010 (edn/read-string iyr) 2020)
                 eyr (<= 2020 (edn/read-string eyr) 2030)
                 hgt (if-let [[_ height unit] (re-matches #"(\d+)(cm|in)" hgt)]
                       (let [h (edn/read-string height)]
                         (or (and (= "cm" unit) (<= 150 h 193))
                             (and (= "in" unit) (<= 59 h 76)))))
                 hcl (re-matches #"#[0-9a-f]{6}" hcl)
                 ecl (#{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} ecl)
                 pid (re-matches #"^\d{9}" pid))))
      input (filter f input)]
  [(count input) (count (filter g input))])
;; [260 153]


;; 202005
(let [f #(if (= (inc %2) %1) %2 (reduced (inc %2)))
      input (->> (str/escape (slurp "src/y2020/input202005") {\B \1 \F \0 \R \1 \L \0})
                 (re-seq #"\w+")
                 (map #(Integer/parseInt % 2))
                 sort
                 reverse)]
  [(first input) (reduce f input)])
;; [864 739]


;; 202006
(let [input (-> (slurp "src/y2020/input202006") (str/split #"\n\n"))]
  [(reduce + (map #(count (set (str/replace % "\n" ""))) input))
   (reduce + (map #(->> (map set (str/split-lines %)) (reduce clojure.set/intersection) count) input))])
;; [6799 3354]


;; 202007
(let [input (->> (slurp "src/y2020/input202007")
                 (re-seq #".+")
                 (map #(re-seq #"(\d+ )?(\w+ \w+)(?= bag)" %)))
      input1 (map (fn [[root & leaves]]
                    {:root (root 2) :leaves (set (map #(% 2) leaves))})
                  input)
      input2 (reduce (fn [m [root & leaves]]
                       (->> (into {} (for [[_ n name] leaves]
                                       [name (if (nil? n) 0 (edn/read-string n))]))
                            (assoc m (root 2))))
                     {"no other" 0}
                     input)
      f (fn [color-set]
          (set (for [{:keys [root leaves]} input1
                     :when (seq (clojure.set/intersection color-set leaves))]
                 root)))
      g (fn g [r [k v]]
          (if (= "no other" k) 0 (+ r v (* v (reduce g 0 (input2 k))))))]
  [(->> #{"shiny gold"}
        (iterate f)
        rest
        (take-while seq)
        (reduce clojure.set/union)
        count)
   (g -1 ["shiny gold" 1])])
;; [128 20189]


;; 202008
(let [input (->> (slurp "src/y2020/input202008")
                 (re-seq #"(\S+) (\S+)")
                 (mapv (fn [[_ op op1]] [op (edn/read-string op1)])))
      l (count input)
      f (fn [input mode]
          (->> {:i 0, :acc 0, :seen #{}}
               (iterate (fn [{:keys [i acc seen] :as state}]
                          (cond (= i l) acc
                                (seen i) (if (= :loop mode) acc nil)
                                :else (let [[op op1] (input i)]
                                        (merge (update state :seen #(conj % i))
                                               (condp = op
                                                 "nop" {:i (inc i)}
                                                 "acc" {:i (inc i) :acc (+ acc op1)}
                                                 "jmp" {:i (+ i op1)}))))))
               (drop-while map?)
               first))]
  [(f input :loop)
   (for [i (range (count input))
         :when (#{"nop" "jmp"} ((input i) 0))
         :let [input (update-in input [i 0] {"nop" "jmp", "jmp" "nop"})
               r (f input :bug)]
         :when r]
     r)])
;; [1200 (1023)]


;; 202009
(let [input (->> (slurp "src/y2020/input202009") (re-seq #"\d+") (mapv edn/read-string))
      p1 (loop [xs input]
           (let [top25 (subvec xs 0 25)
                 top25-set (into #{} top25)
                 n26 (xs 25)]
             (if (some #(and (top25-set %) (top25-set (- n26 %)) (not= % (- n26 %))) top25)
               (recur (subvec xs 1))
               n26)))
      p2 (loop [off0 0
                off1 2
                r (reduce + (subvec input off0 off1))]
           (cond (= r p1) (->> (subvec input off0 off1) sort ((juxt first last)) (reduce +))
                 (> r p1) (recur (inc off0) off1 (- r (input off0)))
                 (< r p1) (recur off0 (inc off1) (+ r (input off1)))))]
  [p1 p2])
;; [20874512 3012420]


;; 202010
;; xxxxxx:() x    x
;; xxxxx:(7) xxxxx x..xx x.x.x xx..x x.xxx xx.xx xxx.x
;; xxxx: (4) xxxx x..x x.xx xx.x
;; xxx:  (2) xxx x.x
;; xx:   (1) xx
(let [input (->> (slurp "src/y2020/input202010")
                 (re-seq #"\d+")
                 (map edn/read-string)
                 (cons 0)
                 sort
                 (partition 2 1)
                 (map #(- (second %) (first %))))
      {d1 1 d3 3} (frequencies input)
      options {4 7, 3 4, 2 2, 1 1}]
  [(* d1 (inc d3))
   (->> input
        (partition-by #(= 3 %))
        (map #(if (= 3 (first %)) 1 (options (count %))))
        (reduce *))])
;; [2484 15790581481472]


;; 202011
(let [input (slurp "src/y2020/input202011")
      input1 (str/split-lines input)
      max-y (count input1)
      max-x (count (first input1))
      seats-map (->> (re-seq #"." input)
                     (keep-indexed (fn [i cell]
                                     (if (not= cell ".")
                                       [[(mod i max-x) (quot i max-x)] cell])))
                     (into {}))
      seats (set (keys seats-map))
      adj-neighbors (fn [_ loc]
                      (for [a [-1 0 1]
                            b [-1 0 1]
                            :let [[x y :as xy] (map + loc [a b])]
                            :when (and (not= a b 0)
                                       (<= 0 x (dec max-x))
                                       (<= 0 y (dec max-y))
                                       (seats xy))]
                        xy))
      star-neighbors (fn [seats-map loc]
                       (for [a [-1 0 1]
                             b [-1 0 1]
                             :when (not= a b 0)
                             :let [r (->> (rest (range))
                                          (keep #(let [[x y :as xy] (->> (map * [a b] [% %]) (mapv + loc))
                                                       v (seats-map xy ".")]
                                                   (if (and (<= 0 x (dec max-x))
                                                            (<= 0 y (dec max-y)))
                                                     (if (#{"#" "L"} v) xy nil)
                                                     -1)))
                                          first)]
                             :when (not= -1 r)]
                         r))
      solution (fn [[neighbor-fn max-neighbor]]
                 (loop [seats-map seats-map]
                   (let [neighbor-coverage-map (->> seats
                                                    (filter #(= "#" (seats-map %)))
                                                    (mapcat #(neighbor-fn seats-map %))
                                                    frequencies)
                         changed-seats (keep #(let [n (neighbor-coverage-map % 0)]
                                                (cond (and (= "L" (seats-map %)) (= n 0))             [% "#"]
                                                      (and (= "#" (seats-map %)) (<= max-neighbor n)) [% "L"]))
                                             seats)]
                     (if (seq changed-seats)
                       (recur (into seats-map changed-seats))
                       (-> seats-map vals frequencies (get "#"))))))]
  (pmap solution [[adj-neighbors 4] [star-neighbors 5]]))
;; (2334 2100)


;; 202012
(let [input (->> (slurp "src/y2020/input202012") (re-seq #"\w+"))
      dir-map {90 [1 0], 180 [0 -1], 270 [-1 0], 0 [0 1]}
      f (fn [x] (reduce + (map #(Math/abs %) (:loc x))))
      ans1 (reduce (fn [{:keys [loc dir] :as state} instruction]
                     (let [v (edn/read-string (subs instruction 1))]
                       (case (first instruction)
                         \N (update state :loc #(map + % [0 v]))
                         \S (update state :loc #(map - % [0 v]))
                         \E (update state :loc #(map + % [v 0]))
                         \W (update state :loc #(map - % [v 0]))
                         \L (update state :dir #(mod (- % v) 360))
                         \R (update state :dir #(mod (+ % v) 360))
                         \F (update state :loc #(map + % (map * [v v] (dir-map dir)))))))
                   {:loc [0 0] :dir 90}
                   input)
      ans2 (reduce (fn [{:keys [loc way] :as state} instruction]
                     (let [v (edn/read-string (subs instruction 1))
                           [x y] way]
                       (case (first instruction)
                         \N (update state :way #(map + % [0 v]))
                         \S (update state :way #(map - % [0 v]))
                         \E (update state :way #(map + % [v 0]))
                         \W (update state :way #(map - % [v 0]))
                         \L (assoc  state :way (case v
                                                 90 [(- y) x]
                                                 180 [(- x) (- y)]
                                                 270 [y (- x)]))
                         \R (assoc  state :way (case v
                                                 270 [(- y) x]
                                                 180 [(- x) (- y)]
                                                 90 [y (- x)]))
                         \F (update state :loc #(map + % (map * way [v v]))))))
                   {:loc [0 0] :way [10 1]}
                   input)]
  (map f [ans1 ans2]))
;; (1645 35292)


;; 202013
(let [gcd (fn [a b] (if (= 0 b) a (recur b (mod a b))))
      lcm (fn [a b] (/ (* a b) (gcd a b)))
      f (fn [n [_ s]]
          (if s (let [x (edn/read-string s)] [x (mod (- (* x (quot n x)) n) x)])))
      [timestamp & buses1] (->> (slurp "src/y2020/input202013") (re-seq #"\d+") (map edn/read-string))
      buses2 (->> (slurp "src/y2020/input202013")
                  (re-seq #"(\d+)|x")
                  rest
                  (keep-indexed f))]
  [(->> (map #(vector (- % (mod timestamp %)) %) buses1) sort first (reduce *))
   ((reduce (fn [[factor1 t] [factor2 remainder2]]
              (loop [t t]
                (if (= (mod t factor2) remainder2)
                  [(lcm factor1 factor2) t]
                  (recur (+ t factor1))))) buses2) 1)])
;; [2935 836024966345345]


;; 202014
(let [input (->> (slurp "src/y2020/input202014") (re-seq #"(?:mask = (\w+))|(?:(\d+)\] = (\d+))"))
      power-of-2 (vec (take 36 (iterate #(* 2 %) 1)))  ;; [1 2 4 ... 2^35]
      f (fn [{:keys [mask mem] :as state} [_ bitmask mem-address v]]
          (if bitmask
            (assoc state :mask [(edn/read-string (str "2r" (str/replace bitmask "X" "0")))
                                (edn/read-string (str "2r" (str/escape bitmask {\1 \0, \X \1})))])
            (let [mem-address (edn/read-string mem-address)
                  pre-value (edn/read-string v)
                  post-value (bit-or (mask 0) (bit-and (mask 1) pre-value))]
              (assoc-in state [:mem mem-address] post-value))))
      g (fn [{:keys [mask mem] :as state} [_ bitmask mem-address v]]
          (if bitmask
            (assoc state :mask [;; turn on  0 bits
                                (edn/read-string (str "2r" (-> bitmask (str/escape {\1 \0, \X \0, \0 \1}))))
                                ;; turn on  1 bits
                                (edn/read-string (str "2r" (-> bitmask (str/replace "X" "0"))))
                                ;; turn off X bits
                                (edn/read-string (str "2r" (-> bitmask (str/escape {\0 \1, \X \0}))))
                                ;; bit-offset of X bits
                                (keep-indexed (fn [i x] (if (= x \X) (- 35 i))) bitmask)])
            (let [mem-address (edn/read-string mem-address)
                  value (edn/read-string v)
                  base-mem-address (bit-or (bit-and (mask 0) (bit-and (mask 2) mem-address))
                                           (bit-or  (mask 1) (bit-and (mask 2) mem-address)))]
              (->> (map power-of-2 (mask 3))
                   (reduce (fn [base offset] (mapcat #(vector % (+ % offset)) base)) [0])
                   (map (fn [offset] [(+ base-mem-address offset) value]))
                   (update state :mem into)))))]
  (map #(->> input (reduce % {:mem {}}) :mem vals (reduce +)) [f g]))
;; (17765746710228 4401465949086)


;; 202015
(let [input (->> (slurp "src/y2020/input202015") (re-seq #"\d+") (map edn/read-string))
      pos (reduce #(-> (update % :cnt inc)
                       (update %2 (fn [x] (if (nil? x) (list (:cnt %)) (take 2 (cons (:cnt %) x))))))
                  {:cnt 1}
                  input)
      f (fn [{:keys [pos n r]}]
          (let [x (if (and (pos r) (= 1 (count (pos r)))) 0 (reduce - (pos r)))
                n (inc n)]
            {:pos (update pos x #(if (nil? %) (list n) (take 2 (cons n %)))) :n n :r x}))
      g (fn [n]
          (-> (iterate f {:pos pos :r (last input) :n (count input)})
              (nth (- n (count input)))
              :r))]
  (mapv g [2020 30000000]))
;; [614 1065]


;; 202016
(let [parse
      (fn [{:keys [mode] :as state} line]
        (cond (re-seq #"your" line) (assoc state :mode :your)
              (re-seq #"nearby" line) (assoc state :mode :nearby)
              (= mode :fields) (let [[_ field-name n1 n2 n3 n4]
                                     (re-matches #"(.*): (\d+)-(\d+) or (\d+)-(\d+)" line)
                                     [n1 n2 n3 n4] (map edn/read-string [n1 n2 n3 n4])
                                     x [field-name (into #{} (concat (range n1 (inc n2)) (range n3 (inc n4))))]]
                                 (update state :fields conj x))
              (= mode :your)  (->> (re-seq #"\d+" line) (mapv edn/read-string) (assoc state :your))
              (= mode :nearby) (->> (re-seq #"\d+" line) (map edn/read-string) (update state :nearby conj))))
      f (fn [{:keys [fields nearby]}]
          (let [all-fields-set (reduce clojure.set/union (vals fields))]
            (->> (flatten nearby)
                 (remove all-fields-set)
                 (reduce +))))
      g (fn [{:keys [fields your nearby]}]
          (let [all-fields-set (reduce clojure.set/union (vals fields))
                valid-nearby (filter #(every? all-fields-set %) nearby)
                field-samples (apply map vector valid-nearby)]
            (loop [pending-nearby (mapcat (fn [[field range-set]]
                                            (map-indexed #(vector field % (every? range-set %2))
                                                         field-samples)) fields)
                   mapping {}]
              (if (seq pending-nearby)
                (let [new-mapping (into mapping
                                        (keep #(let [v (val %)]
                                                 (if (= 1 ((frequencies (flatten v)) true))
                                                   (->> v
                                                        (keep (fn [x] (if (= true (nth x 2)) [(x 0) (x 1)])))
                                                        first)))
                                              (group-by first pending-nearby)))
                      new-pending (remove #((set (vals new-mapping)) (% 1)) pending-nearby)]
                  (recur new-pending new-mapping))
                (reduce * (keep #(if (re-seq #"departure" (key %)) (your (val %))) mapping))))))
      input (->> (str/split (slurp "src/y2020/input202016") #"\n+")
                 (reduce parse {:fields {}, :your nil, :nearby [], :mode :fields}))]
  [(f input) (g input)])
;; [25895 5865723727753]


;; 202017
(let [input (slurp "src/y2020/input202017")
      input1 (str/split-lines input)
      max-x (count (first input1))
      cube3-map (->> (re-seq #"." input)
                     (keep-indexed (fn [i cell]
                                     (if (= cell "#")
                                       [(mod i max-x) (quot i max-x) 0]))))
      cube4-map (->> (re-seq #"." input)
                     (keep-indexed (fn [i cell]
                                     (if (= cell "#")
                                       [(mod i max-x) (quot i max-x) 0 0]))))
      offsets3 (for [x [-1 0 1], y [-1 0 1], z [-1 0 1] :when (not= 0 x y z)] [x y z])
      offsets4 (for [x [-1 0 1], y [-1 0 1], z [-1 0 1], w [-1 0 1] :when (not= 0 x y z w)] [x y z w])
      neighbor3 (fn [pos] (map #(map + pos %) offsets3))
      neighbor4 (fn [pos] (map #(map + pos %) offsets4))
      f (fn [neighbor-fn]
          (fn [s]
            (let [neighbors (->> (mapcat neighbor-fn s) frequencies)]
              (->> (keys neighbors)
                   (filter #(or (and (s %) (= 2 (neighbors %))) (= 3 (neighbors %))))
                   set))))]
  [(-> (iterate (f neighbor3) (set cube3-map)) (nth 6) count)
   (-> (iterate (f neighbor4) (set cube4-map)) (nth 6) count)])
;; [295 1972]


;; 202018
(let [input (->> (slurp "src/y2020/input202018") str/split-lines (map #(edn/read-string (str "(" % ")"))))
      f (fn f [xs]
          (let [new-xs (map #(if (list? %) (f %) %) xs)]
            (reduce (fn [r [op op1]] ((resolve op) r op1)) (first new-xs) (partition 2 (rest new-xs)))))
      g (fn g [xs]
          (->> (map #(if (list? %) (g %) %) xs)
               (partition-by #{'*})
               (remove #{'(*)})
               (map #(reduce + (remove #{'+} %)))
               (reduce *)))]
  (map #(reduce + (map % input)) [f g]))
;; (280014646144 9966990988262)


;; 202019
(let [parse (fn [[x & more]]
              (edn/read-string x))
      [rules messages] (-> (slurp "src/y2020/input202019") (str/split #"\n\n"))
      messages (str/split-lines messages)]
  (->> (re-seq #"\n|\d+|\||a|b" rules)
       (partition-by #(= "\n" %))
       (remove #(= 1 (count %))) #_
       (map parse)))


;; 202020
;; As "the outermost edges won't line up with any other tiles",
;; for q1 we just need to locate corner tiles with 2 edges that can't line up and multiple the ids.
;; Edge pattern can be read from L to R or from R to L when converted to interger,
;; and that should cover all rotating/flipped scenarios.
;; Non-corner edge tile has only 1 edge that can't line up.
;; And upon we inspecting the data, we can see that all edge patterns appear up to twice.
(let [n 10
      f (fn [v] (map (fn [v] (reduce #(+ (* 2 %) %2) v)) [v (reverse v)]))
      parse (fn [[tile & image]]
              (let [data (mapv {"." 0, "#" 1} image)
                    edges (mapcat f [(subvec data 0 n)
                                     (subvec data (* n (dec n)) (* n n))
                                     (take-nth n data)
                                     (take-nth n (subvec data (dec n)))])]
                [(edn/read-string tile) edges data]))
      input (->> (slurp "src/y2020/input202020")
                 (re-seq #"\d+|\.|\#")
                 (partition (inc (* n n))))
      q1-input (map parse input)
      edges (->> (mapcat second q1-input) frequencies (keep #(if (= 1 (val %)) (key %))) set)
      corner-ids (->> q1-input
                      (keep (fn [[id edge-vals]] (if (= 4 (count (filter edges edge-vals))) id))))
      q1 (reduce * corner-ids)
      corner-id (first corner-ids)]
  (->> q1-input
       (keep #(if (= corner-id (first %))
                (let [data (% 2)]
                  [(subvec data 0 n)                   ;; 0-9
                   (take-nth n (subvec data (dec n)))  ;; 9,19,...,99
                   (subvec data (* n (dec n)) (* n n)) ;; 90-99
                   (take-nth n data)]                  ;; 0,10,...,90
                  )))))

108603771107737


;; 202021
(let [input (->> (slurp "src/y2020/input202021")
                 (re-seq #".+")
                 (map #(let [[ingredients _ allergens] (->> (re-seq #"\w+" %) (partition-by #{"contains"}))]
                         [(set ingredients) (set allergens)])))
      all-ingredients (reduce clojure.set/union (map first input))
      all-allergens (reduce clojure.set/union (map second input))
      possible-allergen-ingredient-mapping
      (for [allergen all-allergens]
        [allergen (reduce clojure.set/intersection (keep #(if ((second %) allergen) (first %)) input))])
      ingredients-with-allergen (reduce clojure.set/union (map second possible-allergen-ingredient-mapping))
      q1 (reduce + (map #(count (clojure.set/difference (first %) ingredients-with-allergen)) input))
      q2 (loop [possible-allergen-ingredient-mapping possible-allergen-ingredient-mapping
                allergen-ingredient-mapping {}]
           (if (seq possible-allergen-ingredient-mapping)
             (let [found-mapping (keep (fn [[allergen ingredients]]
                                         (if (= 1 (count ingredients)) [allergen (first ingredients)]))
                                       possible-allergen-ingredient-mapping)
                   found-ingredients (set (map second found-mapping))
                   new-possible-allergen-ingredient-mapping
                   (keep (fn [[allergen ingredients]]
                           (let [new-ingredients (clojure.set/difference ingredients found-ingredients)]
                             (if (seq new-ingredients)
                               [allergen new-ingredients])))
                         possible-allergen-ingredient-mapping)]
               (recur new-possible-allergen-ingredient-mapping
                      (into allergen-ingredient-mapping found-mapping)))
             (->> (sort allergen-ingredient-mapping) (map second) (str/join ","))))]
  [q1 q2])
;; [1679 "lmxt,rggkbpj,mxf,gpxmf,nmtzlj,dlkxsxg,fvqg,dxzq"]


;; 202022
(let [[p1 p2] (->> (str/split (slurp "src/y2020/input202022") #"\n\n")
                   (map #(rest (map edn/read-string (re-seq #"\d+" %)))))
      score #(reduce + (map * % (range (count %) 0 -1)))
      q1 (loop [[head1 & p1' :as p1] p1
                [head2 & p2' :as p2] p2]
           (cond
             (nil? head1) p2
             (nil? head2) p1
             :else (if (< head1 head2)
                     (recur p1' (concat p2' [head2 head1]))
                     (recur (concat p1' [head1 head2]) p2'))))
      q2 (((fn f [p1 p2]
             (loop [[head1 & p1' :as p1] p1
                    [head2 & p2' :as p2] p2
                    seen #{}]
               (cond
                 (seen [p1 p2]) [:p1 p1]
                 (nil? head1)   [:p2 p2]
                 (nil? head2)   [:p1 p1]
                 (and (< head1 (count p1)) (< head2 (count p2)))
                 (let [[winner _] (f (take head1 p1') (take head2 p2'))]
                   (if (= :p2 winner)
                     (recur p1' (concat p2' [head2 head1]) (conj seen [p1 p2]))
                     (recur (concat p1' [head1 head2]) p2' (conj seen [p1 p2]))))
                 :else (if (< head1 head2)
                         (recur p1' (concat p2' [head2 head1]) (conj seen [p1 p2]))
                         (recur (concat p1' [head1 head2]) p2' (conj seen [p1 p2]))))))
           p1 p2) 1)]
  (map score [q1 q2]))
;; (32815 30695)


;; 202023
(let [xs (concat (->> (slurp "src/y2020/input202023") (re-seq #"\d") (map edn/read-string)) (range 10 10000001))]
  (->> xs
       (iterate
        (fn [[head a b c & more :as x]]
          (let [dest (->> (range 10000000 0 -1) cycle (drop-while #(not= head %)) rest (drop-while #{a b c}) first)]
            (concat (take-while #(not= % dest) more)
                    [dest a b c]
                    (rest (drop-while #(not= % dest) more))
                    [head]))))
       (drop 10000000)
       first
       cycle
       (drop-while #(not= 1 %))
       rest
       (take 2)))
(let [xs (->> (slurp "src/y2020/input202023") (re-seq #"\d") (map edn/read-string))]
  (->> xs
       (iterate
        (fn [[head a b c & more :as x]]
          (let [dest (->> (range 9 0 -1) cycle (drop-while #(not= head %)) rest (drop-while #{a b c}) first)]
            (concat (take-while #(not= % dest) more)
                    [dest a b c]
                    (rest (drop-while #(not= % dest) more))
                    [head]))))
       (drop 100)
       first
       cycle
       (drop-while #(not= 1 %))
       rest
       (take 8)
       (reduce #(+ (* 10 %) %2))))


;; 202024
;; hex grid coordinate system can be expressed in a pair of offset in [se, ne]
;; nw offsets se, sw offsets ne, e offsets w, and one unit of w can offset one unit of both se and ne
;; neighbors of [0 0] starting with neighbor in the east and continuing clockwise are:
;; [1 1] [1 0] [0 -1] [-1 -1] [-1 0] [0 1]
(let [input (->> (slurp "src/y2020/input202024") (re-seq #".+"))
      neighbors (fn [xy] (map #(map + xy %) [[1 1] [1 0] [0 -1] [-1 -1] [-1 0] [0 1]]))
      f (fn [s]
          (let [m (->> (re-seq #"se|sw|nw|ne|w|e" s) frequencies)
                [se sw nw ne w e] (map #(m % 0) ["se" "sw" "nw" "ne" "w" "e"])
                we (- w e)]
            [(- se nw we) (- ne sw we)]))
      g (fn [black-only-grid]
          (let [black-neighbor-count-map (frequencies (mapcat neighbors black-only-grid))
                new-black-only-grid (remove #(let [x (black-neighbor-count-map % 0)] (or (= 0 x) (< 2 x)))
                                            black-only-grid)
                y (set black-only-grid)
                white-flip-black (keep (fn [[neighbor n]] (if (and (= 2 n) (not (y neighbor))) neighbor))
                                       black-neighbor-count-map)]
            (concat new-black-only-grid white-flip-black)))
      black-only-grid (->> (map f input) frequencies (keep #(if (odd? (val %)) (key %))))]
  [(count black-only-grid)
   (-> (iterate g black-only-grid) (nth 100) count)])
;; [497 4156]


;; 202025
(let [[pub1 pub2] (->> "src/y2020/input202025" slurp (re-seq #"\d+") (map edn/read-string))
      f (fn [seed] (fn [x] (mod (* seed x) 20201227)))
      [loop-a loop-b] (loop [[n & more] (iterate (f 7) 1), a nil, b nil, cnt 0]
                        (cond (and a b) [a b]
                              (= n pub1) (recur more cnt b (inc cnt))
                              (= n pub2) (recur more a cnt (inc cnt))
                              :else (recur more a b (inc cnt))))]
  [(-> (iterate (f pub1) 1) (nth loop-b))
   (-> (iterate (f pub2) 1) (nth loop-a))])
;; [297257 297257]
