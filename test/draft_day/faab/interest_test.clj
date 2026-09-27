(ns draft-day.faab.interest-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [draft-day.faab.interest :as interest]
            [draft-day.faab.replay :as replay]))

(defn- near? [a b tolerance] (< (Math/abs (- (double a) (double b))) tolerance))

(deftest lgamma-is-the-log-of-the-factorial-one-down
  (is (near? (Math/log 24.0) (interest/lgamma 5.0) 1e-10))
  (is (near? (* 0.5 (Math/log Math/PI)) (interest/lgamma 0.5) 1e-10))
  (is (near? 0.0 (interest/lgamma 1.0) 1e-10))
  (testing "below a half, where the fit's shapes live, by reflection"
    (is (near? (Math/log 9.513507698668732) (interest/lgamma 0.1) 1e-10))
    (is (near? (- (+ (Math/log 1e-4) (* 0.5772156649015329 1e-4))) (interest/lgamma 1e-4) 1e-8)
        "Γ(x) ≈ 1/x - γ near zero")
    (is (near? (+ (interest/lgamma 0.3) (Math/log 0.3)) (interest/lgamma 1.3) 1e-10)
        "Γ(x + 1) = x Γ(x) across the branch")))

(deftest the-count-likelihood-is-a-negative-binomial-of-the-right-moments
  (let [pmf  (fn [spread r n] (Math/exp (interest/nb-ll spread [{:rate r :bidders n}])))
        mom  (fn [spread r]
               (let [ps (map #(pmf spread r %) (range 400))]
                 [(reduce + ps)
                  (reduce + (map-indexed * ps))
                  (reduce + (map-indexed #(* %1 %1 %2) ps))]))]
    (doseq [spread [0.2 1.43 5.0] r [0.05 0.7 3.0]]
      (let [[total m m2] (mom spread r)]
        (is (near? 1.0 total 1e-9) "a distribution")
        (is (near? r m 1e-8) "whose mean is the rate")
        (is (near? (* r (+ 1.0 spread)) (- m2 (* m m)) 1e-6) "and whose variance is r(1 + spread)")))
    (is (near? (- (+ (* 2 (Math/log 0.7)) -0.7) (Math/log 2.0)) (interest/nb-ll 1e-9 [{:rate 0.7 :bidders 2}]) 1e-6)
        "no spread at all is Poisson")))

(deftest solve-is-a-linear-solve-at-any-size
  (let [rng (java.util.Random. 9)
        a   (vec (repeatedly 5 (fn [] (vec (repeatedly 5 #(.nextGaussian rng))))))
        b   (vec (repeatedly 5 #(.nextGaussian rng)))
        x   (interest/solve a b)]
    (is (every? true? (map (fn [row bi] (near? bi (reduce + (map * row x)) 1e-9)) a b)))))

(deftest solve-is-a-linear-solve
  (is (every? true? (map #(near? %1 %2 1e-12) [1.0 -2.0]
                         (interest/solve [[0.0 2.0] [3.0 1.0]] [-4.0 1.0])))
      "a zero on the diagonal wants the pivot"))

(defn- simulated
  "League-weeks where each rival picks one of `n` free agents with chance
  proportional to e^(a·x + b·y), `:a` shared and `:b` his own."
  [a b weeks n seed]
  (let [rng (java.util.Random. seed)
        pick (fn [us]
               (let [ws (mapv #(Math/exp %) us) t (reduce + ws) u (* t (.nextDouble rng))]
                 (loop [j 0 acc 0.0]
                   (let [acc (+ acc (ws j))] (if (or (>= acc u) (= j (dec n))) j (recur (inc j) acc))))))]
    (vec (repeatedly weeks
                     (fn []
                       (let [features (vec (repeatedly n #(hash-map :a (.nextGaussian rng))))]
                         {:league   "L"
                          :features features
                          :rivals   (vec (repeatedly 4
                                                     (fn []
                                                       (let [needs (vec (repeatedly n #(hash-map :b (if (.nextBoolean rng) 1.0 0.0))))]
                                                         {:needs  needs
                                                          :chosen [(pick (mapv #(+ (* a (:a %1)) (* b (:b %2))) features needs))]}))))}))))))

(defn- compact [weeks] (mapv #(interest/compact-week % [:a] :b) weeks))

(deftest the-fit-recovers-the-weights-that-made-the-choices
  (let [sets (interest/choice-sets (compact (simulated 1.2 -0.7 400 30 7)))
        {:keys [weights se]} (interest/fit [:a :b] sets)]
    (is (near? 1.2 (:a weights) (* 3 (:se se 0.1))) (str "a " weights " ± " se))
    (is (near? -0.7 (:b weights) 0.25) (str "b " weights))
    (is (< (:a se) 0.1) "sixteen hundred choices pin a weight down")))

(deftest a-family-left-out-stays-at-zero
  (let [sets (interest/choice-sets (compact (simulated 1.2 -0.7 200 30 8)))
        {:keys [weights se]} (interest/fit [:a :b] sets #{:a})]
    (is (= 0.0 (:b weights)))
    (is (not (contains? se :b)))
    (is (near? 1.2 (:a weights) 0.3) "and the rest is fit without it")))

(deftest a-choice-scores-against-the-uniform-guess
  (let [weeks (compact [{:features [{:a 0.0} {:a 0.0} {:a 0.0} {:a 0.0}]
                         :rivals   [{:needs [{} {} {} {}] :chosen [2]} {:needs [{} {} {} {}] :chosen []}]}])
        {:keys [model uniform choices]} (interest/per-choice {:a 1.0} [:a :b] (interest/choice-sets weeks))]
    (is (= 1 choices) "a rival who chose nobody is no choice")
    (is (near? (- (Math/log 4.0)) uniform 1e-12))
    (is (near? uniform model 1e-12) "features that do not vary say nothing")))

(deftest choices-survive-the-disk
  (let [weeks (compact (map-indexed #(assoc %2 :week (inc %1)) (simulated 1.0 1.0 3 5 9)))
        path  (str (System/getProperty "java.io.tmpdir") "/choices-" (random-uuid) ".bin")
        back  (do (interest/write-choices! path weeks) (interest/read-choices path))
        shape (fn [ws] (mapv (fn [{:keys [week x rivals]}]
                               [week (mapv vec x) (mapv (fn [r] (update r :extra vec)) rivals)])
                             ws))]
    (io/delete-file path)
    (is (= (shape weeks) (shape back)))))

(deftest a-players-rate-is-every-rivals-share-of-his-claims
  (let [weeks (compact [{:features [{:a 0.0} {:a 0.0}]
                         :rivals   [{:needs [{:b 1.0} {}] :chosen [0] :per-week 2.0}
                                    {:needs [{} {}] :chosen [] :per-week 1.0}]}])
        [p q] (interest/player-weeks weeks {:b (Math/log 3.0)} [:a :b])]
    (is (near? (+ (* 2.0 0.75) 0.5) (:rate p) 1e-12) "three to one his way for the first, even for the second")
    (is (near? (+ (* 2.0 0.25) 0.5) (:rate q) 1e-12))
    (is (= [1 0] [(:bidders p) (:bidders q)]))))

(deftest the-spread-tells-piled-up-bids-from-scattered-ones
  (let [scattered (concat (repeat 600 {:rate 0.5 :bidders 0}) (repeat 300 {:rate 0.5 :bidders 1})
                          (repeat 75 {:rate 0.5 :bidders 2}) (repeat 13 {:rate 0.5 :bidders 3}))
        piled     (concat (repeat 900 {:rate 0.5 :bidders 0}) (repeat 40 {:rate 0.5 :bidders 5})
                          (repeat 20 {:rate 0.5 :bidders 7}) (repeat 10 {:rate 0.5 :bidders 2}))
        s         (interest/fit-spread scattered)
        p         (interest/fit-spread piled)]
    (is (< s 0.05) (str "about Poisson: next to no spread, " s))
    (is (> p 3.0) (str "a few players drawing everybody: a wide one, " p))
    (is (>= (interest/nb-ll p piled) (interest/nb-ll (* 1.2 p) piled)))
    (is (>= (interest/nb-ll p piled) (interest/nb-ll (/ p 1.2) piled)) "and it is the maximum")))

(defn- a-set
  "A choice set of `rows` feature vectors with `chosen` indices, its last
  column as the byte `:extra` when `extra?`."
  ([rows chosen] (a-set rows chosen false))
  ([rows chosen extra?]
   (if extra?
     {:x      (into-array (map #(float-array (butlast %)) rows))
      :extra  (byte-array (map #(byte (last %)) rows))
      :chosen chosen}
     {:x (into-array (map float-array rows)) :chosen chosen})))

(deftest set-stats-scores-the-choice-as-a-softmax-does
  (doseq [extra? [false true]]
    (let [s     (a-set [[1.0 0.0] [0.0 1.0] [0.5 1.0]] [0] extra?)
          theta (double-array [0.4 -0.3])
          us    [0.4 -0.3 -0.1]
          [ll]  (interest/set-stats theta s)]
      (is (near? (- (first us) (Math/log (reduce + (map #(Math/exp %) us)))) ll 1e-7)
          (str "the last column " (if extra? "as a byte" "as a float"))))))

(defn- numeric-grad
  "Each weight's partial derivative of `f` by central difference."
  [f theta h]
  (mapv (fn [i]
          (let [bump (fn [d] (f (double-array (update (vec theta) i + d))))]
            (/ (- (bump h) (bump (- h))) (* 2 h))))
        (range (count theta))))

(deftest set-stats-derivatives-agree-with-finite-differences
  (let [rng  (java.util.Random. 11)
        rows (vec (repeatedly 6 (fn [] (conj (vec (repeatedly 2 #(.nextGaussian rng)))
                                             (if (.nextBoolean rng) 1.0 0.0)))))
        h    1e-5]
    (doseq [extra? [false true]
            chosen [[2] [0 4 4]]
            theta  (repeatedly 3 (fn [] (vec (repeatedly 3 #(.nextGaussian rng)))))]
      (let [s          (a-set rows chosen extra?)
            [_ g hess] (interest/set-stats (double-array theta) s)
            ll         #(first (interest/set-stats % s))
            grad       #(vec (second (interest/set-stats % s)))]
        (is (every? true? (map #(near? %1 %2 1e-6) (vec g) (numeric-grad ll theta h)))
            (str "gradient, chosen " chosen))
        (is (near? (ll (double-array theta)) (interest/set-ll (double-array theta) s) 1e-12)
            "the line search's likelihood is the same one")
        (doseq [i (range 3)]
          (is (every? true? (map #(near? %1 %2 1e-5)
                                 (vec (aget ^objects hess i))
                                 (numeric-grad #(nth (grad %) i) theta h)))
              (str "Hessian row " i ", chosen " chosen)))))))

(deftest leagues-extracted-at-once-keep-their-own-weeks
  (with-redefs [replay/replay               (fn [id _]
                                              (dotimes [w 6]
                                                (Thread/sleep (long (rand-int 3)))
                                                (replay/who-bids-rows id nil #{} w)))
                interest/week-choices       (fn [board _ _ w] {:week w :from board})
                interest/write-choices!     (fn [& _])
                interest/choices-dir        (str (System/getProperty "java.io.tmpdir") "/no-such-" (random-uuid))]
    (let [ids  (map str (range 40))
          data (interest/load-leagues {} (map #(hash-map :league-id %) ids))]
      (is (= (set ids) (set (keys data))))
      (is (every? (fn [[id weeks]] (and (= 6 (count weeks)) (every? #(= id (:from %)) weeks))) data)
          "the replay's hook is shared by every thread; what it captures must not be"))))
