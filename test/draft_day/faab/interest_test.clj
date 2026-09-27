(ns draft-day.faab.interest-test
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.faab.interest :as interest]))

(defn- near? [a b tolerance] (< (Math/abs (- (double a) (double b))) tolerance))

(deftest lgamma-is-the-log-of-the-factorial-one-down
  (is (near? (Math/log 24.0) (interest/lgamma 5.0) 1e-10))
  (is (near? (* 0.5 (Math/log Math/PI)) (interest/lgamma 0.5) 1e-10))
  (is (near? 0.0 (interest/lgamma 1.0) 1e-10)))

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

(deftest the-fit-recovers-the-weights-that-made-the-choices
  (let [data (simulated 1.2 -0.7 400 30 7)
        ks   (interest/feature-keys data)
        {:keys [weights se]} (interest/fit ks (interest/choice-sets data ks))]
    (is (= [:a :b] ks))
    (is (near? 1.2 (:a weights) (* 3 (:se se 0.1))) (str "a " weights " ± " se))
    (is (near? -0.7 (:b weights) 0.25) (str "b " weights))
    (is (< (:a se) 0.1) "sixteen hundred choices pin a weight down")))

(deftest a-choice-scores-against-the-uniform-guess
  (let [data [{:features [{:a 0.0} {:a 0.0} {:a 0.0} {:a 0.0}]
               :rivals   [{:needs [{} {} {} {}] :chosen [2]} {:needs [{} {} {} {}] :chosen []}]}]
        {:keys [model uniform choices]} (interest/per-choice {:a 1.0} [:a] (interest/choice-sets data [:a]))]
    (is (= 1 choices) "a rival who chose nobody is no choice")
    (is (near? (- (Math/log 4.0)) uniform 1e-12))
    (is (near? uniform model 1e-12) "features that do not vary say nothing")))

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
  "A choice set of `rows` feature vectors with `chosen` indices."
  [rows chosen]
  {:x (into-array (map double-array rows)) :chosen chosen})

(deftest set-stats-scores-the-choice-as-a-softmax-does
  (let [s     (a-set [[1.0 0.0] [0.0 1.0] [0.5 0.5]] [0])
        theta (double-array [0.4 -0.3])
        us    [0.4 -0.3 0.05]
        [ll]  (interest/set-stats theta s)]
    (is (near? (- (first us) (Math/log (reduce + (map #(Math/exp %) us)))) ll 1e-12))))

(defn- numeric-grad
  "Each weight's partial derivative of `f` by central difference."
  [f theta h]
  (mapv (fn [i]
          (let [bump (fn [d] (f (double-array (update (vec theta) i + d))))]
            (/ (- (bump h) (bump (- h))) (* 2 h))))
        (range (count theta))))

(deftest set-stats-derivatives-agree-with-finite-differences
  (let [rng  (java.util.Random. 11)
        rows (vec (repeatedly 6 (fn [] (vec (repeatedly 3 #(.nextGaussian rng))))))
        h    1e-5]
    (doseq [chosen [[2] [0 4 4]]
            theta  (repeatedly 3 (fn [] (vec (repeatedly 3 #(.nextGaussian rng)))))]
      (let [s          (a-set rows chosen)
            [_ g hess] (interest/set-stats (double-array theta) s)
            ll         #(first (interest/set-stats % s))
            grad       #(vec (second (interest/set-stats % s)))]
        (is (every? true? (map #(near? %1 %2 1e-6) (vec g) (numeric-grad ll theta h)))
            (str "gradient, chosen " chosen))
        (doseq [i (range 3)]
          (is (every? true? (map #(near? %1 %2 1e-5)
                                 (vec (aget ^objects hess i))
                                 (numeric-grad #(nth (grad %) i) theta h)))
              (str "Hessian row " i ", chosen " chosen)))))))
