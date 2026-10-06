(ns alexandria.lebesgue.measure
  "Lebesgue, Integrale, longueur, aire (1902): measure zero and Dirichlet's
   function, in exact arithmetic.

     cover       the n-th rational of [0, 1] (Cantor's count) in an interval
                 of length eps/2^n; the lengths sum to eps (1 - 2^-N)
     dirichlet   chi = 1 on the rationals, 0 elsewhere; Darboux's sums by
                 excess and by defect for any division (Riemann: 1 and 0),
                 Lebesgue's sum over the values (0)
     figures     the covering intervals as eps shrinks
     identities  the geometric sums, graded by Emmy

   Reuse searched: alexandria.measure is geodesic-triangle area (Gauss-Bonnet),
   not set measure; Emmy has quadrature (emmy.numerical.quadrature) but no
   set measure or Lebesgue sum; desargues nothing. The rationals come from
   alexandria.cantor.sets/zig-zag. Emmy proves the closed forms with q = 2^-N
   as a symbol (its simplifier does not merge 2^-(N+1) with 2^-N)."
  (:require [alexandria.cantor.sets :as sets]
            [alexandria.grade :as grade]
            [alexandria.proofs :as proofs]
            [emmy.env :as e]
            [hive-dsl.result :as r]))

;; ---------------------------------------------------------------------------
;; The rationals of [0, 1], counted

(defn rationals-01
  "The rationals of [0, 1] in Cantor's order: 0, 1, then the zig-zag below 1."
  []
  (concat [0 1] (filter #(< % 1) (sets/zig-zag))))

;; ---------------------------------------------------------------------------
;; The cover

(defn cover
  "The first N covering intervals for eps: the n-th rational r (n from 1) in
   [r - eps/2^(n+1), r + eps/2^(n+1)], length eps/2^n.
   [{:n :r :interval [lo hi] :length}]."
  [eps N]
  (mapv (fn [n r]
          (let [half (/ eps (* 2 (bigint (.pow (biginteger 2) (int n)))))]
            {:n n :r r :interval [(- r half) (+ r half)] :length (* 2 half)}))
        (range 1 (inc N)) (rationals-01)))

(defn total-length [intervals] (reduce + (map :length intervals)))

(defn union-length
  "The length of the union of intervals (overlaps counted once), exact."
  [intervals]
  (let [ivs (sort-by first (map :interval intervals))]
    (first
     (reduce (fn [[acc [lo hi]] [a b]]
               (cond (nil? lo) [acc [a b]]
                     (<= a hi) [acc [lo (max hi b)]]
                     :else [(+ acc (- hi lo)) [a b]]))
             [0 [nil nil]]
             (concat ivs [[##Inf ##Inf]])))))

;; ---------------------------------------------------------------------------
;; Dirichlet's function

(defn chi
  "Dirichlet's function on a number given exactly: 1 for a rational
   (a Clojure integer or ratio), 0 for :irrational."
  [x]
  (if (rational? x) 1 0))

(defn darboux-sums
  "Darboux's sums of chi for the division of [0, 1] by the points xs (exact
   rationals, 0 and 1 included): every interval holds a rational (its
   midpoint) and an irrational (its point at width/sqrt 2 from the left),
   so sup = 1 and inf = 0 on each. {:S :s}."
  [xs]
  (let [ivs (partition 2 1 xs)]
    {:S (reduce + (map (fn [[a b]] (* (- b a) (chi (/ (+ a b) 2)))) ivs))
     :s (reduce + (map (fn [[a b]] (* (- b a) (chi :irrational))) ivs))}))

(defn lebesgue-sum
  "Lebesgue's sum: each value of chi times the measure of the set where
   chi takes it. m(rationals) = 0 (the cover), m(irrationals) = 1 - 0."
  []
  (let [m-rat 0 m-irr (- 1 m-rat)]
    (+ (* 1 m-rat) (* 0 m-irr))))

;; ---------------------------------------------------------------------------
;; Figures

(defn covering
  "End e (0 left, 1 right) of the covering interval of the n-th rational at
   eps, drawn at height n (state [n e]); xn is that rational's position.
   Emmy function of the scene's eps: the interval eps/2^n about xn."
  [eps]
  (fn [[xn n e]]
    (let [half (e// eps (e/expt 2 (e/+ n 1)))]
      [(e/+ xn (e/* (e/- (e/* 2 e) 1) half)) (e/* -0.05 n)])))

(def figures
  "Every moving figure, by name: {:f :params :state}."
  {:cover {:f covering :params [0.5] :state [0.5 1 0]}})

;; ---------------------------------------------------------------------------
;; Grades

(def identities
  "[label difference] pairs, each zero, with q = 2^-N and r the ratio of a
   geometric series."
  (let [eps 'epsilon q 'q rr 'r]
    [["a geometric series: a + a r + a r^2 + ... = a / (1 - r), its partial sum a (1 - r^N)/(1 - r) times (1 - r) telescopes to a (1 - r^N)"
      (e/- (e/* (e/- 1 rr) (e/+ 1 rr (e/square rr) (e/expt rr 3))) (e/- 1 (e/expt rr 4)))]
     ["the whole cover: eps/2 + eps/4 + ... = (eps/2) / (1 - 1/2) = eps"
      (e/- (e// (e// eps 2) (e/- 1 1/2)) eps)]
     ["the first N lengths sum to eps (1 - q), q = 2^-N: one more term gives eps (1 - q) + eps q/2 = eps (1 - q/2)"
      (e/- (e/+ (e/* eps (e/- 1 q)) (e// (e/* eps q) 2)) (e/* eps (e/- 1 (e// q 2))))]
     ["the lengths left out after N: eps - eps (1 - q) = eps q, which goes to 0"
      (e/- (e/- eps (e/* eps (e/- 1 q))) (e/* eps q))]
     ["Lebesgue's sum for chi: 1 m(Q) + 0 m([0,1] - Q) with m(Q) = 0"
      (e/- (e/+ (e/* 1 0) (e/* 0 (e/- 1 0))) 0)]]))

(defn- variant [res] (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails))

(defn graded
  "[{:label :grade}]: Emmy's identities, then the exact finite checks."
  []
  (let [eps 1/10]
    (into (mapv (fn [[label diff]] {:label label :grade (variant (grade/grade :symbolic [diff]))})
                identities)
          (map (fn [[label ok?]] {:label label :grade (if ok? :grade/proved :grade/fails)}))
          [["for eps = 1/10 the first 40 intervals have total length exactly (1/10)(1 - 2^-40)"
            (= (total-length (cover eps 40)) (* eps (- 1 (/ 1 (.pow (biginteger 2) 40)))))]
           ["their union is shorter still (overlaps count once)"
            (<= (union-length (cover eps 40)) (total-length (cover eps 40)))]
           ["each of the first 40 rationals of [0, 1] lies inside its interval"
            (every? (fn [{:keys [r] [lo hi] :interval}] (< lo r hi)) (cover eps 40))]
           ["Riemann: S = 1 and s = 0 for the division of [0, 1] into 1, 2, ..., 50 equal parts"
            (every? (fn [k] (= {:S 1 :s 0} (darboux-sums (map #(/ % k) (range (inc k))))))
                    (range 1 51))]
           ["Riemann: S = 1 and s = 0 for the unequal division by the first 30 zig-zag rationals"
            (= {:S 1 :s 0} (darboux-sums (sort (distinct (concat [0 1] (take 30 (rationals-01)))))))]
           ["Lebesgue: the integral of chi is 0" (zero? (lebesgue-sum))]])))

;; ---------------------------------------------------------------------------
;; The proofs as data

(def proofs-resource "alexandria/lebesgue/integrale.proofs.edn")

(defn proof [] (proofs/read-proofs proofs-resource))
