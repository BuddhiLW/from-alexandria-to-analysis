(ns alexandria.structure.probability
  "Domain stratum: a finite probability space, that is a measure of total
   mass 1 on a finite set of outcomes.

   ProbSpace  (outcomes S) the outcomes in order, (measure S event) the mass
              of an event (a predicate on outcomes), (expectation S f) the
              mean of f over the outcomes. Masses are exact rationals.

   Laws (test/alexandria/structure/probability_contract.clj checks them for
   every instance): the mass of the whole space is 1, masses are in [0, 1],
   a measure is additive on disjoint events and (so) the complement has mass
   1 - m, expectation is linear and the expectation of an indicator is the
   mass of the event. No random numbers are drawn here: a sample, if a page
   wants one, is the page's business and is marked as such.

   Instances
     FiniteSpace  an explicit pmf {outcome mass}; the fair coin, a die, the
                  2^n outcomes of Fermat's rounds (alexandria.pascal.points);
                  `bernoulli` p, `product` of two spaces, `pushforward`.
     Binomial     the n-fold product of a trial of success probability p = a/d,
                  seen through the number of successes k = 0..n (Jacob
                  Bernoulli, Ars conjectandi IV, 1713). Weights are the exact
                  integers C(n,k) a^k (d-a)^(n-k) out of d^n, so n of a few
                  thousand is exact too.

   A Lebesgue-style measure (Jordan 1892, Lebesgue 1904) mounts the same
   protocol on a space that is not finite."
  (:import (java.math BigInteger)))

(defprotocol ProbSpace
  (outcomes [S] "The outcomes, in order.")
  (measure [S event] "The mass of the outcomes satisfying the predicate event, an exact rational.")
  (expectation [S f] "The mean of f over the outcomes, an exact rational when f is."))

(defn mass "Mass of the whole space (1 for a probability space)." [S] (measure S (constantly true)))

(defn complement-mass "The mass of the outcomes that do not satisfy event." [S event] (measure S (complement event)))

;; ---------------------------------------------------------------------------
;; FiniteSpace

(defrecord FiniteSpace [pmf order]
  ProbSpace
  (outcomes [_] order)
  (measure [_ event] (reduce + 0N (for [o order :when (event o)] (get pmf o))))
  (expectation [_ f] (reduce + 0N (for [o order] (* (get pmf o) (f o))))))

(defn finite-space
  "The space with the given pmf (a map outcome -> nonnegative rational) and
   outcomes in the order of `order` (default: sorted by outcome when they
   compare, else map order). The masses are normalised to total 1."
  ([pmf] (finite-space pmf (try (vec (sort (keys pmf))) (catch Exception _ (vec (keys pmf))))))
  ([pmf order]
   (let [tot (reduce + 0N (vals pmf))]
     (assert (pos? tot) "a probability space has positive total weight")
     (->FiniteSpace (update-vals pmf #(/ (rationalize %) tot)) (vec order)))))

(defn uniform "Equal mass on each of the outcomes." [os] (finite-space (zipmap os (repeat 1)) os))

(defn bernoulli "One trial: outcome 1 (success) with mass p, 0 with mass 1 - p." [p]
  (finite-space {0 (- 1 (rationalize p)) 1 (rationalize p)} [0 1]))

(defn product
  "S x T with independent coordinates: outcomes [s t], mass m(s) m(t)."
  [S T]
  (let [os (vec (for [s (outcomes S) t (outcomes T)] [s t]))]
    (finite-space (into {} (for [[s t :as o] os] [o (* (measure S #{s}) (measure T #{t}))])) os)))

(defn pushforward
  "The image measure of S under f: mass of y is the mass of {x | f x = y}."
  [S f]
  (let [pmf (reduce (fn [m o] (update m (f o) (fnil + 0N) (measure S #{o}))) {} (outcomes S))]
    (finite-space pmf)))

(defn n-fold
  "n independent trials of S as vectors of outcomes (n >= 1)."
  [S n]
  (reduce (fn [acc _] (pushforward (product acc S) (fn [[v x]] (conj v x))))
          (pushforward S vector) (range (dec n))))

;; ---------------------------------------------------------------------------
;; Binomial

(defn- big [x] (BigInteger/valueOf (long x)))

(defn weights
  "The BigInteger weights w_k = C(n,k) a^k (d-a)^(n-k), k = 0..n, whose sum
   is d^n; exact, by the recurrence w_{k+1} (k+1)(d-a) = w_k (n-k) a."
  [n a d]
  (let [a (big a) b (big (- d a))]
    (loop [k 0 w (.pow b (int n)) acc (transient [])]
      (if (> k n)
        (persistent! acc)
        (recur (inc k)
               (if (= k n) w (.divide (.multiply w (.multiply (big (- n k)) a)) (.multiply (big (inc k)) b)))
               (conj! acc w))))))

(defrecord Binomial [n a d]
  ProbSpace
  (outcomes [_] (vec (range (inc n))))
  (measure [_ event]
    (let [ws (weights n a d)]
      (/ (reduce + 0N (for [k (range (inc n)) :when (event k)] (bigint (nth ws k))))
         (bigint (.pow (big d) (int n))))))
  (expectation [S f]
    (let [ws (weights n a d)]
      (/ (reduce + 0N (for [k (range (inc n))] (* (bigint (nth ws k)) (rationalize (f k)))))
         (bigint (.pow (big d) (int n)))))))

(defn binomial
  "The number of successes in n trials of success probability p = a/d, a
   rational strictly between 0 and 1."
  [n p]
  (let [p (rationalize p)]
    (assert (and (pos-int? n) (< 0 p 1)) "n >= 1 and 0 < p < 1")
    (->Binomial n (numerator p) (denominator p))))

(defn p-of "The success probability of a Binomial." [B] (/ (:a B) (:d B)))

(defn within
  "P(|S_n/n - p| <= eps): the exact mass of the band round p."
  [B eps]
  (let [n (:n B) p (p-of B) eps (rationalize eps)]
    (measure B (fn [k] (<= (abs (- (/ k n) p)) eps)))))

(defn chebyshev-bound
  "Chebyshev's lower bound 1 - p(1-p)/(n eps^2) for P(|S_n/n - p| <= eps)
   (clipped at 0): a bound Bernoulli did not have, shown for comparison."
  [n p eps]
  (let [p (rationalize p) eps (rationalize eps)]
    (max 0N (- 1 (/ (* p (- 1 p)) (* n eps eps))))))

(defn bernoulli-n
  "Bernoulli's number of trials (Ars conjectandi IV, the Scholium to the
   main proposition) for the ratio r : s of fertile to sterile cases,
   t = r + s, tolerance 1/t and odds c : 1, from the two lemmas on the
   largest term of (r+s)^n against the terms a distance nt/... away:
     m1 = ceil(ln(c (s-1)) / ln((r+1)/r)),  n1 = ceil(m1 t + s t (m1-1)/(r+1))
     m2 = ceil(ln(c (r-1)) / ln((s+1)/s)),  n2 = ceil(m2 t + r t (m2-1)/(s+1))
   and n = max(n1, n2). For r=30 s=20 c=1000 this gives 24728 and 25550."
  [r s c]
  (let [t (+ r s)
        m1 (long (Math/ceil (/ (Math/log (* c (dec s))) (Math/log (/ (inc r) (double r))))))
        m2 (long (Math/ceil (/ (Math/log (* c (dec r))) (Math/log (/ (inc s) (double s))))))
        n1 (long (Math/ceil (+ (* m1 t) (/ (* s t (dec m1)) (double (inc r))))))
        n2 (long (Math/ceil (+ (* m2 t) (/ (* r t (dec m2)) (double (inc s))))))]
    {:r r :s s :t t :c c :m1 m1 :m2 m2 :n1 n1 :n2 n2 :n (max n1 n2)}))
