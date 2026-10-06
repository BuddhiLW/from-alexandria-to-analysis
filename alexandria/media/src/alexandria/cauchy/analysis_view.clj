(ns alexandria.cauchy.analysis-view
  "Cauchy's analysis as proof players for a Clerk notebook: each function
   returns a Clerk value, the steps of alexandria.cauchy.analysis/proof
   played over alexandria.cauchy.analysis-scenes, with the numbers the
   scenes show computed here, on the JVM, by the core."
  (:require [alexandria.cauchy.analysis :as cauchy]
            [alexandria.medium.clerk :as medium]
            [alexandria.proofs :as proofs]
            [hive-dsl.result :as r]
            [alexandria.raster :as raster]
            [alexandria.raster.series :as rs]
            [emmy.env :as e]))

(def ^:private render-fn 'alexandria.cauchy.analysis-scenes/render)

(defn part
  "The proof data of part id (:cauchy/continuity, :cauchy/series,
   :cauchy/sum-theorem, :cauchy/integral, :cauchy/flat): :quotes :steps
   :source."
  [id]
  (let [res (cauchy/proof)]
    (when (r/ok? res) (get-in res [:ok id]))))

(defn- steps [id] (proofs/steps cauchy/proofs-resource id))

(defn- sample
  "[[x f(x)] ...] at n + 1 equal steps over [x0 x1], f an Emmy function
   evaluated by its raster kernel (alexandria.raster/sample)."
  [f x0 x1 n]
  (let [xs (mapv (fn [i] (+ x0 (* (- x1 x0) (/ i (double n))))) (range (inc n)))]
    (mapv vector xs (raster/sample f (mapv vector xs)))))

;; ---------------------------------------------------------------------------
;; The epsilon-delta game

(def game-epsilons [0.8 0.4 0.2 0.1 0.05 0.025])

(defn continuity-data
  "The game on f(x) = x^2 at a = 1, with Cauchy's closed-form delta for
   each eps (exact, then shown as a double), and a step function for the
   jump."
  []
  (let [f e/square
        a 1.0
        delta (get-in cauchy/games [:square :delta])]
    {:curve (sample f -0.3 2.0 240) :a a :fa (raster/value f a)
     :rounds (mapv (fn [eps] {:eps eps :delta (raster/value (fn [a eps] (delta a eps)) a eps)}) game-epsilons)
     :x [-0.4 2.1] :y [-0.4 3.6] :label "f(x) = x^2, a = 1"
     :jump {:curve-l (sample (constantly 1.0) -0.3 1.0 2)
            :curve-r (sample (constantly 2.2) 1.0 2.0 120)
            :a 1.0 :fa 1.0}}))

(defn continuity []
  (medium/player render-fn :cauchy/continuity (steps :cauchy/continuity) {} (continuity-data)
                 {:window [[-0.5 2.2] [-0.55 3.7]] :height 400
                  :durations {:graph 3500 :window 5000 :shrink 9000 :jump 7000}}))

;; ---------------------------------------------------------------------------
;; Series

(defn series-data
  "The partial sums of 1/n! and of the harmonic series are exact rationals
   (shown as doubles); e is exp(1) by a raster kernel."
  []
  {:sums (cauchy/e-partials 12)
   :limit (raster/value e/exp 1)
   :harmonic (mapv (fn [n] [n (double (reduce + (map #(/ 1 %) (range 1 (inc n)))))]) (range 1 17))})

(defn series []
  (medium/player render-fn :cauchy/series (steps :cauchy/series) {} (series-data)
                 {:window [[-0.3 9.5] [-1.0 4.2]] :height 360
                  :durations {:partials 6000 :tail-band 8000 :ratio 5000 :harmonic 8000}}))

;; ---------------------------------------------------------------------------
;; The sum theorem

(defn sum-theorem-data []
  {:sawtooth [[[(- Math/PI) (- (/ Math/PI 2))] [Math/PI (/ Math/PI 2)]]]
   :si-pi cauchy/si-pi})

(defn sum-theorem
  "Abel's series. The slider sets n for the last stage, up to the kernel's
   cauchy/abel-terms: whatever n, the error at x = pi - 1/n stays about
   0.6 (local state; the curve is the :abel raster kernel, re-evaluated
   per frame, never re-mounted)."
  []
  (medium/player render-fn :cauchy/sum-theorem (steps :cauchy/sum-theorem)
                 cauchy/figures (sum-theorem-data)
                 {:window [[-3.4 3.6] [-2.35 2.6]] :height 400
                  :durations {:terms 5000 :partial-sums 9000 :jump 7000 :gibbs 9000 :uniform 9000}
                  :controls [{:id :n :label "terms n (last stage)" :min 1 :max cauchy/abel-terms :step 1 :init 10}]}))

;; ---------------------------------------------------------------------------
;; The definite integral

(defn integral-data
  "Cauchy's sum for f(x) = 1 + sin(x) on [0, 3], at n = 4, 8, ..., 128.
   Every number is raster's: the curve and the rectangle heights by f's
   kernel, I by raster quadrature, the mean-value point by raster's Brent,
   each S by a raster loop kernel."
  []
  (let [f (fn [x] (e/+ 1 (e/sin x)))
        x0 0.0 X 3.0
        I (:value (raster/integral f x0 X))
        mean-h (/ I (- X x0))]
    {:curve (sample f x0 X 200)
     :label "f(x) = 1 + sin x"
     :integral I
     :mean (:value (raster/root (fn [x] (e/- (f x) mean-h)) x0 (/ Math/PI 2)))
     :levels (mapv (fn [n] (let [h (/ (- X x0) n)
                                 xs (mapv #(+ x0 (* % h)) (range n))
                                 rs (mapv (fn [a y] [a (+ a h) y]) xs (raster/sample f (mapv vector xs)))]
                             {:n n :rects rs :S (:value (rs/left-sum f x0 X n))}))
                   [4 8 16 32 64 128])}))

(defn integral []
  (medium/player render-fn :cauchy/integral (steps :cauchy/integral) {} (integral-data)
                 {:window [[-0.3 3.4] [-0.5 2.85]] :height 380
                  :durations {:rectangles 5000 :refine 10000 :mean 6000 :fundamental 8000}}))

;; ---------------------------------------------------------------------------
;; The flat function

(def flat-zooms [1 0.5 0.3 0.2])

(defn flat-data
  "exp(-1/x^2) over [-z z] mapped onto [-3 3], the height magnified by
   (1/z)^4 (and by 1.2 to sit under the dashed line y = 1); sampled by a
   raster kernel of the Emmy expression, the point x = 0 (where it is 0)
   set apart."
  []
  {:zooms flat-zooms
   :curves (mapv (fn [z]
                   (let [us (mapv (fn [i] (- (* 6.0 (/ i 300)) 3)) (range 301))
                         g (fn [u] (e/* 1.2 (e/expt (/ 1.0 z) 4) (cauchy/flat (e/* z (e// u 3)))))
                         ys (raster/sample g (mapv vector (remove zero? us)))
                         ys (into (subvec ys 0 150) (cons 0.0 (subvec ys 150)))]
                     (mapv vector us ys)))
                 (cons 3 (rest flat-zooms)))})

(defn flat []
  (medium/player render-fn :cauchy/flat (steps :cauchy/flat) {} (flat-data)
                 {:window [[-3.1 3.1] [-0.3 2.1]] :height 300
                  :durations {:zoom 8000}}))

;; ---------------------------------------------------------------------------
;; Note III: the root search by tenths

(defn ivt-data
  "Six rounds of Cauchy's search on x^3 - 2x - 5 from 2 to 3: the bounds
   exact, the eleven values of each round and its curve sampled by the
   equation's raster kernel, the root by raster's Brent."
  []
  (let [f cauchy/ivt-example
        rounds (cauchy/tenths-search f 2 3 10 6)]
    {:label "x^3 - 2x - 5 = 0, x0 = 2, X = 3, m = 10"
     :root (:value (raster/root f 2 3))
     :rounds (mapv (fn [{:keys [x X xs values j]}]
                     (let [curve (sample f (double x) (double X) 120)
                           scale (reduce max 1e-300 (map #(Math/abs (double %)) (concat values (map second curve))))]
                       {:x (double x) :X (double X) :xs (mapv double xs) :values values :j j
                        :curve curve :scale scale}))
                   rounds)}))

(defn ivt []
  (medium/player render-fn :cauchy/ivt (steps :cauchy/ivt) {} (ivt-data)
                 {:window [[-0.3 5.3] [-1.9 2.6]] :height 360
                  :durations {:divide 5000 :pair 4000 :refine 12000 :limit 7000 :scholie 6000}}))

;; ---------------------------------------------------------------------------
;; Lesson 3: the ratio of differences

(defn derivative-data
  "sin x near x = 1: the curve, and the chord for i = 1, 1/2, ..., 1/64,
   each f(x + i) and quotient by raster kernels."
  []
  (let [x 1.0
        is (mapv #(/ 1.0 (Math/pow 2 %)) (range 7))]
    {:curve (sample e/sin -0.1 3.2 200) :x x :fx (raster/value e/sin x)
     :slope (raster/value e/cos x) :label "y = sin x at x = 1"
     :chords (mapv (fn [i] {:i i :fxi (raster/value e/sin (+ x i))
                            :q (raster/value (fn [i] (cauchy/difference-quotient e/sin x i)) i)})
                   is)}))

(defn derivative []
  (medium/player render-fn :cauchy/derivative (steps :cauchy/derivative) {} (derivative-data)
                 {:window [[-0.3 3.3] [-0.4 2.1]] :height 340
                  :durations {:chord 4000 :shrink 9000 :power 6000 :derived 6000}}))

;; ---------------------------------------------------------------------------
;; Chapter VI, Theorem 1

(defn root-test-data
  "u_n = n^2/2^n against U^n, U = 3/4, as log10 (of exact rationals);
   (u_n)^(1/n) by a raster kernel at n = 1 .. 10^4."
  []
  (let [ln2 (raster/value e/log 2)
        lg10 (fn [q] (let [[a b] (if (ratio? q) [(.numerator q) (.denominator q)] [q 1])] (/ (- (Math/log (double a)) (Math/log (double b))) (Math/log 10))))
        root (fn [n] (raster/value (fn [n c] (e/exp (e// (e/- (e/* 2 (e/log n)) (e/* n c)) n))) n ln2))]
    {:k 0.5 :U 0.75 :N 13
     :k-roots (mapv (fn [j] (let [n (Math/pow 10 (/ j 10.0))] [(/ j 10.0) (root n)])) (range 0 41))
     :terms (mapv (fn [n] [n (lg10 (cauchy/root-test-term n)) (lg10 (cauchy/expt-rational 3/4 n))]) (range 1 31))}))

(defn root-test []
  (medium/player render-fn :cauchy/root-test (steps :cauchy/root-test) {} (root-test-data)
                 {:window [[-0.2 6.4] [-0.3 4.0]] :height 340
                  :durations {:roots 6000 :choose-u 5000 :compare 7000}}))

;; ---------------------------------------------------------------------------
;; Lesson 21 (3)-(7) and Lesson 26

(defn refinement []
  (medium/player render-fn :cauchy/refinement (steps :cauchy/refinement) {} (integral-data)
                 {:window [[-0.3 3.4] [-0.5 2.85]] :height 380
                  :durations {:mean 6000 :subdivide 5000 :errors 9000 :third 10000}}))

(defn fundamental-data
  "integral-data, plus the strip of equation (3) at x = 1, alpha = 1/2 and
   the quotients for alpha = 1/2 .. 1/1000: F by raster quadrature, theta
   by raster's Brent."
  []
  (let [f cauchy/cauchy-integrand
        x 1.0
        row (fn [alpha]
              (let [q (/ (- (cauchy/integral-function (+ x alpha)) (cauchy/integral-function x)) alpha)
                    xt (:value (raster/root (fn [t] (e/- (f t) q)) x (+ x alpha)))]
                {:alpha alpha :quotient q :theta (/ (- xt x) alpha) :xt xt :ft q}))]
    (assoc (integral-data)
           :fx (raster/value f x)
           :strip (assoc (row 0.5) :x x)
           :quotients (mapv row [0.5 0.1 0.01 0.001]))))

(defn fundamental []
  (medium/player render-fn :cauchy/fundamental (steps :cauchy/fundamental) {} (fundamental-data)
                 {:window [[-0.3 3.4] [-0.5 2.85]] :height 380
                  :durations {:area 6000 :increment 6000 :divide 8000}}))
