(ns alexandria.raster.series
  "Sums of many terms as raster kernels: the partial sums of a series and
   the tagged sums of an integral run as one compiled loop, not as a
   Clojure reduction over doubles. Emmy writes the term; raster sums it.

     sum        sum_{k = k0 .. k1} s^(k - k0) term(k, p..), s = 1 or -1 (an
                alternating series), the term an Emmy function of the index
                k and the params p.. (numbers), lowered once to raster's
                vocabulary and compiled into a loop kernel
     left-sum   Cauchy's S = sum h f(a + k h), k = 0 .. n-1: the integral's
                sum on n equal elements, by `sum`
     sup        the largest value of an Emmy function over a grid, the grid
                swept inside one kernel (the sup-norm of an error)

   Every result is {:value v :source :raster}.

   Reuse searched: alexandria.raster (kernel, value, sample, integral, root,
   ode) compiles one evaluation per call; raster.sci.quadrature sums
   sampled data (trapz) but no series; desargues.board.kernel sweep-form
   sweeps one variable into arrays. A summing loop with a sign carry is
   none of them, so it is written here on the same seam alexandria.raster
   uses (desargues.board.algebra/realize :raster, the raster-compiler
   namespace), not as a second evaluator."
  (:require [alexandria.raster :as ar]
            [desargues.board.algebra :as algebra]
            [desargues.board.compiler :as compiler]
            [emmy.env :as e]
            [raster.sci.roots]))

(def ^:private kernel-ns 'alexandria.raster.series-kernels)

(def ^:private ensure-ns!
  (delay (compiler/raster-compiler kernel-ns) kernel-ns))

(defn- lowered [expr]
  (let [v (e/simplify expr)]
    (if (number? v) (double v) (algebra/realize :raster v))))

(defn- param-syms [n] (mapv #(symbol (str "p" %)) (range n)))

(defn- compile-sum
  "ftm [k0 k1 s p0 .. p(n-1)] -> sum_{k = k0}^{k1} s^(k-k0) body; the index
   runs as a Double (raster's wasm has no Long*Double)."
  [body n-params]
  (binding [*ns* (the-ns @ensure-ns!)]
    (eval `(raster.core/ftm [~'k0 :- ~'Double ~'k1 :- ~'Double ~'s :- ~'Double
                             ~@(mapcat (fn [p] [p :- 'Double]) (param-syms n-params))] :- ~'Double
             (loop [~'k ~'k0 ~'sg 1.0 ~'acc 0.0]
               (if (<= ~'k ~'k1)
                 (recur (+ ~'k 1.0) (* ~'sg ~'s) (+ ~'acc (* ~'sg ~body)))
                 ~'acc))))))

(def ^:private sum-kernel
  (memoize (fn [body n-params] (compile-sum body n-params))))

(defn sum-kernel-of
  "The compiled loop of term, an Emmy function (fn [k p0 .. p(n-1)])."
  [term n-params]
  (sum-kernel (lowered (apply term 'k (param-syms n-params))) n-params))

(defn- call
  "Invoke a raster ftm on args as doubles. An ftm is an IFn without applyTo,
   so the arity is spelled out."
  [k args]
  (let [[a b c d f g] (map double args)]
    (case (count args)
      3 (k a b c)
      4 (k a b c d)
      5 (k a b c d f)
      6 (k a b c d f g)
      (throw (ex-info "alexandria.raster.series: at most 3 params" {:args args})))))

(defn sum
  "sum_{k = k0}^{k1} term(k, params..), alternating in sign from + when
   :alternating? is true: {:value v :source :raster}."
  ([term k0 k1] (sum term k0 k1 [] {}))
  ([term k0 k1 params] (sum term k0 k1 params {}))
  ([term k0 k1 params {:keys [alternating?]}]
   {:value (call (sum-kernel-of term (count params))
                 (concat [k0 k1 (if alternating? -1 1)] params))
    :source ar/source}))

(defn sums
  "The same partial sum at each params vector of params-seq, one kernel:
   [v ...]."
  [term k0 k1 params-seq opts]
  (let [k (sum-kernel-of term (count (first params-seq)))
        s (if (:alternating? opts) -1 1)]
    (mapv #(call k (concat [k0 k1 s] %)) params-seq)))

(defn left-sum
  "Cauchy's S = h (f(a) + f(a + h) + ... + f(a + (n-1) h)), h = (b - a)/n,
   f an Emmy function of one number: {:value v :source :raster}."
  [f a b n]
  (let [h (/ (- (double b) (double a)) n)]
    (sum (fn [k a h] (e/* h (f (e/+ a (e/* k h))))) 0 (dec n) [a h])))

(defn- compile-sup
  "ftm [lo h n p..] -> max_{i < n} body(lo + i h, p..)."
  [body n-params]
  (binding [*ns* (the-ns @ensure-ns!)]
    (eval `(raster.core/ftm [~'lo :- ~'Double ~'h :- ~'Double ~'n :- ~'Double
                             ~@(mapcat (fn [p] [p :- 'Double]) (param-syms n-params))] :- ~'Double
             (loop [~'i 0.0 ~'best -1.0e308]
               (if (< ~'i ~'n)
                 (let [~'x (+ ~'lo (* ~'i ~'h)) ~'v ~body]
                   (recur (+ ~'i 1.0) (if (> ~'v ~'best) ~'v ~'best)))
                 ~'best))))))

(def ^:private sup-kernel
  (memoize (fn [body n-params] (compile-sup body n-params))))

(defn sup
  "The largest value of g (an Emmy function (fn [x p0 ..])) over the n
   points lo, lo + h, ..., h = (hi - lo)/(n - 1): {:value v :source :raster}."
  ([g lo hi n] (sup g lo hi n []))
  ([g lo hi n params]
   (let [k (sup-kernel (lowered (apply g 'x (param-syms (count params)))) (count params))
         h (/ (- (double hi) (double lo)) (dec n))]
     {:value (call k (concat [lo h n] params)) :source ar/source})))

(defn- compile-sup-of-sum
  "ftm [lo h n k0 k1 s] -> max over x = lo + i h (i < n) of
   sum_{k=k0}^{k1} s^(k-k0) body(k, x) - g(x), or of its absolute value
   when abs?; body the term and g the comparison (both lowered)."
  [term-body cmp-body abs?]
  (binding [*ns* (the-ns @ensure-ns!)]
    (eval `(raster.core/ftm [~'lo :- ~'Double ~'h :- ~'Double ~'n :- ~'Double
                             ~'k0 :- ~'Double ~'k1 :- ~'Double ~'s :- ~'Double] :- ~'Double
             (loop [~'i 0.0 ~'best -1.0e308]
               (if (< ~'i ~'n)
                 (let [~'x (+ ~'lo (* ~'i ~'h))
                       ~'d (- (loop [~'k ~'k0 ~'sg 1.0 ~'acc 0.0]
                                (if (<= ~'k ~'k1)
                                  (recur (+ ~'k 1.0) (* ~'sg ~'s) (+ ~'acc (* ~'sg ~term-body)))
                                  ~'acc))
                              ~cmp-body)
                       ~'v ~(if abs? `(Math/abs ~'d) 'd)]
                   (recur (+ ~'i 1.0) (if (> ~'v ~'best) ~'v ~'best)))
                 ~'best))))))

(def ^:private sup-of-sum-kernel (memoize compile-sup-of-sum))

(defn sup-of-sum
  "max over the n grid points x of [lo hi] of S(x) - g(x), S(x) =
   sum_{k=k0}^{k1} (+-)^(k-k0) term(k, x): a partial sum's peak (g = 0) or,
   with :abs?, the sup of its error |S - g| (g the limit). One kernel, the
   grid and the terms as nested loops. opts: :alternating?, :abs?, :from
   (k0, default 1), :to (k1). The term's power of x must be written
   (e/exp (e/* k (e/log x))) or as a product when k is the exponent:
   raster lowers only literal exponents. {:value v :source :raster}."
  [term g lo hi n {:keys [alternating? abs? from to] :or {from 1}}]
  (let [k (sup-of-sum-kernel (lowered (term 'k 'x)) (lowered (g 'x)) (boolean abs?))
        h (/ (- (double hi) (double lo)) (dec n))]
    {:value (k (double lo) h (double n) (double from) (double to) (if alternating? -1.0 1.0))
     :source ar/source}))

(defn root-of
  "A zero between a and b of g, a function of one number whose values are
   themselves raster results (a `sup` or a `sum` at that number), by
   raster's Brent (raster.sci.roots/brent, which takes a raster ftm, so g is
   wrapped in one): {:value x :residual g(x) :iterations n :source :raster},
   nil when Brent does not converge. For an Emmy expression use
   alexandria.raster/root, which compiles it whole."
  [g a b]
  (let [k (binding [*ns* (the-ns @ensure-ns!)]
            ((eval `(fn [g#] (raster.core/ftm [~'d :- ~'Double] :- ~'Double (double (g# ~'d))))) g))
        {:keys [root f-root iterations converged?]} (raster.sci.roots/brent k (double a) (double b))]
    (when converged?
      {:value root :residual f-root :iterations iterations :source ar/source})))
