(ns alexandria.raster
  "The number backend of the library, on the JVM: an Emmy function of
   numbers, simplified by Emmy, lowered to raster's vocabulary
   (desargues.board.algebra/realize :raster) and compiled by raster into a
   native kernel. Emmy does the algebra; every number a shelf grades or a
   notebook shows comes out of a raster kernel or one of raster's solvers.

     kernel    (kernel f n) -> a raster ftm of n Double arguments, the
               compiled form of (f x1 .. xn); a vector-valued f gives one
               ftm per component (kernels)
     value     f at numbers, through its kernel
     sample    f at many points, one kernel call per point
     integral  the integral of f over [a b] by raster's adaptive
               Gauss-Kronrod (raster.sci.quadrature/quadgk)
     root      a zero of f in [a b] by raster's Brent (raster.sci.roots/brent)
     ode       the flow of an Emmy state derivative by raster's RK4
               (raster.ode/solve with raster.ode.core/->RK4)
     source    the provenance tag a :grade/numeric records

   Kernels are memoized on the simplified expression, so a figure or a
   claim compiles once per JVM. The browser side of the same contract is
   alexandria.medium.kernel (emmy-viewers' :raster backend, WebAssembly).

   Reuse searched: desargues.board.compiler/raster-compiler defines kernels
   in a namespace with raster's vocabulary referred (used here);
   desargues.board.algebra/realize :raster lowers an Emmy expression (used
   here); raster.sci.quadrature, raster.sci.roots and raster.ode are the
   solvers. Nothing here re-implements a solver."
  (:require [desargues.board.algebra :as algebra]
            [desargues.board.compiler :as compiler]
            [emmy.env :as e]
            [raster.ode]
            [raster.ode.core]
            [raster.sci.quadrature :as quadrature]
            [raster.sci.roots :as roots]
            [clojure.walk :as walk]))

(def source
  "What a :grade/numeric of this library records as the origin of its number."
  :raster)

(def ^:private kernel-ns 'alexandria.raster.kernels)

(def ^:private ensure-ns!
  (delay (compiler/raster-compiler kernel-ns) kernel-ns))

(defn- arg-syms [n] (mapv #(symbol (str "x" %)) (range n)))

(defn- atan2
  "desargues' :raster realisation passes (atan y x) through as raster's
   one-argument atan; Emmy's two-argument atan is atan2 (same order), so
   rewrite it after the lowering."
  [form]
  (walk/postwalk (fn [x] (if (and (seq? x) (= 'raster.math/atan (first x)) (= 3 (count x)))
                           (cons 'raster.math/atan2 (rest x))
                           x))
                 form))

(defn- lowered
  "The raster vocabulary of Emmy expression expr; a number stays a double."
  [expr]
  (let [v (e/simplify expr)]
    (if (number? v) (double v) (atan2 (algebra/realize :raster v)))))

(defn- compile-ftm
  "A raster ftm (fn [x0 .. x(n-1)] :- Double) of the lowered body."
  [syms body]
  (binding [*ns* (the-ns @ensure-ns!)]
    (eval `(raster.core/ftm [~@(mapcat (fn [s] [s :- 'Double]) syms)] :- ~'Double ~body))))

(def ^:private compile-memo
  (memoize (fn [syms body] (compile-ftm syms body))))

(defn kernels
  "The raster ftms of f, a function of n numbers returning a number or a
   vector of numbers: one ftm per component, each of n Double arguments."
  [f n]
  (let [syms (arg-syms n)
        out (apply f syms)]
    (mapv #(compile-memo syms (lowered %)) (if (sequential? out) out [out]))))

(defn kernel
  "The raster ftm of f, a function of n numbers returning one number."
  [f n]
  (first (kernels f n)))

(defn- call
  "Kernel k at the numbers xs. An ftm has no applyTo, so every arity is
   spelled out."
  [k xs]
  (let [x (fn [i] (double (nth xs i)))]
    (case (count xs)
      0 (k)
      1 (k (x 0))
      2 (k (x 0) (x 1))
      3 (k (x 0) (x 1) (x 2))
      4 (k (x 0) (x 1) (x 2) (x 3))
      5 (k (x 0) (x 1) (x 2) (x 3) (x 4))
      6 (k (x 0) (x 1) (x 2) (x 3) (x 4) (x 5))
      (throw (ex-info "alexandria.raster: kernels take at most 6 numbers" {:n (count xs)})))))

(defn value
  "f at the numbers xs, computed by f's raster kernels: a double, or a
   vector of doubles when f returns a vector."
  [f & xs]
  (let [n (count xs)
        ks (kernels f n)
        out (apply f (arg-syms n))
        vs (mapv #(call % xs) ks)]
    (if (sequential? out) vs (first vs))))

(defn sample
  "f at each point of points (each a vector of numbers), by its raster
   kernels: [value ...]."
  [f points]
  (let [n (count (first points))
        ks (kernels f n)
        vector? (sequential? (apply f (arg-syms n)))]
    (mapv (fn [p] (let [vs (mapv #(call % p) ks)] (if vector? vs (first vs)))) points)))

(defn integral
  "The integral of f (an Emmy function of one number) over [a b], by
   raster's adaptive Gauss-Kronrod quadrature on f's kernel:
   {:value v :error estimate :source :raster}."
  [f a b]
  (let [[v err] (quadrature/quadgk (kernel f 1) (double a) (double b))]
    {:value v :error err :source source}))

(defn root
  "A zero of f (an Emmy function of one number) between a and b, where f
   changes sign, by raster's Brent method on f's kernel:
   {:value x :residual f(x) :iterations n :source :raster}, or nil when
   Brent does not converge."
  [f a b]
  (let [{:keys [root f-root iterations converged?]} (roots/brent (kernel f 1) (double a) (double b))]
    (when converged?
      {:value root :residual f-root :iterations iterations :source source})))

(defn- rhs
  "A raster ftm (du u t) writing the components of (deriv t u0 .. u(d-1))."
  [deriv d]
  (let [us (arg-syms d)
        outs (apply deriv 't us)
        bodies (mapv lowered outs)]
    (binding [*ns* (the-ns @ensure-ns!)]
      (eval `(raster.core/ftm [~'du :- (~'Array ~'double) ~'u :- (~'Array ~'double) ~'t :- ~'Double] :- ~'Long
               (let [~@(mapcat (fn [i s] [s `(~'aget ~'u ~i)]) (range) us)]
                 ~@(map-indexed (fn [i b] `(~'aset ~'du ~i ~b)) bodies)
                 0))))))

(defn ode
  "The flow of du/dt = (deriv t u0 .. u(d-1)), deriv an Emmy function
   returning the d components of the derivative, from the state u0 at
   t = 0 to t1 in steps dt, by raster's RK4:
   {:ts [t ...] :us [[u0 ..] ...] :source :raster}."
  [deriv u0 t1 dt]
  (let [d (count u0)
        prob (raster.ode/ode-problem (rhs deriv d) (double-array (map double u0)) 0.0 (double t1))
        sol (raster.ode/solve (raster.ode.core/->RK4) prob (double dt))]
    {:ts (vec (:ts sol)) :us (mapv vec (:us sol)) :source source}))
