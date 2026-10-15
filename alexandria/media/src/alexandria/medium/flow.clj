(ns alexandria.medium.flow
  "Browser-side integration for playgrounds: a raster RK4 step kernel
   (alexandria.raster.step/rk4 compiled by emmy-viewers on the raster
   backend) iterated in the page, cached on its parameters.

   The forms here are quoted ClojureScript for a reagent with-let; the
   math stays in the Emmy kernels."
  (:require [alexandria.raster.step :as step]
            [clojure.walk :as walk]
            [emmy.env :as e]
            [emmy.viewer.compile :as vc]
            [emmy.viewer.raster]))

(defn kernel
  "Compile (f params...) -> (fn [state] ...) on the raster backend,
   unsimplified: [sym form] for a reagent with-let."
  [label f params state]
  (binding [vc/*backend* :raster]
    [(gensym label) (vc/compiled-fn f params state {:simplify? false})]))

(defn step-kernel
  "[sym form] of one RK4 step: (fn [& params] (fn [s] (rk4 (deriv params) dt s))),
   `deriv-of` taking the params without dt (dt is the last param)."
  [label deriv-of params state]
  (kernel label
          (fn [& ps] (fn [s] (step/rk4 (apply deriv-of (butlast ps)) (last ps) s)))
          params state))

(defn cross-values
  "Where a sampled quantity u reaches the level m between two RK4 nodes
   (t0, u0) and (t1, u1): the linear crossing time, and the same for a
   second coordinate x carried along."
  [t0 u0 t1 u1 m]
  (fn [_] [(e/+ t0 (e/* (e/- m u0) (e// (e/- t1 t0) (e/- u1 u0))))]))

(defn cross-kernel [] (kernel "cross" cross-values [0 0 1 1 0.5] [0]))

(defn integrate-form
  "A quoted (fn [s0 params n stop?] [s0 s1 ...]): iterate the step kernel
   `step` from s0 until n steps or (stop? s i)."
  [step]
  (list 'fn '[s0 params n stop?]
        (list 'loop '[i 0 s s0 acc (transient [s0])]
              (list 'if '(and (< i n) (not (stop? s i)))
                    (list 'let ['s2 (list step 's 'params)] '(recur (inc i) s2 (conj! acc s2)))
                    '(persistent! acc)))))

(defn crossing-form
  "A quoted (fn [pts j k m] t): the crossing of coordinate k's level m by
   the RK4 points, coordinate j as abscissa, through the cross kernel; nil
   when the points never reach m."
  [cross]
  (list 'fn '[pts j k m]
        (list 'let '[i (loop [i 1] (cond (>= i (count pts)) nil
                                          (>= (* (- (aget (nth pts i) k) m) (- (aget (nth pts (dec i)) k) m)) 0) (recur (inc i))
                                          :else i))]
              (list 'when 'i
                    (list 'let '[p (nth pts (dec i)) q (nth pts i)]
                          (list 'aget (list cross '(array 0) '(array (aget p j) (aget p k) (aget q j) (aget q k) m)) 0))))))

(defn fmt
  "A quoted number formatter: d decimals, \"undefined\" when not finite."
  ([x] (fmt x 3))
  ([x d] (list 'let ['v x] (list 'if '(and (number? v) (js/isFinite v)) (list '.toFixed 'v d) "undefined"))))

(defn template
  "Fill a quoted browser form: every symbol key of subs is replaced."
  [subs form]
  (walk/postwalk (fn [x] (if (and (symbol? x) (contains? subs x)) (subs x) x)) form))

(defn button
  "A \"try this\" preset: resets the state atom to values."
  [state label values]
  [:button {:type "button" :data-preset label :style {:margin "0.2em"}
            :on-click (list 'fn [] (list 'reset! state values))} label])
