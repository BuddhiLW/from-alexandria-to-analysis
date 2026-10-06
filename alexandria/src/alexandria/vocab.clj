(ns alexandria.vocab
  "Alexandria's historical vocabulary as Emmy literal functions.

   Reuse searched before writing: Emmy literal functions
   (`emmy.abstract.function/literal-function`), expression freezing and
   simplification (`emmy.env/freeze`, `emmy.env/simplify`), and Desargues board
   point/check readers (`desargues.board.construction/coords-of`, `xy-of`,
   `author-expr`, `check`; `desargues.board.euclid` registers `:area`). This
   namespace only names vocabulary operations and registers open meanings."
  (:refer-clojure :exclude [angle])
  (:require [clojure.walk :as walk]
            [desargues.board.construction :as c]
            [desargues.board.euclid]
            [emmy.abstract.function :as f]
            [emmy.env :as e]
            [emmy.generic]
            [emmy.numerical.quadrature :as q]
            [hive-dsl.result :as r]))

(def vocabulary
  '[area length angle ratio segment-area parabola hyperbola ellipse circle
    series-sum square rectangle cube cone sphere cylinder arc sector tangent
    chord diameter radius circumference volume surface-area differential])

(defn literal
  "An unconstrained scalar Emmy literal function named by symbol `op`, with arity n."
  [op n]
  (f/literal-function op (vec (repeat n 0)) 0))

(def area (literal 'area 3))

(def length (literal 'length 2))
(def angle (literal 'angle 3))
(def ratio (literal 'ratio 2))
(def segment-area (literal 'segment-area 3))
(def parabola (literal 'parabola 3))
(def hyperbola (literal 'hyperbola 2))
(def ellipse (literal 'ellipse 2))
(def circle (literal 'circle 2))
(def series-sum (literal 'series-sum 2))
(def square (literal 'square 1))
(def rectangle (literal 'rectangle 2))
(def cube (literal 'cube 1))
(def cone (literal 'cone 2))
(def sphere (literal 'sphere 1))
(def cylinder (literal 'cylinder 2))
(def arc (literal 'arc 3))
(def sector (literal 'sector 3))
(def tangent (literal 'tangent 2))
(def chord (literal 'chord 2))
(def diameter (literal 'diameter 1))
(def radius (literal 'radius 1))
(def circumference (literal 'circumference 1))
(def volume (literal 'volume 1))
(def surface-area (literal 'surface-area 1))

(def differential
  "The difference of a flowing quantity: Leibniz's d, Newton's fluxion.
   Its meaning is Emmy's derivative operator D applied to the quantity, a
   function of the flow (time, or the abscissa); its notations are
   alexandria.notation's :leibniz and :newton."
  (literal 'differential 1))

(defmulti realize
  "Operation symbol, already-evaluated args, board env -> Emmy value. Open by op."
  (fn [op _args _env] op))

(defn- realize-form
  "Bottom-up realization of an already frozen form, without freezing again
   (the form may carry Emmy's derivative perturbations)."
  [form env]
  (walk/postwalk (fn [x] (if (seq? x) (realize (first x) (vec (rest x)) env) x)) form))

(defn d-symbol
  "The name of the difference of quantity s: x -> dx."
  [s]
  (symbol (str "d" (name s))))

(defn- d-symbol? [s]
  (and (symbol? s) (boolean (re-matches #"d[a-zA-Z][0-9]*" (name s)))))

(defn- quantities
  "The free quantity symbols of a frozen expression, operators and
   differences excluded, sorted."
  [form]
  (if (symbol? form)
    (if (d-symbol? form) [] [form])
    (->> (tree-seq seq? rest form)
         (filter #(and (symbol? %) (not (d-symbol? %))))
         distinct
         sort)))

(defmethod realize 'differential [_ [q] env]
  ;; Leibniz's rule of the calculus: d q is the sum, over the quantities s of
  ;; q, of the rate of q in s times ds. The rate is Emmy's derivative D of q
  ;; taken as a function of s alone. Quantities named in env :constants are
  ;; Leibniz's "quantitas data constans", whose d is 0.
  (let [form (e/freeze q)
        constant? (set (:constants env))]
    (reduce e/+ 0
            (for [s (remove constant? (quantities form))]
              (e/* ((e/D (fn [t] (realize-form (walk/postwalk-replace {s t} form) env))) s)
                   (d-symbol s))))))

(defmethod realize 'parabola [_ [a b c] _env]
  (fn [x]
    (e/+ (e/* a x x) (e/* b x) c)))

(defmethod realize :default [op args _env]
  (if-let [v (or (ns-resolve 'emmy.env op)
                 (ns-resolve 'emmy.generic op))]
    (apply (deref v) args)
    (throw (ex-info "Unknown Alexandria vocabulary operation"
                    {:alexandria/error :vocab/unknown-op
                     :vocab/op op
                     :vocab/known vocabulary}))))

(defn- point-id [x]
  (cond
    (keyword? x) x
    (symbol? x) (keyword (name x))
    :else x))

(defn- point-xy [p env]
  (let [p (get-in env [:points (point-id p)])]
    [(:x p) (:y p)]))

(defmethod realize 'length [_ [a b] env]
  (c/check {:distance [(point-id a) (point-id b)]} env))

(defmethod realize 'angle [_ [a o b] env]
  (c/check {:angle [(point-id a) (point-id o) (point-id b)]} env))

(defmethod realize 'area [_ ids env]
  (c/check {:area (mapv point-id ids)} env))

(defmethod realize 'segment-area [_ [curve a b] env]
  (let [[x1 y1] (point-xy a env)
        [x2 y2] (point-xy b env)
        lo (min x1 x2)
        hi (max x1 x2)
        dx (- x2 x1)
        chord (if (zero? dx)
                (constantly y1)
                (let [m (/ (- y2 y1) dx)
                      intercept (- y1 (* m x1))]
                  (fn [x] (+ (* m x) intercept))))]
    (e/simplify
     (q/definite-integral
      (fn [x] (e/abs (- (curve x) (chord x))))
      lo
      hi))))

(defmethod realize 'ratio [_ [a b] _env]
  (e/divide a b))

(defn evaluate
  "Bottom-up realization of a quoted or Emmy-frozen vocabulary expression in env."
  [expr env]
  (walk/postwalk
   (fn [x]
     (if (seq? x)
       (realize (first x) (vec (rest x)) env)
       x))
   (e/freeze expr)))

(defn evaluate-result
  "Evaluate expr on Result rails. Unknown vocabulary heads are :vocab/unknown-op errors."
  [expr env]
  (try
    (r/ok (evaluate expr env))
    (catch clojure.lang.ExceptionInfo ex
      (let [data (ex-data ex)]
        (if (= :vocab/unknown-op (:alexandria/error data))
          (r/err data)
          (throw ex))))))

(defn- claim-sides [claim]
  (cond
    (and (seq? claim) (= '= (first claim))) [(second claim) (nth claim 2)]
    (and (vector? claim) (= 2 (count claim))) claim
    :else [claim 0]))

(defmethod c/check :claim [{:keys [claim]} env]
  (let [[lhs rhs] (claim-sides claim)]
    (e/simplify (e/- (evaluate lhs env) (evaluate rhs env)))))
