(ns alexandria.lagrange.playground
  "Reader experiments attached to Lagrange's six narrated constructions."
  (:require [alexandria.medium.tie :as tie]
            [emmy.env :as e]
            [emmy.leva :as leva]
            [emmy.mafs :as mafs]
            [emmy.viewer :as ev]
            [emmy.viewer.compile :as vc]
            [emmy.viewer.raster]
            [alexandria.raster :as raster]
            [alexandria.lagrange.variations :as variations]
            [emmy.mechanics.lagrange :as L]
            [alexandria.lagrange.mechanics :as mechanics]
            [clojure.walk :as walk]))

(def setups
  {:delta {:start {:q 1 :eta 0.5 :eps 0 :shape 0}
           :range {:q [0 2 0.01] :eta [-2 2 0.01] :eps [-1 1 0.01] :shape [0 2 1]}
           :tries [["non-extremal curve" {:q 1.6 :eta 0.5 :eps 0 :shape 0}]
                   ["reverse η" {:q 1.6 :eta -0.5 :eps 0 :shape 1}]
                   ["tilt the path" {:q 1 :eta 0.8 :eps 0.5 :shape 2}]]}
   :virtual {:start {:m1 2 :m2 1 :l1 1 :l2 2 :dq 0.4}
             :range {:m1 [0 5 0.01] :m2 [0 5 0.01] :l1 [0.1 3 0.01] :l2 [0.1 3 0.01] :dq [-1 1 0.01]}
             :tries [["unbalance the lever" {:m1 2 :m2 1 :l1 1 :l2 2.8 :dq 0.4}]
                     ["reverse δq" {:m1 2 :m2 1 :l1 1 :l2 2.8 :dq -0.4}]]}
   :pendulum {:start {:l 1 :g 9.81 :theta 1.1 :omega 0 :m 1}
              :range {:l [0.2 3 0.01] :g [1 20 0.01] :theta [-3.1 3.1 0.01] :omega [-5 5 0.01] :m [0.1 4 0.01]}
              :tries [["small angle" {:l 1 :g 9.81 :theta 0.15 :omega 0 :m 1}]
                      ["rotation" {:l 1 :g 9.81 :theta 1.1 :omega 4.5 :m 1}]]}
   :double {:start {:l1 1 :l2 1 :m1 1 :m2 1 :a 1.9 :b 2.4 :da 0.001}
            :range {:l1 [0.3 2 0.01] :l2 [0.3 2 0.01] :m1 [0.2 3 0.01] :m2 [0.2 3 0.01] :a [-3 3 0.01] :b [-3 3 0.01] :da [0 0.1 0.0001]}
            :tries [["wider separation" {:l1 1 :l2 1 :m1 1 :m2 1 :a 1.9 :b 2.4 :da 0.08}]
                    ["near the bottom" {:l1 1 :l2 1 :m1 1 :m2 1 :a 0.2 :b 0.1 :da 0.001}]]}
   :kepler {:start {:energy -0.4 :h 1 :mu 1}
            :range {:energy [-1 1 0.01] :h [0.3 2 0.01] :mu [0.2 2 0.01]}
            :tries [["parabola" {:energy 0 :h 1 :mu 1}]
                    ["hyperbola" {:energy 0.4 :h 1 :mu 1}]]}
   :mean {:start {:k 1 :s 0 :a 0.2 :b 1.4}
          :range {:k [0 2 0.01] :s [-2 2 0.01] :a [0 1.5 0.01] :b [0.1 2 0.01]}
          :tries [["shift the interval" {:k 1 :s 0 :a 1 :b 1.9}]
                  ["straight line" {:k 0 :s 1 :a 0.2 :b 1.4}]]}
   :brach {:start {:x 3.14159 :y 2 :g 9.81}
           :range {:x [0.3 5 0.01] :y [0.3 4 0.01] :g [1 20 0.01]}
           :tries [["shallow descent" {:x 4 :y 0.5 :g 9.81}]
                   ["steep descent" {:x 1 :y 3 :g 9.81}]]}})

(defn- sq [x] (e/* x x))

(defn variation-shape [shape t]
  (let [base (e/sin (e/* Math/PI t))
        sine (e/sin (e/* 2 Math/PI t))
        tilt (e/* 4 t (e/- 1 t) (e/- (e/* 2 t) 1))]
    (e/+ (e/* (tie/at-least (e/- 1 shape) 0) base)
         (e/* (e/- 1 (e/abs (e/- shape 1))) sine)
         (e/* (tie/at-least (e/- shape 1) 0) tilt))))

(defn delta-coefficients
  "Quadrature of the chosen eta against the base variation and itself.
   The action is pi * integral (y - sin(pi*t))^2 dt."
  [shape]
  (let [eta (fn [t] (variation-shape shape t))]
    {:cross (:value (raster/integral (fn [t] (e/* (e/sin (e/* Math/PI t)) (eta t))) 0 1))
     :norm (:value (raster/integral (fn [t] (sq (eta t))) 0 1))}))

(def ^:private variation-coefficients (delay (mapv delta-coefficients [0 1 2])))
(defn eccentricity [energy h mu]
  (e/sqrt (tie/at-least (e/+ 1 (e// (e/* 2 energy (sq h)) (sq mu))) 0)))
(defn mean-c [a b] (e/sqrt (e// (e/+ (sq a) (e/* a b) (sq b)) 3)))

(defn mean-root
  "Brent solves f'(c) = (f(b)-f(a))/(b-a), f = kx³+sx."
  [k s a b]
  (when (< a b)
    (if (zero? k)
      (raster/value (fn [a b] (e/* 0.5 (e/+ a b))) a b)
      (let [slope (+ (* k (+ (* a a) (* a b) (* b b))) s)]
        (:value (raster/root
                 (fn [c] (e/- (e/+ (e/* 3 k (sq c)) s) slope)) a b))))))

(defn cycloid-end
  "Brent solves (θ − sin θ)/(1 − cos θ) = x/y; endpoint B is (x,y).
   Positive endpoints require θ in (0,2π)."
  [x y]
  (let [theta (:value (raster/root
                       (fn [t] (e/- (e/- t (e/sin t))
                                     (e/* (/ x y) (e/- 1 (e/cos t)))))
                       1e-5 (- (* 2 Math/PI) 1e-5)))]
    (when theta {:theta theta :a (/ y (- 1 (Math/cos theta)))})))

(defn brach-race
  "Endpoint-dependent tracks from A=(0,0) to B=(x,y), all times from
   raster quadrature of the Emmy descent Lagrangian. The circular comparison
   is the arc tangent vertically at A."
  [x y g]
  (let [{:keys [a theta]} (cycloid-end x y)
        c (/ (+ (* x x) (* y y)) (* 2 x))
        phi (Math/atan2 y (- c x))
        tracks {:line (fn [u] (e/up (e/* x u) (e/* y u)))
                :arc (fn [u] (let [v (e/* phi u)]
                                (e/up (e/* c (e/- 1 (e/cos v))) (e/* c (e/sin v)))))
                :cycloid (fn [u] (let [v (e/* theta u)]
                                    (e/up (e/* a (e/- v (e/sin v)))
                                          (e/* a (e/- 1 (e/cos v))))))}]
    {:tracks tracks
     :times (update-vals tracks (fn [path]
                                   (:value (raster/integral (variations/time-element g path) 1e-4 1))))
     :theta theta :a a}))

(defn pendulum-run
  "Emmy's L-pendulum state derivative, raster RK4; rows [t, θ, ω, E]."
  [l g m theta omega]
  (let [sd (L/Lagrangian->state-derivative (L/L-pendulum g m l))
        raw (raster/ode (fn [t th om]
                          (let [[_ dth dom] (sd (e/up t th om))]
                            [dth dom]))
                        [theta omega] 8.0 0.02)]
    (mapv (fn [t [th om]]
            [t th om (raster/value
                         (fn [a w] (e/* m (e/- (e/* 0.5 (sq (e/* l w)))
                                                 (e/* g l (e/cos a))))) th om)])
          (:ts raw) (:us raw))))

(defn double-runs
  "Two nearby initial angles integrated by raster RK4 from Emmy's double
   pendulum Lagrangian. Both carry the chosen masses and lengths."
  [l1 l2 m1 m2 a b da]
  (let [sd (L/Lagrangian->state-derivative
            (mechanics/L-double-pendulum m1 m2 l1 l2 9.81))
        deriv (fn [t a b va vb]
                (let [[_ _ [aa ab]] (sd (e/up t (e/up a b) (e/up va vb)))]
                  [va vb aa ab]))
        flow (fn [initial] (raster/ode deriv [initial b 0 0] 8.0 0.02))]
    [(flow a) (flow (+ a da))]))

(defn- pendulum-derivative [l g theta omega]
  (let [sd (L/Lagrangian->state-derivative (L/L-pendulum g 1 l))
        [_ dt dw] (sd (e/up 0 theta omega))]
    [dt dw]))

(defn pendulum-measure
  "Period between upward zero-crossings in the RK4 θ(t) trajectory."
  [rows]
  (let [crossings (keep (fn [[[t0 a] [t1 b]]]
                          (when (and (< a 0) (<= 0 b))
                            (+ t0 (* (- t1 t0) (/ (- a) (- b a))))))
                        (partition 2 1 (map (fn [[t theta & _]] [t theta]) rows)))]
    (when (>= (count crossings) 2) (- (second crossings) (first crossings)))))

(defn- rk4-form
  "Client RK4 stepping the raster WASM derivative of Emmy's Lagrangian."
  [f params initial steps dt]
  (let [call (fn [u] (list f '(array 0) (list* 'array (concat params (map (fn [i] (list 'nth u i)) (range (count initial)))))))]
    (list 'loop ['i 0 't 0 'u initial 'rows []]
          (list 'if (list '>= 'i steps) 'rows
                (list 'let ['a (call 'u)
                            'half (list 'mapv '+ 'u (list 'mapv (list 'fn ['v] (list '* 0.5 dt 'v)) 'a))
                            'b (call 'half)
                            'half2 (list 'mapv '+ 'u (list 'mapv (list 'fn ['v] (list '* 0.5 dt 'v)) 'b))
                            'c (call 'half2)
                            'next-u (list 'mapv '+ 'u (list 'mapv (list 'fn ['v] (list '* dt 'v)) 'c))
                            'd (call 'next-u)
                            'new-u (list 'mapv (list 'fn ['x 'ka 'kb 'kc 'kd]
                                                       (list '+ 'x (list '* (/ dt 6) (list '+ 'ka (list '* 2 'kb) (list '* 2 'kc) 'kd))))
                                           'u 'a 'b 'c 'd)]
                      (list 'recur (list 'inc 'i) (list '+ 't dt) 'new-u (list 'conj 'rows (list 'into ['t] 'u))))))))

;; the pendulum's bob handle is a draggable, defined below with the other
;; figures' handles
(declare ^:private draggable)

(defn- pendulum-figure [!p]
  (let [[dsym dform] (tie/kernel-binding "pendulum-rhs"
                       (fn [l g theta omega] (fn [_] (pendulum-derivative l g theta omega)))
                       [1 9.81 1.1 0])
        run (rk4-form dsym [(ev/get !p :l) (ev/get !p :g)]
                      [(ev/get !p :theta) (ev/get !p :omega)] 800 0.02)]
    (list 'reagent.core/with-let [dsym dform 'clock '(atom 0) 'raf '(atom nil)
                                   '_ '(letfn [(step [_] (swap! clock (fn [t] (mod (+ t 1) 400)))
                                                       (reset! raf (js/requestAnimationFrame step)))]
                                          (reset! raf (js/requestAnimationFrame step)))]
          (list 'let ['rows run 'now '(nth rows @clock) 'theta '(nth now 1)
                      'period '(let [crossings (keep (fn [[[t0 a] [t1 b]]]
                                       (when (and (< a 0) (<= 0 b))
                                         (+ t0 (* (- t1 t0) (/ (- a) (- b a))))))
                                     (partition 2 1 (map (fn [[t theta & _]] [t theta]) rows)))]
                (when (>= (count crossings) 2) (- (second crossings) (first crossings))))]
                [:div
                 (mafs/mafs {:view-box {:x [-3.5 3.5] :y [-3.5 1]} :height 280}
                            (mafs/cartesian)
                            (mafs/segment {:point1 [0 0]
                                           :point2 [(list '* (ev/get !p :l) '(js/Math.sin theta))
                                                    (list '- (list '* (ev/get !p :l) '(js/Math.cos theta)))] :color :blue})
                            (mafs/point {:x (list '* (ev/get !p :l) '(js/Math.sin theta))
                                         :y (list '- (list '* (ev/get !p :l) '(js/Math.cos theta))) :color :red})
                            (draggable !p :theta 1.1 '(fn [[x y]] (js/Math.atan2 x (- y)))
                                       (fn [v] (if (number? v) [(Math/sin v) (- (Math/cos v))]
                                                   [(list 'js/Math.sin v) (list '- (list 'js/Math.cos v))]))
                                       '(fn [[x y]] [x y])))
                 (mafs/mafs {:view-box {:x [0 8] :y [-7 7]} :height 180}
                            (mafs/cartesian)
                            (mafs/polyline {:points '(mapv (fn [[t th _]] [t th]) rows) :color :blue})
                            (mafs/text "θ(t), raster RK4" {:x 4 :y 6}))
                 (mafs/mafs {:view-box {:x [-7 7] :y [-7 7]} :height 180}
                            (mafs/cartesian)
                            (mafs/polyline {:points '(mapv (fn [[_ th om]] [th om]) rows) :color :violet})
                            (mafs/text "phase (θ, ω), energy level" {:x 2 :y 6}))
                 [:p "Measured period from RK4 zero-crossings: "
                  [:span {:data-period "measured"} (list 'if 'period (tie/fmt 'period 3) "rotation: no period")]
                  "; small-angle 2π√(l/g): " (tie/fmt '(aget r 1) 3)]])
          '(finally (some-> @raf js/cancelAnimationFrame)))))

(defn- double-derivative [l1 l2 m1 m2 a b va vb]
  (let [sd (L/Lagrangian->state-derivative
             (mechanics/L-double-pendulum m1 m2 l1 l2 9.81))
        [_ _ [aa ab]] (sd (e/up 0 (e/up a b) (e/up va vb)))]
    [va vb aa ab]))

(defn- double-figure [!p]
  (let [[dsym dform] (tie/kernel-binding "double-rhs" (fn [l1 l2 m1 m2 a b va vb] (fn [_] (double-derivative l1 l2 m1 m2 a b va vb))) [1 1 1 1 1.9 2.4 0 0])
        params (mapv #(ev/get !p %) [:l1 :l2 :m1 :m2])
        a (ev/get !p :a) b (ev/get !p :b)
        one (rk4-form dsym params [a b 0 0] 400 0.02)
        other (rk4-form dsym params [(list '+ a (ev/get !p :da)) b 0 0] 400 0.02)
        bob (fn [row] (list 'let ['a (list 'nth row 1) 'b (list 'nth row 2)]
                               [(list '+ (list '* (ev/get !p :l1) '(js/Math.sin a))
                                        (list '* (ev/get !p :l2) '(js/Math.sin b)))
                                (list '- (list '+ (list '* (ev/get !p :l1) '(js/Math.cos a))
                                                       (list '* (ev/get !p :l2) '(js/Math.cos b))))]))]
    (list 'reagent.core/with-let [dsym dform 'clock '(atom 0) 'raf '(atom nil)
                                   '_ '(letfn [(step [_] (swap! clock (fn [t] (mod (+ t 1) 400)))
                                                       (reset! raf (js/requestAnimationFrame step)))]
                                          (reset! raf (js/requestAnimationFrame step)))]
          (list 'let ['first-run one 'second-run other
                      'first-bobs (list 'mapv (list 'fn ['row] (bob 'row)) 'first-run)
                      'second-bobs (list 'mapv (list 'fn ['row] (bob 'row)) 'second-run)
                      'separation '(mapv (fn [r s p q]
                                             [(first r) (js/Math.log (max 1e-9 (js/Math.hypot (- (first p) (first q)) (- (second p) (second q)))))])
                                           first-run second-run first-bobs second-bobs)]
                [:div
                 (mafs/mafs {:view-box {:x [-4 4] :y [-4 4]} :height 300}
                            (mafs/cartesian)
                            (mafs/polyline {:points 'first-bobs :color :blue})
                            (mafs/polyline {:points 'second-bobs :color :red})
                            (mafs/point {:x '(first (nth first-bobs @clock)) :y '(second (nth first-bobs @clock)) :color :blue})
                            (mafs/point {:x '(first (nth second-bobs @clock)) :y '(second (nth second-bobs @clock)) :color :red})
                            (draggable !p :a 1.9 '(fn [[x y]] (js/Math.atan2 x (- y)))
                                       (fn [v] (if (number? v) [(Math/sin v) (- (Math/cos v))]
                                                   [(list 'js/Math.sin v) (list '- (list 'js/Math.cos v))]))
                                       '(fn [[x y]] [x y])))
                 (mafs/mafs {:view-box {:x [0 8] :y [-15 4]} :height 230}
                            (mafs/cartesian)
                            (mafs/polyline {:points 'separation :color :violet})
                            (mafs/text "log bob separation vs t" {:x 3 :y 2}))])
          '(finally (some-> @raf js/cancelAnimationFrame)))))

(defn- kepler-derivative [mu x y vx vy]
  (let [r (e/sqrt (e/+ (sq x) (sq y) 1e-12))
        inv (e// mu (e/* r r r))]
    [vx vy (e/- (e/* inv x)) (e/- (e/* inv y))]))

(defn- kepler-figure [!p]
  (let [[dsym dform] (tie/kernel-binding "kepler-rhs" (fn [mu x y vx vy] (fn [_] (kepler-derivative mu x y vx vy))) [1 1 0 0 1])
        ecc (list 'aget 'r 0) p (list 'aget 'r 1)
        rp (list '/ p (list '+ 1 ecc))
        vy (list '/ (ev/get !p :h) rp)
        flow (rk4-form dsym [(ev/get !p :mu)] [rp 0 0 vy] 500 0.015)]
    (list 'reagent.core/with-let [dsym dform 'clock '(atom 0) 'raf '(atom nil)
                                   '_ '(letfn [(step [_] (swap! clock (fn [t] (mod (+ t 1) 500)))
                                                       (reset! raf (js/requestAnimationFrame step)))]
                                          (reset! raf (js/requestAnimationFrame step)))]
          (list 'let ['rows flow 'planet '(nth rows @clock)]
                (mafs/mafs {:view-box {:x [-5 5] :y [-5 5]} :height 390}
                           (mafs/cartesian)
                           (mafs/polyline {:points '(mapv (fn [[t x y & _]] [x y]) rows) :color :blue})
                           (mafs/point {:x 0 :y 0 :color :yellow})
                           (mafs/text "F" {:x 0 :y 0})
                           (mafs/point {:x '(nth planet 1) :y '(nth planet 2) :color :red})
                           (mafs/segment {:point1 [rp 0] :point2 [rp vy] :color :green})
                           (draggable !p :h 1 '(fn [[x y]] (max 0.3 (min 2 (* y (js/Math.sqrt (max 1e-6 x))))))
                                      (fn [v] [1 v])
                                      (list 'fn '[[x y]] [rp '(max 0.3 (min 2 y))]))
                           (mafs/text "velocity at perihelion" {:x 2 :y 1})))
          '(finally (some-> @raf js/cancelAnimationFrame)))))

(defn- brach-figure [!p xy]
  (let [x (ev/get !p :x) y (ev/get !p :y)]
    (mafs/mafs {:view-box {:x [-0.3 5.5] :y [-0.3 4.5]} :height 340}
               (mafs/cartesian)
               (mafs/parametric {:xy xy :t [0 1] :color :blue})
               (mafs/point {:x 0 :y 0 :color :green})
               (mafs/text "A" {:x 0 :y 0})
               (mafs/text "B" {:x x :y y})
               (draggable !p :x 3.14159 'first (fn [v] [v 2])
                          (walk/postwalk-replace {'!p !p} '(fn [[px py]] [(max 0.3 (min 5 px)) (get @!p :y)])))
               (draggable !p :y 2 'second (fn [v] [3.14159 v])
                          (walk/postwalk-replace {'!p !p} '(fn [[px py]] [(get @!p :x) (max 0.3 (min 4 py))]))))))

(defn readout [id]
  (case id
    :delta (fn [q eta eps shape] (fn [_]
             (let [cross (reduce e/+ (map-indexed (fn [i {:keys [cross]}]
                                                    (e/* cross (e/- 1 (e/abs (e/- shape i))))) @variation-coefficients))
                   norm (reduce e/+ (map-indexed (fn [i {:keys [norm]}]
                                                   (e/* norm (e/- 1 (e/abs (e/- shape i))))) @variation-coefficients))
                   d (e/- q 1)]
               [(e/* Math/PI (e/+ (e/* 0.5 (sq d)) (e/* 2 d eps eta cross) (e/* (sq (e/* eps eta)) norm)))
                (e/* 2 Math/PI d eta cross)])))
    :virtual (fn [m1 m2 l1 l2 dq] (fn [_] [(e/* 9.81 dq (e/- (e/* m1 l1) (e/* m2 l2)))
                                               (e/- (e/* m1 l1) (e/* m2 l2))]))
    :pendulum (fn [l g theta omega m] (fn [_] [(e/* m (e/- (e/* 0.5 (sq (e/* l omega))) (e/* g l (e/cos theta))))
                                                     (e/* 2 Math/PI (e/sqrt (e// l g)))]))
    :double (fn [l1 l2 m1 m2 a b da] (fn [_] [(e/- (e/* -9.81 (e/+ m1 m2) l1 (e/cos a))
                                                        (e/* 9.81 m2 l2 (e/cos b)))
                                                     (e/* l1 (e/- (e/sin (e/+ a da)) (e/sin a)))]))
    :kepler (fn [energy h mu] (fn [_] [(eccentricity energy h mu) (e// (sq h) mu)]))
    :mean (fn [k s a b] (fn [_] (let [c (mean-c a b)] [c (e/+ (e/* k (e/+ (sq a) (e/* a b) (sq b))) s)
                                                        (e/+ (e/* 3 k (sq c)) s)])))
    :brach (fn [x y g] (fn [_] [(e/sqrt (e// (e/* 2 (e/+ (sq x) (sq y))) (e/* g y)))]))))

(def readout-labels
  {:delta ["J(ε) = π∫(y + εη − y*)²dt" "J′(0) = 2π∫(y − y*)ηdt"]
   :virtual ["Σ Fᵢ δrᵢ = g(m₁l₁ − m₂l₂)δq" "m₁l₁ − m₂l₂"]
   :pendulum ["E = ½ml²ω² − mgl cos θ" "2π√(l/g), small-angle limit"]
   :double ["initial energy E" "initial bob separation"]
   :kepler ["e = √(1 + 2Eh²/(GM)²)" "p = h²/GM"]
   :mean ["c" "chord slope" "f′(c)"]
   :brach ["line descent time"]})

(defn curve [id]
  (case id
    :delta (fn [q eta eps shape] (fn [t] [t (e/+ (e/* q (e/sin (e/* Math/PI t)))
                                                    (e/* eps eta (variation-shape shape t)))]))
    :mean (fn [k s a b] (fn [t] [t (e/+ (e/* k (e/expt t 3)) (e/* s t))]))
    :brach (fn [x y g] (fn [t] [(e/* x t) (e/* y t)]))
    nil))

(defn- bind-p
  "A quoted browser form with the symbol !p replaced by the state atom. Left
   bare, !p is a Clojure local the browser's SCI cannot resolve."
  [form !p]
  (walk/postwalk-replace {'!p !p} form))

(defn- case-form [id !p]
  (bind-p
   (case id
     :delta '(if (< (js/Math.abs (- (get @!p :q) 1)) 0.005) "stationary extremal: δJ = 0 for every η" "non-extremal: δJ ≠ 0")
     :virtual '(if (< (js/Math.abs (aget r 1)) 0.02) "equilibrium: Σ Fᵢ δrᵢ = 0" "not in equilibrium")
     :pendulum '(if (> (aget r 0) (* (get @!p :m) (get @!p :g) (get @!p :l))) "rotation" "libration")
     :double '(if (< (js/Math.abs (get @!p :a)) 0.4) "regular: nearby traces remain near" "chaotic: nearby traces diverge")
     :kepler '(cond (< (aget r 0) 0.995) "ellipse" (> (aget r 0) 1.005) "hyperbola" :else "parabola")
     :mean '(if (>= (get @!p :a) (get @!p :b)) "degenerate: a must be less than b" (if (< (get @!p :k) 0.001) "line: every c works" "parallel tangent exists"))
     :brach '(if (> (get @!p :y) (get @!p :x)) "steep descent" "shallow descent"))
   !p))

(defn- draggable [!p key init x->v v->point constrain]
  (let [v (gensym "drag")]
    (list 'reagent.core/with-let
          [v (list 'atom (v->point init))
           '_ (list 'add-watch v :to-panel
                    (list 'fn ['_ '_ '_ 'pos]
                          (list 'when (list 'not= (list x->v 'pos) (list 'get (list 'deref !p) key))
                                (list 'swap! !p 'assoc key (list x->v 'pos)))))
           '_ (list 'add-watch !p key
                    (list 'fn ['_ '_ 'old 'new]
                          (list 'when (list 'not= (list 'get 'old key) (list 'get 'new key))
                                (list 'when (list 'not= (list 'deref v) (v->point (list 'get 'new key)))
                                      (list 'reset! v (v->point (list 'get 'new key)))))))]
          (mafs/movable-point {:atom v :color :red :constrain constrain})
          (list 'finally (list 'remove-watch v :to-panel) (list 'remove-watch !p key)))))

(defn- mean-figure [!p xy]
  (let [f (fn [k s x] (e/+ (e/* k (e/expt x 3)) (e/* s x)))
        chord (fn [k s a b] (fn [t]
                [t (e/+ (f k s a)
                        (e/* (e// (e/- (f k s b) (f k s a)) (e/+ 1e-8 (e/- b a))) (e/- t a)))]))
        tangent (fn [k s a b] (fn [t]
                  (let [c (mean-c a b)]
                    [t (e/+ (f k s c) (e/* (e/+ (e/* 3 k (sq c)) s) (e/- t c)))])))
        params {:atom !p :params [:k :s :a :b]}]
    (mafs/mafs {:view-box {:x [-0.2 2.2] :y [-1 5]} :height 340}
               (mafs/cartesian)
               (mafs/parametric {:xy xy :t [0 2] :color :blue})
               (mafs/parametric {:xy (ev/with-params params chord) :t [0 2] :color :red})
               (mafs/parametric {:xy (ev/with-params params tangent) :t [0 2] :color :green :style "dashed"})
               (draggable !p :a 0.2 'first (fn [v] [v 0]) '(fn [[x _]] [(max 0 (min 1.5 x)) 0]))
               (draggable !p :b 1.4 'first (fn [v] [v 0]) '(fn [[x _]] [(max 0.1 (min 2 x)) 0]))
               (mafs/text "a" {:x (ev/get !p :a) :y -0.25})
               (mafs/text "b" {:x (ev/get !p :b) :y -0.25}))))

(defn- virtual-figure [!p]
  ;; a weight m hangs at depth h0 + s·m below its end of the lever. With
  ;; the weight drawn at s·m alone, the m₂ handle (m₂ = 1) sat 16 px under
  ;; the l₂ handle, inside Mafs' 30 px hitbox, and took every press meant
  ;; for l₂; h0 keeps them apart even at m = 0. The weight handles stand at
  ;; the lever's ends (−l₁, l₂), not at the default arm lengths.
  (let [x (fn [k] (ev/get !p k))
        h0 0.6
        s 0.3
        depth (fn [m] (if (number? m) (- (+ h0 (* s m))) (list '- (list '+ h0 (list '* s m)))))
        mass-of (list 'fn '[[_ py]] (list 'max 0 (list 'min 5 (list '/ (list '- (list '- 'py) h0) s))))]
    (mafs/mafs {:view-box {:x [-3.5 3.5] :y [-2.6 1]} :height 340}
               (mafs/cartesian)
               (mafs/segment {:point1 [(list '- (x :l1)) 0] :point2 [(x :l2) 0] :color :blue :weight 4})
               (mafs/segment {:point1 [0 -0.12] :point2 [0 0.12] :color :green})
               (mafs/segment {:point1 [(list '- (x :l1)) 0] :point2 [(list '- (x :l1)) (depth (x :m1))] :color :red})
               (mafs/segment {:point1 [(x :l2) 0] :point2 [(x :l2) (depth (x :m2))] :color :red})
               (draggable !p :l1 1 '(fn [[px _]] (max 0.1 (min 3 (- px))))
                          (fn [v] [(if (number? v) (- v) (list '- v)) 0]) '(fn [[px _]] [(max -3 (min -0.1 px)) 0]))
               (draggable !p :l2 2 '(fn [[px _]] (max 0.1 (min 3 px)))
                          (fn [v] [v 0]) '(fn [[px _]] [(max 0.1 (min 3 px)) 0]))
               (draggable !p :m1 2 mass-of
                          (fn [v] [(list '- (x :l1)) (depth v)])
                          (list 'fn '[[px py]] [(list '- (x :l1)) (list 'max (- (+ h0 (* 5 s))) (list 'min (- h0) 'py))]))
               (draggable !p :m2 1 mass-of
                          (fn [v] [(x :l2) (depth v)])
                          (list 'fn '[[px py]] [(x :l2) (list 'max (- (+ h0 (* 5 s))) (list 'min (- h0) 'py))]))
               (mafs/text "m₁" {:x (list '- (x :l1)) :y -2.4})
               (mafs/text "m₂" {:x (x :l2) :y -2.4}))))

(defn- delta-figure [!p xy]
  (let [coeff @variation-coefficients
        cross (fn [shape] (reduce e/+ (map-indexed (fn [i v] (e/* (:cross v) (e/- 1 (e/abs (e/- shape i))))) coeff)))
        norm (fn [shape] (reduce e/+ (map-indexed (fn [i v] (e/* (:norm v) (e/- 1 (e/abs (e/- shape i))))) coeff)))
        action (fn [q eta eps shape] (fn [t]
                 [t (e/* Math/PI (e/+ (e/* 0.5 (sq (e/- q 1)))
                                        (e/* 2 (e/- q 1) t eta (cross shape))
                                        (e/* (sq (e/* t eta)) (norm shape))))]))
        tangent (fn [q eta eps shape] (fn [t]
                  [t (e/* Math/PI (e/+ (e/* 0.5 (sq (e/- q 1)))
                                         (e/* 2 (e/- q 1) t eta (cross shape))))]))]
    [:div
     (mafs/mafs {:view-box {:x [-0.2 1.2] :y [-1.5 2.5]} :height 290}
                (mafs/cartesian)
                (mafs/parametric {:xy (ev/with-params {:atom !p :params [:q]} (fn [q] (fn [t] [t (e/* q (e/sin (e/* Math/PI t)))]))) :t [0 1] :color :blue})
                (mafs/parametric {:xy xy :t [0 1] :color :red})
                (draggable !p :q 1 'second (fn [v] [0.5 v]) '(fn [[_ y]] [0.5 (max 0 (min 2 y))]))
                (mafs/text "y" {:x 0.2 :y 1})
                (mafs/text "y + εη" {:x 0.7 :y 1.6}))
     (mafs/mafs {:view-box {:x [-1 1] :y [-0.3 6]} :height 210}
                (mafs/cartesian)
                (mafs/parametric {:xy (ev/with-params {:atom !p :params [:q :eta :eps :shape]} action) :t [-1 1] :color :red})
                (mafs/parametric {:xy (ev/with-params {:atom !p :params [:q :eta :eps :shape]} tangent) :t [-1 1] :color :green :style "dashed"})
                (mafs/text "J(ε), tangent at 0" {:x 0.4 :y 5}))]))

(defn playground [id]
  (let [{:keys [start range tries]} (setups id)
        ks (vec (keys start))
        nums (mapv start ks)]
    (binding [vc/*backend* :raster]
      (ev/with-let [!p start]
        (let [[sym compiled] (tie/kernel-binding "readout" (readout id) nums)
              xy (when (curve id) (ev/with-params {:atom !p :params ks} (curve id)))]
          (list 'reagent.core/with-let [sym compiled]
                (list 'let ['r (list sym '(array 0) (tie/state-array #(ev/get !p %) ks))]
                      [:div {:data-figure (str "lagrange-" (name id))}
                       [:div {:style {:display "flex" :gap "1em" :flex-wrap "wrap"}}
                        [:div {:style {:flex "1 1 390px"}}
                         (case id
                           :mean (mean-figure !p xy)
                           :virtual (virtual-figure !p)
                           :delta (delta-figure !p xy)
                           :pendulum (pendulum-figure !p)
                           :double (double-figure !p)
                           :kepler (kepler-figure !p)
                           :brach (brach-figure !p xy))]
                        [:div {:style {:flex "1 1 260px"}}
                         (leva/sub-panel {:fill true :flat true :title-bar false}
                                         (leva/controls {:atom !p :schema (tie/schema ks range (zipmap ks (map name ks)))}))
                         [:div {:data-case (name id) :style {:font-weight 700 :margin "0.5em 0"}} (case-form id !p)]
                         (into [:div {:style {:font-family "monospace"}}]
                               (map-indexed (fn [i label] [:div label " = " (tie/fmt (list 'aget 'r i) 3)]) (readout-labels id)))]]
                       (into [:div [:strong "Try this: "]]
                             (for [[label values] tries]
                               [:button {:type "button" :on-click (list 'fn [] (list 'reset! !p values))} label]))
                       [:button {:type "button" :on-click (list 'fn [] (list 'reset! !p start))} "Reset to the author's setup"]])))))))
