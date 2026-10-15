(ns alexandria.ptolemy.epicycle
  "Ptolemy's planets (Almagest IX-XII): a planet P runs on an epicycle whose
   centre C runs on a deferent about the Earth E,

     C(t) = R (cos w1 t, sin w1 t),   P(t) = C(t) + r (cos w2 t, sin w2 t),

   angles in revolutions per year (w2 is the epicycle's own turn, counted
   against the fixed stars, so for an outer planet it is the year). Seen
   from the Earth the planet's longitude lambda = atan2(P) sometimes runs
   backwards: the retrograde arcs of Almagest XII.

   The page INTEGRATES the motion (playground brief): the state
   [Cx Cy u v Lambda] flows under dC/dt = w1 J C, d(u,v)/dt = w2 J (u,v)
   and dLambda/dt = (X Y' - Y X')/(X^2 + Y^2) (P = C + (u,v)), one RK4
   step a raster kernel (alexandria.raster.step). The closed form above is
   drawn beside it, labelled, with the gap.

   Emmy derives the apparent rate in closed form,

     lambda' = (R^2 w1 + r^2 w2 + R r (w1 + w2) cos phi) / (R^2 + r^2 + 2 R r cos phi),
     phi = (w2 - w1) t,

   and at the nearest point (phi = pi) lambda' = (R w1 - r w2)/(R - r): the
   planet retrogrades there exactly when r w2 > R w1."
  (:require [alexandria.grade :as grade]
            [alexandria.raster :as raster]
            [emmy.env :as e]
            [hive-dsl.result :as r]))

(def tau (* 2 Math/PI))

(defn flow
  "The state derivative of [Cx Cy u v Lambda] for speeds w1 w2 (rev/yr)."
  [w1 w2]
  (fn [[cx cy u v _L]]
    (let [a1 (e/* tau w1) a2 (e/* tau w2)
          dcx (e/- 0 (e/* a1 cy)) dcy (e/* a1 cx)
          du (e/- 0 (e/* a2 v)) dv (e/* a2 u)
          X (e/+ cx u) Y (e/+ cy v)
          Xd (e/+ dcx du) Yd (e/+ dcy dv)]
      [dcx dcy du dv (e// (e/- (e/* X Yd) (e/* Y Xd)) (e/+ (e/square X) (e/square Y)))])))

(defn start
  "Planet and epicycle centre on the line of the first point of Aries: [R 0 r 0 0]."
  [R r']
  (fn [_] [R 0 r' 0 0]))

(defn rate
  "From a state, for speeds w1 w2: [X Y lambda-dot (rev/yr -> rad/yr) Lambda-deg]."
  [w1 w2]
  (fn [[cx cy u v L]]
    (let [[_ _ _ _ Ld] ((flow w1 w2) [cx cy u v L])]
      [(e/+ cx u) (e/+ cy v) Ld (e/* L (/ 180 Math/PI))])))

(defn closed
  "The closed form P(t), for drawing beside the integration."
  [R r' w1 w2]
  (fn [t] [(e/+ (e/* R (e/cos (e/* tau w1 t))) (e/* r' (e/cos (e/* tau w2 t))))
           (e/+ (e/* R (e/sin (e/* tau w1 t))) (e/* r' (e/sin (e/* tau w2 t))))]))

(defn gap
  "Distance from an RK4 point (x, y) at time t to the closed form."
  [R r' w1 w2 t x y]
  (fn [_] (let [[cx cy] ((closed R r' w1 w2) t)]
            [(e/sqrt (e/+ (e/square (e/- x cx)) (e/square (e/- y cy))))])))

(defn arc
  "A retrograde arc from its two stations: [width in degrees, duration in days]."
  [L1 L2 t1 t2]
  (fn [_] [(e/- L1 L2) (e/* 365.25 (e/- t2 t1))]))

(defn opposition
  "[lambda' at the nearest point (deg/day), r w2 - R w1, R - r, nearest
   distance, farthest distance] for R r w1 w2."
  [R r' w1 w2]
  (fn [_] [(e/* (/ 360 365.25) (e// (e/- (e/* R w1) (e/* r' w2)) (e/- R r')))
           (e/- (e/* r' w2) (e/* R w1)) (e/- R r') (e/abs (e/- R r')) (e/+ R r')]))

;; ---------------------------------------------------------------------------
;; Ptolemy's planets (Almagest IX-XI, epicycle radius with deferent 60)

(def planets
  "Epicycle radius r for deferent R = 1 (Ptolemy's r/60) and the speeds in
   revolutions per year. Ptolemy (Almagest IX-XI): Mars 39;30, Jupiter
   11;30, Saturn 6;30 parts of 60; Venus 43;10 and Mercury 22;30 on a
   deferent carried with the mean Sun."
  {:mars {:R 1 :r (/ 39.5 60) :w1 (/ 1 1.8809) :w2 1 :years 4}
   :jupiter {:R 1 :r (/ 11.5 60) :w1 (/ 1 11.862) :w2 1 :years 4}
   :saturn {:R 1 :r (/ 6.5 60) :w1 (/ 1 29.457) :w2 1 :years 4}
   :venus {:R 1 :r (/ 43.1667 60) :w1 1 :w2 (/ 1 0.61520) :years 4}
   :mercury {:R 1 :r (/ 22.5 60) :w1 1 :w2 (/ 1 0.24085) :years 2}})

;; ---------------------------------------------------------------------------
;; Derivation, grades

(defn- lambda-dot-closed
  "lambda' of P(t) = R (cos k1 t, sin k1 t) + r (cos k2 t, sin k2 t), by
   Emmy's derivative (angular speeds k in radians per year)."
  [R r' k1 k2 t]
  (let [P (fn [t] [(e/+ (e/* R (e/cos (e/* k1 t))) (e/* r' (e/cos (e/* k2 t))))
                   (e/+ (e/* R (e/sin (e/* k1 t))) (e/* r' (e/sin (e/* k2 t))))])
        [x y] (P t)
        [xd yd] ((e/D P) t)]
    (e// (e/- (e/* x yd) (e/* y xd)) (e/+ (e/square x) (e/square y)))))

(def identities
  "Angular speeds k1 = 2 pi w1, k2 = 2 pi w2 and the angles u = k1 t,
   v = k2 t kept symbolic; cos phi = cos u cos v + sin u sin v. The rate
   is proved in two steps that do not depend on how Emmy's simplifier
   chooses to write cos(u - v): Emmy's derivative of P equals the velocity
   written out, and the cross product over |P|^2, cleared of its
   denominator, is the stated rate."
  (let [P (fn [t] [(e/+ (e/* 'R (e/cos (e/* 'k1 t))) (e/* 'r (e/cos (e/* 'k2 t))))
                   (e/+ (e/* 'R (e/sin (e/* 'k1 t))) (e/* 'r (e/sin (e/* 'k2 t))))])
        [xd yd] ((e/D P) 't)
        u (e/* 'k1 't) v (e/* 'k2 't)
        vel [(e/- 0 (e/+ (e/* 'R 'k1 (e/sin u)) (e/* 'r 'k2 (e/sin v))))
             (e/+ (e/* 'R 'k1 (e/cos u)) (e/* 'r 'k2 (e/cos v)))]
        x (e/+ (e/* 'R (e/cos 'u)) (e/* 'r (e/cos 'v))) y (e/+ (e/* 'R (e/sin 'u)) (e/* 'r (e/sin 'v)))
        xv (e/- 0 (e/+ (e/* 'R 'k1 (e/sin 'u)) (e/* 'r 'k2 (e/sin 'v))))
        yv (e/+ (e/* 'R 'k1 (e/cos 'u)) (e/* 'r 'k2 (e/cos 'v)))
        cphi (e/+ (e/* (e/cos 'u) (e/cos 'v)) (e/* (e/sin 'u) (e/sin 'v)))]
    [["the velocity of P(t) = R e^{i k1 t} + r e^{i k2 t}, by Emmy's derivative"
      (e/+ (e/square (e/- xd (first vel))) (e/square (e/- yd (second vel))))]
     ["|P|² = R² + r² + 2 R r cos φ, φ = (k2 − k1) t"
      (e/- (e/+ (e/square x) (e/square y)) (e/+ (e/square 'R) (e/square 'r) (e/* 2 'R 'r cphi)))]
     ["apparent rate: (x y' − y x') = R² k1 + r² k2 + R r (k1 + k2) cos φ, so λ' = that over |P|²"
      (e/- (e/- (e/* x yv) (e/* y xv))
           (e/+ (e/* (e/square 'R) 'k1) (e/* (e/square 'r) 'k2) (e/* 'R 'r (e/+ 'k1 'k2) cphi)))]
     ["at the nearest point (cos φ = −1): λ' = (R k1 − r k2)/(R − r)"
      (e/- (e// (e/+ (e/* (e/square 'R) 'k1) (e/* (e/square 'r) 'k2) (e/* -1 'R 'r (e/+ 'k1 'k2)))
                (e/+ (e/square 'R) (e/square 'r) (e/* -2 'R 'r)))
           (e// (e/- (e/* 'R 'k1) (e/* 'r 'k2)) (e/- 'R 'r)))]]))

(defn- variant [res] (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails))

(defn integrate
  "Mars by raster's RK4 on the JVM (alexandria.raster/ode) over `years`:
   {:ts :us}."
  [{:keys [R r w1 w2]} years dt]
  (let [f (flow w1 w2)]
    (raster/ode (fn [_t cx cy u v L] (f [cx cy u v L])) [R 0 r 0 0] years dt)))

(defn graded []
  (let [{:keys [R r w1 w2] :as mars} (planets :mars)
        {:keys [ts us]} (integrate mars 2.0 0.002)
        gaps (map (fn [t [cx cy u v]]
                    (let [[x y] (mapv double ((closed R r w1 w2) t))]
                      (Math/hypot (- (+ cx u) x) (- (+ cy v) y))))
                  ts us)]
    (-> (mapv (fn [[label diff]] {:label label :grade (variant (grade/grade :symbolic [diff]))}) identities)
        (conj {:label "Mars integrated two years by raster's RK4 stays on the closed form within 1e-8"
               :grade (variant (grade/grade :numeric gaps 1e-8)) :engine :raster}
              {:label "Mars: r w2 > R w1, so the planet retrogrades at opposition (Almagest XII)"
               :grade (variant (grade/grade :numeric [(if (> (* r w2) (* R w1)) 0.0 1.0)] 0.5)) :engine :raster}))))

(def tex
  {:model "P(t) = R\\,(\\cos 2\\pi w_1 t, \\sin 2\\pi w_1 t) + r\\,(\\cos 2\\pi w_2 t, \\sin 2\\pi w_2 t)"
   :rate "\\dot\\lambda = 2\\pi\\,\\frac{R^2 w_1 + r^2 w_2 + R r (w_1 + w_2)\\cos\\varphi}{R^2 + r^2 + 2Rr\\cos\\varphi}"
   :opposition "\\dot\\lambda_{\\text{nearest}} = 2\\pi\\,\\frac{R w_1 - r w_2}{R - r} < 0 \\iff r\\,w_2 > R\\,w_1"})
