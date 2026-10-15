(ns alexandria.copernicus.orbits
  "Copernicus, De revolutionibus (1543): the planets on circles about the
   Sun, the Earth among them (I.10). Seen from the moving Earth a planet
   that always runs forward seems to stop and go back (V.1): the
   retrograde loops of Ptolemy's epicycle are the Earth's own orbit seen
   from the other side.

   Heliocentric state [ex ey px py L]: the Earth E on the circle of radius
   1 at wE revolutions a year, the planet P on radius a at wP; L is the
   longitude of G = P - E seen from the Earth, carried as a fifth
   coordinate,

     dE/dt = 2 pi wE J E,  dP/dt = 2 pi wP J P,
     dL/dt = (Gx Gy' - Gy Gx') / |G|^2.

   One RK4 step of this flow is a raster kernel in the page
   (alexandria.medium.flow); on the JVM alexandria.raster/ode integrates it.

   Ptolemy's model is the same motion (alexandria.ptolemy.epicycle):
   G = P + (-E). For an outer planet the deferent is the planet's own
   orbit (R = a, w1 = wP) and the epicycle the Earth's, reversed (r = 1,
   w2 = wE); for an inner one the deferent is the Earth's (the mean Sun,
   R = 1, w1 = wE) and the epicycle the planet's orbit (r = a, w2 = wP).
   Emmy proves the two flows give the same apparent rate, and that at the
   nearest point

     lambda' = (a wP - wE) / (a - 1)   (revolutions a year),

   so the planet retrogrades exactly when the inner body is the faster.

   Periods (V.1): the synodic period S (Copernicus' 'parallactic'
   revolution, the Earth lapping the planet) and the sidereal P,

     1/S = 1/E - 1/P  (outer),   1/S = 1/P - 1/E  (inner),

   tied as three controls E, P, S: each solved from the others."
  (:require [alexandria.grade :as grade]
            [alexandria.ptolemy.epicycle :as ep]
            [alexandria.raster :as raster]
            [emmy.env :as e]
            [hive-dsl.result :as r]))

(def tau (* 2 Math/PI))

;; ---------------------------------------------------------------------------
;; Branch-free clamps (raster compiles them)

(defn at-least [x lo] (e/* 0.5 (e/+ x lo (e/abs (e/- x lo)))))
(defn at-most [x hi] (e/* 0.5 (e/- (e/+ x hi) (e/abs (e/- x hi)))))
(defn clamp [x lo hi] (at-most (at-least x lo) hi))
(defn sign* "+1 for x > 0, -1 for x < 0 (x kept off 0)." [x] (let [y (e/+ x 1e-12)] (e// y (e/abs y))))

;; ---------------------------------------------------------------------------
;; The heliocentric flow

(defn flow
  "The derivative of [ex ey px py L] for the speeds wE wP (rev/yr)."
  [wE wP]
  (fn [[ex ey px py _L]]
    (let [aE (e/* tau wE) aP (e/* tau wP)
          dex (e/- 0 (e/* aE ey)) dey (e/* aE ex)
          dpx (e/- 0 (e/* aP py)) dpy (e/* aP px)
          gx (e/- px ex) gy (e/- py ey)
          gxd (e/- dpx dex) gyd (e/- dpy dey)]
      [dex dey dpx dpy (e// (e/- (e/* gx gyd) (e/* gy gxd)) (e/+ (e/square gx) (e/square gy)))])))

(defn start
  "Earth and planet on one line from the Sun, the planet at opposition
   (outer) or inferior conjunction (inner): [1 0 a 0 0]."
  [a]
  (fn [_] [1 0 a 0 0]))

(defn view
  "Positions in the frame centred at O = (1 - f) E (f = 0 the Earth, f = 1
   the Sun), from a state, for outer = 1 (a > 1) or 0: [Sx Sy Ex Ey Px Py
   lambda-deg Gx Gy Cx Cy], C the centre of Ptolemy's epicycle in that
   frame (outer: the Earth plus the planet's heliocentric vector, the
   epicycle the Earth's orbit reversed; inner: the Earth plus the Sun's
   vector, the epicycle the planet's orbit)."
  [f outer]
  (fn [[ex ey px py L]]
    (let [k (e/- 1 f) ox (e/* k ex) oy (e/* k ey)
          Ex (e/- ex ox) Ey (e/- ey oy)
          cx (e/+ Ex (e/* outer px) (e/* (e/- outer 1) ex))
          cy (e/+ Ey (e/* outer py) (e/* (e/- outer 1) ey))]
      [(e/- 0 ox) (e/- 0 oy) Ex Ey (e/- px ox) (e/- py oy)
       (e/* L (/ 180 Math/PI)) (e/- px ex) (e/- py ey) cx cy])))

(defn ptolemy-start
  "Ptolemy's state [Cx Cy u v L] (alexandria.ptolemy.epicycle/flow) for the
   planet of radius a, at his own free scale k (the deferent's radius) and
   with the epicycle m times the size Copernicus' Earth fixes, for outer =
   1 (deferent = the planet's orbit, epicycle = the Earth's, reversed:
   [k 0 -m k/a 0 0], speeds w1 = wP, w2 = wE) or 0 (deferent = the Earth's
   orbit seen as the mean Sun, epicycle = the planet's: [-k 0 m k a 0 0],
   w1 = wE, w2 = wP). The start is opposition or inferior conjunction, as
   in `start`."
  [k a outer m]
  (fn [_]
    (let [s (e// k (e/+ (e/* outer a) (e/- 1 outer)))]
      [(e/* k (e/- (e/* 2 outer) 1)) 0
       (e/* m s (e/- (e/* (e/- 1 outer) a) outer)) 0 0])))

(defn agreement
  "Copernicus' G = P - E from his state [ex ey px py] against Ptolemy's
   planet Q = C + (u, v) from his [cx cy u v]: [Gx Gy Qx Qy sin-delta
   |Q|/|G|], delta the angle between the two lines of sight (0 when the
   two models put the planet in the same place in the sky)."
  [ex ey px py cx cy u v]
  (fn [_]
    (let [gx (e/- px ex) gy (e/- py ey) qx (e/+ cx u) qy (e/+ cy v)
          ng (e/sqrt (e/+ (e/square gx) (e/square gy)))
          nq (e/sqrt (e/+ (e/square qx) (e/square qy)))]
      [gx gy qx qy (e// (e/- (e/* qx gy) (e/* qy gx)) (e/+ (e/* nq ng) 1e-12)) (e// nq (e/+ ng 1e-12))])))

(defn rate
  "From a state, for speeds wE wP: [Gx Gy lambda-dot (rad/yr) lambda-deg]."
  [wE wP]
  (fn [[ex ey px py L]]
    (let [[_ _ _ _ Ld] ((flow wE wP) [ex ey px py L])]
      [(e/- px ex) (e/- py ey) Ld (e/* L (/ 180 Math/PI))])))

(defn closest-rate
  "lambda' at the nearest point (opposition or inferior conjunction), rev/yr:
   (a wP - wE)/(a - 1)."
  [a wE wP]
  (e// (e/- (e/* a wP) wE) (e/- a 1)))

(defn regime
  "[lambda' at the nearest point (deg/day), planet speed a wP, Earth speed
   wE (orbit radii a year), (a wP - wE)(a - 1) (negative: retrograde),
   a - 1, nearest distance |a - 1|, farthest a + 1, Ptolemy's epicycle
   ratio r/R (outer 1/a, inner a)] for a wE wP."
  [a wE wP]
  (fn [_]
    (let [a' (e/+ a (e/* 1e-9 (sign* (e/- a 1))))]
      [(e/* (/ 360 365.25) (closest-rate a' wE wP))
       (e/* a wP) wE
       (e/* (e/- (e/* a wP) wE) (e/- a 1))
       (e/- a 1) (e/abs (e/- a 1)) (e/+ a 1)
       (at-most a (e// 1 a))])))

(def regime-index {:closest 0 :vP 1 :vE 2 :test 3 :a-1 4 :near 5 :far 6 :ratio 7})

(defn sample-at
  "The heliocentric closed form at t (for drawing beside the integration
   and for the gap): [ex ey px py]."
  [a wE wP t]
  (fn [_] [(e/cos (e/* tau wE t)) (e/sin (e/* tau wE t))
           (e/* a (e/cos (e/* tau wP t))) (e/* a (e/sin (e/* tau wP t)))]))

(defn gap
  "Distance from an RK4 planet-from-Earth vector (gx, gy) at time t to the
   closed form."
  [a wE wP t gx gy]
  (fn [_] (let [[ex ey px py] ((sample-at a wE wP t) nil)]
            [(e/sqrt (e/+ (e/square (e/- gx (e/- px ex))) (e/square (e/- gy (e/- py ey)))))])))

;; ---------------------------------------------------------------------------
;; Synodic and sidereal periods, tied

(def tied [:E :P :S])

(def ranges
  "[min max step] in years."
  {:E [0.5 2 0.001] :P [0.1 40 0.001] :S [0.1 60 0.001]})

(def labels {:E "Earth's year E" :P "planet's period P" :S "synodic period S"})

(def ^:private eps 1e-4)

(defn synodic
  "1/S = |1/E - 1/P|, kept off 0 (P = E: no synodic return; S is capped at 60)."
  [E P]
  (e// 1 (at-least (e/abs (e/- (e// 1 E) (e// 1 P))) (/ 1 60.0))))

(defn sidereal
  "P from E and S on the side sgn (+1 outer, -1 inner): 1/P = 1/E - sgn/S,
   kept positive."
  [E S sgn]
  (e// 1 (at-least (e/- (e// 1 E) (e// sgn S)) eps)))

(defn- lo [k] (first (ranges k)))
(defn- hi [k] (second (ranges k)))

(defn- from-E [E P _S] (let [E (clamp E (lo :E) (hi :E))] [E P (synodic E P)]))
(defn- from-P [E P _S] (let [P (clamp P (lo :P) (hi :P))] [E P (synodic E P)]))
(defn- from-S
  "The side (outer or inner) is the planet's current one."
  [E P S]
  (let [S (clamp S (lo :S) (hi :S)) P' (clamp (sidereal E S (sign* (e/- P E))) (lo :P) (hi :P))] [E P' S]))

(defn tie
  "The tie kernel: the solved [E P S] for each source, in the order of `tied`."
  [E P S]
  (fn [_] (vec (mapcat #(% E P S) [from-E from-P from-S]))))

(defn conjunctions
  "Readout for E P S: [1/E 1/P 1/S |1/E - 1/P| - 1/S, side (+1 outer,
   -1 inner), the advance of each conjunction along the planet's orbit in
   degrees (360 S/P), laps of the Earth per synodic period S/E]."
  [E P S]
  (fn [_]
    [(e// 1 E) (e// 1 P) (e// 1 S)
     (e/- (e/abs (e/- (e// 1 E) (e// 1 P))) (e// 1 S))
     (sign* (e/- P E))
     (e/* 360 (e// S P))
     (e// S E)]))

(def tie-index {:invE 0 :invP 1 :invS 2 :residual 3 :side 4 :advance 5 :laps 6})

(defn conj-point
  "The k-th conjunction (opposition for an outer planet, inferior
   conjunction for an inner one) on the planet's circle of radius a: the
   planet has turned 2 pi k S/P since the first: [x y]."
  [P S a k]
  (fn [_] (let [th (e/* tau k (e// S P))] [(e/* a (e/cos th)) (e/* a (e/sin th))])))

(defn kepler-period
  "P = a^(3/2) (Kepler, Harmonices mundi V.3, 1619), for the playground's
   optional rule: [P]."
  [a]
  (fn [_] [(e/* a (e/sqrt a))]))

(defn radius-of
  "a = P^(2/3), the inverse of kepler-period, for drawing a period's
   orbit in the synodic widget: [a]."
  [P]
  (fn [_] [(e/exp (e/* (/ 2 3) (e/log P)))]))

;; ---------------------------------------------------------------------------
;; Copernicus' numbers (De revolutionibus V.1, Menzzer 1879 pp. 255-256)

(defn sexagesimal
  "d;m,s,t (days and sixtieths) as a number."
  [d & parts]
  (reduce + d (map-indexed (fn [i x] (/ x (Math/pow 60 (inc i)))) parts)))

(def year-days
  "The sidereal year (III.14): 365 days 15' 24'' 10''' of a day."
  (sexagesimal 365 15 24 10))

(def copernicus
  "V.1 (Menzzer pp. 255-256): the cycle he states for each planet: in
   `years` sidereal years plus `extra` days the Earth laps the planet
   `laps` times (the parallactic revolutions) while the planet makes
   `revs` whole revolutions (outer: revs = years - laps, inner:
   revs = years + laps). His synodic period is that time over `laps`.
   :table is the period as Menzzer prints it (his list after the cycles;
   Jupiter's 23' is a misprint, the 1543 edition reads LIII = 53').
   Modern sidereal periods for comparison (years), and Ptolemy's epicycle
   in parts of 60 (Almagest IX-XI)."
  [{:id :saturn :name "Saturn" :side :outer :years 59 :extra (sexagesimal 1 6 48) :laps 57 :revs 2
    :table "378d 5′ 32″" :modern 29.457 :ptolemy 6.5}
   {:id :jupiter :name "Jupiter" :side :outer :years 71 :extra (- (sexagesimal 5 45 27)) :laps 65 :revs 6
    :table "398d 53′ 3″" :modern 11.862 :ptolemy 11.5}
   {:id :mars :name "Mars" :side :outer :years 79 :extra (sexagesimal 2 27 3) :laps 37 :revs 42
    :table "779d 56′ 19″" :modern 1.8809 :ptolemy 39.5}
   {:id :venus :name "Venus" :side :inner :years 8 :extra (- (sexagesimal 2 26 46)) :laps 5 :revs 13
    :table "583d 55′ 17″" :modern 0.61520 :ptolemy 43.1667}
   {:id :mercury :name "Mercury" :side :inner :years 46 :extra (sexagesimal 0 34 23) :laps 145 :revs 191
    :table "115d 52′ 42″" :modern 0.24085 :ptolemy 22.5}])

(defn- sgn-of [side] (if (= side :outer) 1 -1))

(defn table
  "Copernicus' cycles (V.1) turned into synodic periods S (days and years),
   then into sidereal periods by raster (1/P = 1/E - sgn/S, E = 1), and
   the radius each planet's Ptolemaic epicycle ratio fixes once the
   epicycle is the Earth's orbit (outer a = 60/r, inner a = r/60)."
  []
  (mapv (fn [{:keys [side years extra laps ptolemy] :as p}]
          (let [S-days (raster/value (fn [y x n] (e// (e/+ (e/* y year-days) x) n)) years extra laps)
                Sy (/ S-days year-days)
                sgn (sgn-of side)
                P (raster/value (fn [s] (e// 1 (e/- 1 (e// sgn s)))) Sy)
                ratio (/ ptolemy 60.0)
                a (raster/value (fn [q] (if (= side :outer) (e// 1 q) (e/* 1 q))) ratio)]
            (assoc p :S-days S-days :S-years Sy :P P :a a :ratio ratio)))
        copernicus))

(defn radii
  "The distances only the moving Earth fixes: from Ptolemy's epicycle
   ratios q (r/R, in parts of 60 over 60) of Saturn, Jupiter, Mars (outer:
   the epicycle is the Earth's orbit, so a = 1/q) and Venus, Mercury
   (inner: the epicycle is the planet's orbit, a = q), and their periods
   P: [a-sat a-jup a-mar a-ven a-mer, then for each the speed 2 pi a/P
   (Earth = 2 pi)]."
  [qs qj qm qv qme Ps Pj Pm Pv Pme]
  (fn [_]
    (let [as [(e// 1 (at-least qs 1e-3)) (e// 1 (at-least qj 1e-3)) (e// 1 (at-least qm 1e-3)) qv qme]]
      (into as (map (fn [a P] (e// (e/* tau a) P)) as [Ps Pj Pm Pv Pme])))))

;; ---------------------------------------------------------------------------
;; Derivations and grades

(def identities
  "[label difference] pairs that Emmy reduces to 0."
  (let [state ['ex 'ey 'px 'py 'L]
        cop ((flow 'wE 'wP) state)
        ;; Ptolemy's outer planet: deferent centre C = P, epicycle vector -E
        pto-out ((ep/flow 'wP 'wE) ['px 'py (e/- 0 'ex) (e/- 0 'ey) 'L])
        ;; inner planet: deferent centre C = -E (the Sun seen from the Earth), epicycle P
        pto-in ((ep/flow 'wE 'wP) [(e/- 0 'ex) (e/- 0 'ey) 'px 'py 'L])]
    [["outer planet: Copernicus' apparent rate equals Ptolemy's with deferent = the planet's orbit, epicycle = the Earth's"
      (e/- (nth cop 4) (nth pto-out 4))]
     ["inner planet: equals Ptolemy's with deferent = the Earth's orbit (the mean Sun), epicycle = the planet's"
      (e/- (nth cop 4) (nth pto-in 4))]
     ["at the nearest point (Earth and planet on one ray from the Sun) λ' = 2π (a w_P − w_E)/(a − 1)"
      (let [ld (nth ((flow 'wE 'wP) [1 0 'a 0 0]) 4)]
        (e/- ld (e/* tau (closest-rate 'a 'wE 'wP))))]
     ["synodic: Earth and planet meet again when 2π(1/E − 1/P) S = 2π, so 1/S = 1/E − 1/P (outer)"
      (e/- (e/* (e/- (e// 1 'E) (e// 1 'P)) (e// 1 (e/- (e// 1 'E) (e// 1 'P)))) 1)]
     ["tie, outer: P → S → P is the identity"
      (let [S (e// 1 (e/- (e// 1 'E) (e// 1 'P)))] (e/- (e// 1 (e/- (e// 1 'E) (e// 1 S))) 'P))]
     ["tie, inner: P → S → P is the identity (1/S = 1/P − 1/E)"
      (let [S (e// 1 (e/- (e// 1 'P) (e// 1 'E)))] (e/- (e// 1 (e/+ (e// 1 'E) (e// 1 S))) 'P))]]))

(defn- variant [res] (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails))

(defn integrate
  "The heliocentric flow by raster's RK4 on the JVM: {:ts :us}."
  [a P years dt]
  (let [f (flow 1 (/ 1 P))]
    (raster/ode (fn [_t ex ey px py L] (f [ex ey px py L])) [1 0 a 0 0] years dt)))

(defn graded []
  (let [rows (table)
        mars (first (filter #(= :mars (:id %)) rows))
        a (:a mars) P (:P mars)
        {:keys [ts us]} (integrate a P 2.0 0.002)
        gaps (map (fn [t [ex ey px py]]
                    (let [[ex' ey' px' py'] (mapv double ((sample-at a 1 (/ 1 P) t) nil))]
                      (Math/hypot (- (- px ex) (- px' ex')) (- (- py ey) (- py' ey')))))
                  ts us)]
    (-> (mapv (fn [[label diff]] {:label label :grade (variant (grade/grade :symbolic [diff]))}) identities)
        (conj {:label "Mars from a moving Earth, integrated two years by raster's RK4, stays on the closed form within 1e-8"
               :grade (variant (grade/grade :numeric gaps 1e-8)) :engine :raster}
              {:label "Copernicus' synodic periods (V.1) give sidereal periods within 0.5% of the modern ones"
               :grade (variant (grade/grade :numeric (map (fn [{:keys [P modern]}] (/ (- P modern) modern)) rows) 0.005))
               :engine :raster}
              {:label "his cycles agree: revolutions = years − laps (outer), years + laps (inner)"
               :grade (variant (grade/grade :numeric (map (fn [{:keys [side years laps revs]}]
                                                            (double (- revs (if (= side :outer) (- years laps) (+ years laps)))))
                                                          rows) 0.5))
               :engine :raster}))))

(def tex
  {:helio "E(t) = (\\cos 2\\pi w_E t, \\sin 2\\pi w_E t),\\quad P(t) = a\\,(\\cos 2\\pi w_P t, \\sin 2\\pi w_P t),\\quad G = P - E"
   :rate "\\dot\\lambda = \\frac{G_x \\dot G_y - G_y \\dot G_x}{|G|^2}"
   :closest "\\dot\\lambda_{\\text{nearest}} = 2\\pi\\,\\frac{a\\,w_P - w_E}{a - 1} < 0 \\iff \\text{the inner body is the faster}"
   :synodic "\\frac1S = \\frac1E - \\frac1P\\ \\text{(outer)},\\qquad \\frac1S = \\frac1P - \\frac1E\\ \\text{(inner)}"
   :ptolemy "\\text{outer: } R = a,\\ r = 1;\\qquad \\text{inner: } R = 1,\\ r = a;\\qquad \\frac rR = \\frac1a \\text{ or } a"})
