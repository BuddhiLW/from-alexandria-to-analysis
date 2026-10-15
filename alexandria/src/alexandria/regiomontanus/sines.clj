(ns alexandria.regiomontanus.sines
  "From Ptolemy's chord to the Indian half-chord, the sine.

   Ptolemy (Almagest I.10) tabulates the chord of an arc in parts of a
   diameter of 120. Aryabhata (Aryabhatiya, 499, Ganitapada 12 and
   Dasagitika 10) tabulates the half-chord of the double arc, jya (later
   jiva, Arabic jiba, Latin sinus), in minutes of arc, on a radius of 3438
   minutes: the radius measured in the units of its own circumference
   (21600 / 2 pi = 3437.75). Both are one segment:

     crd(2a) = 2 R sin a        jya(a) = crd(2a) / 2 = R sin a.

   This namespace holds:
   - the TIE: arc 2a (degrees), Ptolemy's chord (parts of 120), Aryabhata's
     jya (minutes, R = 3438) and the sine (radius 1), four Leva controls
     solved from whichever one moves (forward and inverse raster kernels,
     atan2 forms for the inverse, branch-free clamps);
   - Aryabhata's 24 sines at steps of 3 deg 45 min (225 minutes) by his rule
     of differences, one raster STEP kernel (d, s) -> (d', s') iterated in
     the page: the next difference is the previous one less s / divisor.
     Aryabhata's divisor is the first sine, 225; the exact divisor is
     1 / (2 - 2 cos h) = 233.5. His printed table (the differences of
     Dasagitika 10, summed) is beside both, and raster's 3438 sin;
   - the Emmy identities behind both, graded."
  (:require [alexandria.grade :as grade]
            [alexandria.raster :as raster]
            [emmy.env :as e]
            [hive-dsl.result :as r]))

;; ---------------------------------------------------------------------------
;; Units

(def ptolemy-radius "Ptolemy's radius, in parts (diameter 120)." 60)
(def aryabhata-radius "Aryabhata's radius, in minutes of arc." 3438)
(def step-minutes "Aryabhata's step, 3 deg 45 min, in minutes." 225)
(def step-degrees (/ step-minutes 60))

(defn rad [deg] (e/* deg (/ Math/PI 180)))
(defn deg [r] (e/* r (/ 180 Math/PI)))

;; branch-free clamps (as alexandria.medium.tie, which the core cannot require)
(defn at-least [x lo] (e/* 0.5 (e/+ x lo (e/abs (e/- x lo)))))
(defn at-most [x hi] (e/* 0.5 (e/- (e/+ x hi) (e/abs (e/- x hi)))))
(defn clamp [x lo hi] (at-most (at-least x lo) hi))

(defn asin*
  "arcsin as atan2, finite at 1 (raster-friendly)."
  [x]
  (e/atan x (e/sqrt (at-least (e/- 1 (e/square x)) 0))))

;; ---------------------------------------------------------------------------
;; The tie: arc, chord, jya, sine

(def tied [:arc :crd :jya :sin])

(def ranges
  "[min max step]: arc 2a in [1, 179] degrees and its images."
  (let [s (fn [d] (Math/sin (Math/toRadians (/ d 2))))]
    {:arc [1 179 0.25]
     :crd [(* 120 (s 1)) (* 120 (s 179)) 0.001]
     :jya [(* 3438 (s 1)) (* 3438 (s 179)) 0.1]
     :sin [(s 1) (s 179) 0.0001]}))

(def labels {:arc "arc 2a (°)" :crd "crd 2a (parts of 120)" :jya "jya a (minutes, R 3438)" :sin "sin a"})

(defn- lo [k] (first (ranges k)))
(defn- hi [k] (second (ranges k)))

(defn of-arc
  "[arc crd jya sin] from the arc 2a in degrees."
  [arc]
  (let [s (e/sin (e/* arc (/ Math/PI 360)))]
    [arc (e/* 2 ptolemy-radius s) (e/* aryabhata-radius s) s]))

(defn arc-of-sin "The arc 2a (degrees) whose half has sine s." [s] (e/* 2 (deg (asin* s))))

(defn- from-arc [arc _ _ _] (of-arc (clamp arc (lo :arc) (hi :arc))))
(defn- from-crd [_ crd _ _] (of-arc (arc-of-sin (e/divide (clamp crd (lo :crd) (hi :crd)) (* 2 ptolemy-radius)))))
(defn- from-jya [_ _ jya _] (of-arc (arc-of-sin (e/divide (clamp jya (lo :jya) (hi :jya)) aryabhata-radius))))
(defn- from-sin [_ _ _ s] (of-arc (arc-of-sin (clamp s (lo :sin) (hi :sin)))))

(defn tie
  "The tie kernel: the solved [arc crd jya sin] for each source, in the
   order of `tied` (16 numbers)."
  [arc crd jya s]
  (fn [_] (vec (mapcat #(% arc crd jya s) [from-arc from-crd from-jya from-sin]))))

(defn figure
  "The circle readout for the arc 2a (degrees), radius 1, the arc centred
   on the x axis: [Px Py Qx Qy Mx crd/2-on-unit sin a cos a (1 - cos a)].
   P = (cos a, sin a), Q = (cos a, -sin a), M the foot of the half-chord."
  [arc]
  (fn [_]
    (let [a (e/* arc (/ Math/PI 360))
          c (e/cos a) s (e/sin a)]
      [c s c (e/- s) c s s c (e/- 1 c)])))

(def figure-index {:Px 0 :Py 1 :Qx 2 :Qy 3 :Mx 4 :half 5 :sin 6 :cos 7 :versin 8})

(defn arc-of-point
  "The arc 2a (degrees) of the point (x, y) dragged on the figure: twice
   its polar angle, |y| so either end of the chord drags it."
  [x y]
  (fn [_] [(clamp (e/* 2 (deg (e/atan (e/abs y) x))) (lo :arc) (hi :arc))]))

;; ---------------------------------------------------------------------------
;; Aryabhata's table

(def printed-differences
  "Aryabhata's 24 sine differences (Dasagitika 10, in the order of the
   arcs 3 deg 45 min, 7 deg 30 min, ... 90 deg), in minutes: Clark (1930),
   p. 19; the numbers are encoded in his alphabet (makhi bhakhi ...)."
  [225 224 222 219 215 210 205 199 191 183 174 164 154 143 131 119 106 93 79 65 51 37 22 7])

(def printed-sines "The printed differences summed: Aryabhata's table." (vec (reductions + printed-differences)))

(def exact-divisor
  "The divisor that makes the rule exact: 1 / (2 - 2 cos 3 deg 45 min)."
  (/ 1 (- 2 (* 2 (Math/cos (Math/toRadians 3.75))))))

(defn difference-step
  "One step of the rule of Ganitapada 12, as a raster kernel state map:
   state [n d s] (n sines made, the difference d that made the last one,
   the last sine s) -> [n+1 d' s'] with d' = d - s / divisor, s' = s + d'.
   The first difference is the first sine (n = 0, d = 225 + s0/divisor)."
  [divisor]
  (fn [[n d s]]
    (let [d' (e/- d (e/divide s divisor))]
      [(e/+ n 1) d' (e/+ s d')])))

(defn first-sine
  "The first sine the rule starts from: Aryabhata's 225 with his divisor,
   3438 sin 3 deg 45 min (224.856) with the exact one."
  [divisor]
  (if (== divisor step-minutes) step-minutes (* aryabhata-radius (Math/sin (Math/toRadians 3.75)))))

(defn rule-table
  "The 24 sines by the rule with `divisor` (Aryabhata's 225 or the exact
   233.5), from `first-sine`: [{:n :arc :d :s}] by the raster step kernel,
   iterated from the state [0 first 0] (the first d' is the first sine)."
  [divisor]
  (let [step (raster/kernels (fn [n d s] ((difference-step divisor) [n d s])) 3)
        f (fn [[n d s]] (mapv #(% n d s) step))]
    (->> (iterate f [0.0 (double (first-sine divisor)) 0.0])
         rest (take 24)
         (mapv (fn [[n d s]] {:n (long (Math/round (double n))) :arc (* n step-degrees) :d d :s s})))))

(defn true-sines "3438 sin(n . 3 deg 45 min), n = 1..24, by raster." []
  (vec (raster/sample (fn [n] (e/* aryabhata-radius (e/sin (rad (e/* n step-degrees))))) (mapv (comp vector double) (range 1 25)))))

(defn table
  "Each of the 24 arcs: Aryabhata's printed sine, the rule with his
   divisor 225, the rule with the exact divisor, 3438 sin by raster, and
   the errors of each against raster."
  []
  (let [r225 (rule-table step-minutes) rex (rule-table exact-divisor) tru (true-sines)]
    (mapv (fn [i]
            (let [t (tru i)]
              {:n (inc i) :arc (* (inc i) step-degrees) :printed (printed-sines i) :printed-d (printed-differences i)
               :rule (:s (r225 i)) :rule-d (:d (r225 i)) :exact (:s (rex i)) :raster t
               :printed-error (- (printed-sines i) t) :rule-error (- (:s (r225 i)) t) :exact-error (- (:s (rex i)) t)}))
          (range 24))))

;; ---------------------------------------------------------------------------
;; Emmy: identities and grades

(defn dist2 [[x0 y0] [x1 y1]] (e/+ (e/square (e/- x1 x0)) (e/square (e/- y1 y0))))

(def identities
  "[label difference] pairs reduced to 0 by Emmy."
  (let [u (e/cos 'p) v (e/sin 'p) c (e/cos 'a) s (e/sin 'a)
        c2 (e/- (e/square c) (e/square s)) s2 (e/* 2 s c)
        Q [(e/- (e/* u c2) (e/* v s2)) (e/+ (e/* u s2) (e/* v c2))]]
    [["crd(2a) = 2R sin a: the chord from P to P turned by 2a, anywhere on the circle (radius 1)"
      (e/- (dist2 [u v] Q) (e/square (e/* 2 s)))]
     ["the half-chord is the sine: jya a = crd(2a)/2 = R sin a, for Ptolemy's R = 60 and Aryabhata's R = 3438 alike"
      (e/- (e/divide (e/* 2 'R s) 2) (e/* 'R s))]
     ["forward then inverse: 120 sin(arcsin(k/120)) = k (from the chord back to the arc and forward again)"
      (e/- (e/* 120 (e/sin (e/asin (e/divide 'k 120)))) 'k)]
     ["forward then inverse: 3438 sin(arcsin(j/3438)) = j (the same for Aryabhata's jya)"
      (e/- (e/* 3438 (e/sin (e/asin (e/divide 'j 3438)))) 'j)]
     ["second differences: sin(x+h) - 2 sin x + sin(x-h) = -(2 - 2 cos h) sin x (addition theorem written out)"
      (let [S 'S C 'C sh 'sh ch 'ch]
        (e/- (e/+ (e/+ (e/* S ch) (e/* C sh)) (e/* -2 S) (e/- (e/* S ch) (e/* C sh)))
             (e/* -1 (e/- 2 (e/* 2 ch)) S)))]]))

(def tex
  "The relations, as TeX (the derivation shows them beside the grades)."
  {:chord "\\operatorname{crd}(2a) = 2R\\sin a,\\qquad \\operatorname{jya}(a) = \\tfrac12\\operatorname{crd}(2a) = R\\sin a"
   :units "R_{\\text{Ptolemy}} = 60^{p},\\qquad R_{\\text{Aryabhata}} = \\frac{21600'}{2\\pi} \\approx 3438'"
   :inverse "2a = 2\\arcsin\\frac{\\operatorname{crd}}{120} = 2\\arcsin\\frac{\\operatorname{jya}}{3438}"
   :rule "d_{n+1} = d_n - \\frac{s_n}{225},\\qquad s_{n+1} = s_n + d_{n+1}"
   :exact "s_{n+1} - 2s_n + s_{n-1} = -(2 - 2\\cos h)\\,s_n,\\qquad \\frac{1}{2-2\\cos 3^\\circ 45'} = 233.5"})

(defn derived-tex
  "Emmy's own TeX of the relations it derives (->TeX of simplified forms)."
  []
  {:chord (e/->TeX (e/simplify (e/* 2 'R (e/sin 'a))))
   :inverse (e/->TeX (e/simplify (e/* 2 (e/asin (e/divide 'k (e/* 2 'R))))))
   :second (e/->TeX (e/simplify (e/* -1 (e/- 2 (e/* 2 (e/cos 'h))) (e/sin 'x))))})

(defn- variant [res] (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails))

(defn tie-round-trip
  "Max |state - solved state| over the four sources at a grid of arcs (raster)."
  []
  (let [k (fn [& xs] ((apply tie xs) 0))]
    (apply max
           (for [arc [5.0 37.5 72.0 120.0 166.0]
                 :let [st (raster/value (fn [a] (of-arc a)) arc)
                       out (apply raster/value k st)]
                 i (range 4) j (range 4)]
             (Math/abs (- (double (nth out (+ (* 4 i) j))) (double (nth st j))))))))

(defn graded
  "[{:label :grade}]: the identities (proved), the tie round trip and the
   tables (numeric, raster)."
  []
  (let [rows (table)]
    (-> (mapv (fn [[label diff]] {:label label :grade (variant (grade/grade :symbolic [diff]))}) identities)
        (conj {:label "the tie: every source solves back to the same arc, chord, jya and sine (raster, within 1e-9)"
               :grade (variant (grade/grade :numeric [(tie-round-trip)] 1e-9)) :engine :raster}
              {:label "Aryabhata's printed table is 3438 sin to within 1.5 minutes at every one of the 24 arcs (raster)"
               :grade (variant (grade/grade :numeric (map #(/ (:printed-error %) 1.5) rows) 1.0)) :engine :raster}
              {:label "the rule with the exact divisor 233.5, started from 3438 sin 3°45′, IS 3438 sin at all 24 arcs (raster, within 1e-6)"
               :grade (variant (grade/grade :numeric (map :exact-error rows) 1e-6)) :engine :raster}))))
