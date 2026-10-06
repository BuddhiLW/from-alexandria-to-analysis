(ns alexandria.kepler.orbit
  "Kepler's laws: Astronomia Nova (Prague, 1609) and Harmonices Mundi
   (Linz, 1619).

     first law   the planet moves on an ellipse with the Sun at a focus
     second law  the line Sun-planet sweeps equal areas in equal times
     equation    M = E - e sin E: mean anomaly M (time, as an angle), eccentric
                 anomaly E (the angle at the centre of the ellipse's
                 circumscribed circle), eccentricity e
     third law   T^2 / a^3 is the same for every planet (period T, mean
                 distance a)

   The ellipse is x = a cos E, y = b sin E, b = a sqrt(1 - e^2), centre at
   the origin and the Sun at the focus (a e, 0). Emmy proves:
     - the distance Sun-planet is a (1 - e cos E) (first law, the focal
       property; Apollonius III.52 has the sum of the focal distances = 2a);
     - the area swept from perihelion, written through E, is
       (a b / 2)(E - e sin E): its E-derivative equals the sweep rate of the
       Sun-planet line, and it is 0 at E = 0; so with M = E - e sin E the
       area is (a b / 2) M, linear in M, i.e. in time (second law);
     - over E = 2 pi the area is pi a b, the whole ellipse.
   Kepler's equation has no closed-form inverse: E(M) is found numerically,
   by raster's Brent on the JVM (Emmy writes the equation, raster solves it;
   graded numeric) and by fixed-point iteration E <- M + e sin E inside the
   raster kernel that moves the planet. The equal-area check integrates the
   sweep rate with raster's Gauss-Kronrod quadrature.

   Data: Mars' eccentricity 0.09264 and mean distance 1.52350 as Kepler
   found them (Dreyer 1906, p. 389 n. 1; Newton, Principia III, Phaenomenon
   IV, Kepler's column). The third law runs over Newton's table (Principia
   III, Phaenomenon IV, Motte's translation): the periods and Kepler's mean
   distances of the six planets.

   Reuse searched: Emmy has the derivative (emmy.env/D) and simplification;
   raster has root finding (raster.sci.roots/brent) and quadrature
   (raster.sci.quadrature/quadgk), reached through alexandria.raster;
   no orbit code in Emmy, desargues or raster."
  (:require [alexandria.grade :as grade]
            [alexandria.proofs :as proofs]
            [alexandria.raster :as raster]
            [emmy.env :as e]
            [hive-dsl.result :as r]
            [alexandria.vocab :as vocab]))

;; ---------------------------------------------------------------------------
;; The ellipse, through the eccentric anomaly

(defn minor
  "b = a sqrt(1 - e^2)."
  [a ecc]
  (e/* a (e/sqrt (e/- 1 (e/square ecc)))))

(defn position
  "The planet at eccentric anomaly E on the ellipse of semi-axis a and
   eccentricity ecc, centre at the origin: [a cos E, b sin E]."
  [a ecc E]
  [(e/* a (e/cos E)) (e/* (minor a ecc) (e/sin E))])

(defn sun
  "The occupied focus, (a e, 0)."
  [a ecc]
  [(e/* a ecc) 0])

(defn mean-anomaly
  "Kepler's equation: M = E - e sin E."
  [ecc E]
  (e/- E (e/* ecc (e/sin E))))

(defn swept-area
  "The area swept from perihelion to E, written through E:
   (a b / 2)(E - e sin E)."
  [a ecc E]
  (e/* (e// (e/* a (minor a ecc)) 2) (mean-anomaly ecc E)))

(defn- sweep-rate
  "dA/dE of the line from the Sun to the planet: half the cross product of
   (planet - sun) and d(planet)/dE."
  [a ecc E]
  (let [[sx sy] (sun a ecc)
        [x y] (position a ecc E)
        [dx dy] ((e/D (fn [t] (position a ecc t))) E)]
    (e/* 1/2 (e/- (e/* (e/- x sx) dy) (e/* (e/- y sy) dx)))))

(defn- distance-sq [[x0 y0] [x1 y1]]
  (e/+ (e/square (e/- x1 x0)) (e/square (e/- y1 y0))))

(def identities
  "[label difference] pairs, each exactly 0, a e E symbolic."
  (let [a 'a ecc 'e E 'E
        p (position a ecc E)
        s (sun a ecc)
        other [(e/- (e/* a ecc)) 0]]
    [["first law: the Sun-planet distance is a (1 - e cos E)"
      (e/- (distance-sq s p) (e/square (e/* a (e/- 1 (e/* ecc (e/cos E))))))]
     ["the other focus: distance a (1 + e cos E), so the two sum to 2a (Apollonius III.52)"
      (e/- (distance-sq other p) (e/square (e/* a (e/+ 1 (e/* ecc (e/cos E))))))]
     ["second law: d/dE of (ab/2)(E - e sin E) is the rate the Sun-planet line sweeps"
      (e/- ((e/D (fn [t] (swept-area a ecc t))) E) (sweep-rate a ecc E))]
     ["no area at perihelion: A(0) = 0" (swept-area a ecc 0)]
     ["so A = (ab/2) M: linear in the mean anomaly M = E - e sin E"
      (e/- (swept-area a ecc E) (e/* (e// (e/* a (minor a ecc)) 2) (mean-anomaly ecc E)))]
     ["one revolution, E = 2 pi, sweeps the whole ellipse pi a b"
      (e/- (swept-area a ecc (e/* 2 'pi)) (e/* 'pi a (minor a ecc)))]]))

;; The vocabulary: (ellipse a e) is the orbit, (sector orbit E0 E1) the area
;; the Sun-planet line sweeps between eccentric anomalies E0 and E1.

(defmethod vocab/realize 'ellipse [_ [a ecc] _env] {:a a :e ecc})

(defmethod vocab/realize 'sector [_ [{:keys [a e]} E0 E1] _env]
  (e/- (swept-area a e E1) (swept-area a e E0)))

;; ---------------------------------------------------------------------------
;; Kepler's equation, solved

(def mars
  "Kepler's Mars: eccentricity and mean distance (Earth = 1)."
  {:e 0.09264 :a 1.52350})

(defn eccentric-anomaly
  "E with E - e sin E = M, by raster's Brent (alexandria.raster/root) on
   [M - e, M + e]: Kepler's equation, written by Emmy (mean-anomaly) and
   lowered to a raster kernel. The root lies in that bracket since
   |E - M| = |e sin E| <= e."
  [ecc M]
  (let [ecc (double ecc) M (double M)]
    (if (zero? ecc)
      M
      (:value (raster/root (fn [E] (e/- (mean-anomaly ecc E) M)) (- M ecc) (+ M ecc))))))

(defn kepler-iterate
  "E after n fixed-point steps E <- M + e sin E from E = M: an Emmy
   expression of M and e, the form a raster kernel compiles (it contracts
   by e < 1 each step)."
  ([ecc M] (kepler-iterate ecc M 12))
  ([ecc M n] (nth (iterate (fn [E] (e/+ M (e/* ecc (e/sin E)))) M) n)))

(def kernel-steps
  "Fixed-point steps the planet kernel takes: each one multiplies the error
   by at most e, so 6 steps leave Mars within e^6 < 1e-6 rad. The
   expression doubles with each step (E appears in M + e sin E), so more
   steps cost compile time and gain nothing on screen."
  6)

(defn planet
  "Figure: (fn [ecc a] (fn [[m]] [x y])), the planet at mean anomaly m
   (radians), centre at the origin."
  [ecc a]
  (fn [[m]] (position a ecc (kepler-iterate ecc m kernel-steps))))

(defn circle-figure
  "Figure: the circle of radius a about the centre, (fn [a] (fn [[t]] [x y])):
   the orbit Kepler fitted first, with its equant."
  [a]
  (fn [[t]] [(e/* a (e/cos t)) (e/* a (e/sin t))]))

(defn iterates-figure
  "Figure: the fixed-point steps E_k and E_(k+1) of E <- M + e sin E from
   E_0 = M, as one 'point' [E_k E_(k+1)] of state [M]; params [e]. The
   :solve scene reads the steps the kernel computes, two at a time."
  [k]
  (fn [ecc] (fn [[m]] [(kepler-iterate ecc m k) (kepler-iterate ecc m (inc k))])))

(def figures
  "Every moving figure, by name: {:f :params :state}."
  {:planet {:f planet :params [(:e mars) (:a mars)] :state [0]}
   :circle {:f circle-figure :params [(:a mars)] :state [0]}
   :steps-0 {:f (iterates-figure 0) :params [(:e mars)] :state [0]}
   :steps-2 {:f (iterates-figure 2) :params [(:e mars)] :state [0]}
   :steps-4 {:f (iterates-figure 4) :params [(:e mars)] :state [0]}})

(defn equal-time-areas
  "The areas swept by Mars in n equal times (equal steps of M over one
   revolution): E(M) by raster's Brent, then each area twice, from the
   exact formula (ab/2)(E - e sin E) and by raster's quadrature of the
   Sun-planet line's sweep rate dA/dE (independent of the formula)."
  [n]
  (let [{ecc :e a :a} mars
        Ms (map #(* 2 Math/PI (/ % n)) (range (inc n)))
        Es (mapv #(eccentric-anomaly ecc %) Ms)]
    {:formula (map (fn [E0 E1] (double (e/- (swept-area a ecc E1) (swept-area a ecc E0)))) Es (rest Es))
     :quadrature (map (fn [E0 E1] (:value (raster/integral (fn [E] (sweep-rate a ecc E)) E0 E1))) Es (rest Es))
     :E Es}))

;; ---------------------------------------------------------------------------
;; The third law (Principia III, Phaenomenon IV)

(def planets [:mercury :venus :earth :mars :jupiter :saturn])

(def periods
  "Sidereal periods in days, Newton's table."
  {:saturn 10759.275 :jupiter 4332.514 :mars 686.9785 :earth 365.2565
   :venus 224.6176 :mercury 87.9692})

(def kepler-distances
  "Mean distances according to Kepler, Earth = 100000 (Newton's table)."
  {:saturn 951000 :jupiter 519650 :mars 152350 :earth 100000 :venus 72400
   :mercury 38806})

(defn- third-law-numbers
  "Emmy function of T and a: [T^2 / a^3, log10 a^3, log10 T^2], the ratio
   and the point of the log-log plot."
  [T a]
  (let [ln10 (e/log 10)]
    [(e// (e/square T) (e/cube a))
     (e// (e/* 3 (e/log a)) ln10)
     (e// (e/* 2 (e/log T)) ln10)]))

(defn third-law
  "Per planet: T in years, a in Earth distances (Newton's table), and from
   a raster kernel T^2 / a^3 (:k) and the log-log point (:log-a3 :log-T2)."
  []
  (mapv (fn [p]
          (let [T (/ (periods p) (periods :earth))
                a (/ (kepler-distances p) 100000.0)
                [k la3 lt2] (raster/value third-law-numbers (double T) (double a))]
            {:planet p :T T :a a :k k :log-a3 la3 :log-T2 lt2}))
        planets))

(defn spread
  "Least and greatest T^2/a^3 over the six planets."
  []
  (let [ks (map :k (third-law))] {:min (apply min ks) :max (apply max ks)}))

;; ---------------------------------------------------------------------------
;; Graded

(defn- variant [res] (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails))

(defn graded
  "[{:label :grade}]: the identities (proved); Kepler's equation solved for
   Mars and the equal areas in equal times (numeric, :engine :raster); T^2/a^3
   = 1 within 1% on Kepler's distances (numeric, :engine :raster: Newton's
   table through the third-law-numbers kernel, no solver)."
  []
  (let [{ecc :e} mars
        M 1.0
        E (eccentric-anomaly ecc M)
        {:keys [formula quadrature]} (equal-time-areas 12)
        twelfth (/ (* Math/PI (:a mars) (double (minor (:a mars) ecc))) 12)]
    (-> (mapv (fn [[label diff]] {:label label :grade (variant (grade/grade :symbolic [diff]))})
              identities)
        (conj {:label "Kepler's equation for Mars at M = 1, solved by raster (Brent): E - e sin E - M = 0"
               :grade (variant (grade/grade :numeric [(double (e/- (mean-anomaly ecc E) M))] 1e-12))
               :engine :raster}
              {:label (str "the kernel's " kernel-steps " fixed-point steps agree with raster's root to 1e-6")
               :grade (variant (grade/grade :numeric [(- (double (kepler-iterate ecc M kernel-steps)) E)] 1e-6))
               :engine :raster}
              {:label "12 equal times sweep 12 equal areas, each a twelfth of pi a b"
               :grade (variant (grade/grade :numeric (map #(- % twelfth) formula) 1e-9))
               :engine :raster}
              {:label "the same areas by raster's quadrature of the sweep rate, independent of the formula"
               :grade (variant (grade/grade :numeric (map #(- % twelfth) quadrature) 1e-9))
               :engine :raster}
              {:label "third law: T^2 / a^3 = 1 for all six planets, within 1%"
               :grade (variant (grade/grade :numeric (map #(- (:k %) 1) (third-law)) 0.01))
               :engine :raster}))))

;; ---------------------------------------------------------------------------
;; The propositions as data

(def proofs-resource "alexandria/kepler/astronomia_nova.proofs.edn")

(defn proof [] (proofs/read-proofs proofs-resource))
