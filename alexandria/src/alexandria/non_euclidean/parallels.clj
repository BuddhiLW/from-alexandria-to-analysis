(ns alexandria.non-euclidean.parallels
  "Lobachevsky's angle of parallelism (Geometrical researches on the theory
   of parallels, 1840, tr. Halsted 1891, sections 16 and 36).

   From a point A at distance p = AD from a line, the lines through A fall
   into those that cut the line and those that do not; the boundary line is
   the parallel, and the angle HAD it makes with the perpendicular is the
   angle of parallelism Pi(p). Section 36 finds

     tan(Pi(x)/2) = e^-x

   (the unit of length chosen so that the base is Napier's e). Pi falls from
   a right angle at x = 0 (Euclid: one parallel, at 90 degrees) to 0 as x
   grows.

   The tie: three quantities, x, Pi (degrees) and t = tan(Pi/2) = e^-x, each
   a control; moving one solves the other two:

     from x:  t = e^-x,          Pi = 2 atan t
     from Pi: t = tan(Pi/2),     x = -log t
     from t:  x = -log t,        Pi = 2 atan t

   Branch-free clamps keep x in [0 x-max], Pi in (0 90) and t in (0 1].
   Emmy grades forward-then-inverse = identity as :grade/proved."
  (:require [alexandria.grade :as grade]
            [emmy.env :as e]
            [hive-dsl.result :as r]
            [alexandria.structure.arith :as a]
            [alexandria.structure.curvature :as kc]
            [alexandria.structure.tie :as st]))

(def x-max 6)

(def tied [:x :Pi :t])

(def ranges
  {:x [0 x-max 0.01]
   :Pi [(* 2 (Math/toDegrees (Math/atan (Math/exp (- x-max))))) 90 0.01]
   :t [(Math/exp (- x-max)) 1 0.0001]})

(def labels {:x "distance x" :Pi "Π(x) in degrees" :t "tan(Π/2) = e^−x"})

(def rad a/rad)
(def deg a/deg)

(defn Pi-of "Pi(x) in radians: 2 atan e^-x (K = -1)." [x] (kc/angle-of-parallelism kc/lobachevsky x))

(defn x-of-t [t] (e/- 0 (e/log t)))
(defn t-of-Pi [P] (e/tan (e/divide P 2)))

(def parallelism
  "The tie of x, Pi (degrees) and t = tan(Pi/2), all functions of the
   pivot t = e^-x (alexandria.structure.tie :pivot)."
  (st/tie
   {:tie/kind :pivot
    :keys tied
    :ranges ranges
    :to-pivot {:x #(e/exp (e/- 0 %)) :Pi #(t-of-Pi (rad %)) :t identity}
    :state (fn [t] [(x-of-t t) (deg (e/* 2 (e/atan t))) t])}))

(def tie
  "The tie kernel: solved [x Pi t] for each source, in the order of `tied`."
  (st/kernel parallelism))

(def readout-shape
  "From x: Pi(deg), t, the departure from Euclid 90 - Pi, the first-order
   Euclidean approximation 90 - (180/pi) x, and d-on-disk = tanh(x/2), the
   Poincare radius at which a point lies at distance x from the centre."
  {:outputs [:x :Pi :t :defect :linear :r]
   :f (fn [x] (let [P (deg (Pi-of x))]
                {:x x :Pi P :t (e/exp (e/- 0 x)) :defect (e/- 90 P)
                 :linear (e/- 90 (deg x)) :r (e/tanh (e/divide x 2))}))})

(def readout "The readout kernel of x." (st/readout-kernel readout-shape))

(def readout-index (st/index readout-shape))

;; ---------------------------------------------------------------------------
;; Grades

(defn- variant [res] (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails))

(defn graded
  "Forward then inverse = identity: proved by Emmy where its simplifier
   reaches 0, numeric (raster-free JVM doubles) for the tie's round trip."
  []
  [{:label "from t: x = −log t, then e^{−x} = t"
    :grade (variant (grade/grade :symbolic [(e/- (e/exp (e/- 0 (x-of-t 't))) 't)]))}
   {:label "from x: t = e^{−x}, then −log t = x"
    :grade (variant (grade/grade :symbolic [(e/- (x-of-t (e/exp (e/- 0 'x))) 'x)]))}
   {:label "tan(Π/2) with Π = 2 atan t gives t back (40 values of t)"
    :grade (variant (grade/grade :numeric
                                 (for [i (range 1 41) :let [t (/ i 40.0)]]
                                   (- (Math/tan (/ (* 2 (Math/atan t)) 2)) t))
                                 1e-12))}
   {:label "the tie from each source lands on one state: x → Π → t → x"
    :grade (variant (grade/grade :numeric
                                 (for [x [0.0 0.3 1.0 2.5 5.0]
                                       :let [[_ P _] ((tie x 45 0.5) nil)
                                             [x2 _ t2] (subvec ((tie 0 P 0.5) nil) 3 6)
                                             [x3] (subvec ((tie 0 45 t2) nil) 6 9)]
                                       d [(- x2 x) (- x3 x)]]
                                   d)
                                 1e-9))}
   {:label "Euclid's limit: Π(0) = π/2, one parallel at a right angle"
    :grade (variant (grade/grade :numeric [(- (Pi-of 0) (/ Math/PI 2))] 1e-15))}])

(def tex
  {:pi "\\tan\\frac{\\Pi(x)}{2} = e^{-x}"
   :tie "t = e^{-x},\\quad \\Pi = 2\\arctan t,\\quad x = -\\log t"})
