(ns alexandria.ptolemy.theorem
  "Ptolemy's theorem (Almagest I.10, Halma 1813 pp. 29-30): in a
   quadrilateral ABCD inscribed in a circle, the rectangle on the diagonals
   equals the two rectangles on the opposite sides,

     AC.BD = AB.CD + AD.BC,

   and, for four points in general (not proved by Ptolemy; the inequality
   bears his name), AC.BD <= AB.CD + AD.BC with equality exactly when ABCD
   is inscribed in that order.

   Two Emmy setups, both lowered to raster kernels for the page:

   - the TIE on the unit circle: A, B, C fixed at 200, 80 and 20 degrees,
     D at 20 - d degrees, d the arc CD. The chords CD = 2 sin(d/2),
     BD = 2 sin((60 + d)/2) and AD = 2 cos(d/2) are each monotone in d on
     [1, 119], so each is a control whose inverse gives d back
     (atan2 forms, never acos at 1);
   - the PLAYGROUND: four free points (8 numbers), the six distances, the
     two sides of Ptolemy's relation, the circle through A, B, C and how far
     D is from it, and the cross products that say whether the diagonals
     cross (the order ABCD is convex)."
  (:require [alexandria.grade :as grade]
            [alexandria.ptolemy.chords :as chords]
            [emmy.env :as e]
            [hive-dsl.result :as r]))

;; ---------------------------------------------------------------------------
;; Branch-free clamps (as alexandria.medium.tie, which the core cannot require)

(defn at-least [x lo] (e/* 0.5 (e/+ x lo (e/abs (e/- x lo)))))
(defn at-most [x hi] (e/* 0.5 (e/- (e/+ x hi) (e/abs (e/- x hi)))))
(defn clamp [x lo hi] (at-most (at-least x lo) hi))
(defn step-up "1 where x > 0, 0 where x < 0." [x] (e/* 0.5 (e/+ 1 (e// x (e/abs x)))))

;; ---------------------------------------------------------------------------
;; The tie on the unit circle

(def angles "A, B, C in degrees on the unit circle." {:A 200 :B 80 :C 20})

(def tied [:d :CD :BD :AD])

(def ranges
  "[min max step]; the chord ranges are the images of d in [1, 119]."
  {:d [1 119 0.5]
   :CD [(* 2 (Math/sin (Math/toRadians 0.5))) (* 2 (Math/sin (Math/toRadians 59.5))) 0.001]
   :BD [(* 2 (Math/sin (Math/toRadians 30.5))) (* 2 (Math/sin (Math/toRadians 89.5))) 0.001]
   :AD [(* 2 (Math/cos (Math/toRadians 59.5))) (* 2 (Math/cos (Math/toRadians 0.5))) 0.001]})

(def labels {:d "arc CD (°)" :CD "CD" :BD "BD" :AD "AD"})

(defn- lo [k] (first (ranges k)))
(defn- hi [k] (second (ranges k)))

(defn- half-rad [deg] (e/* deg (/ Math/PI 360)))

(defn- asin* [x] (e/atan x (e/sqrt (e/- 1 (e/square x)))))
(defn- acos* [x] (e/atan (e/sqrt (e/- 1 (e/square x))) x))

(defn- deg [r] (e/* r (/ 180 Math/PI)))

(defn chords-of
  "[d CD BD AD] from d (degrees)."
  [d]
  [d (e/* 2 (e/sin (half-rad d))) (e/* 2 (e/sin (half-rad (e/+ 60 d)))) (e/* 2 (e/cos (half-rad d)))])

(defn- from-d [d _ _ _] (chords-of (clamp d (lo :d) (hi :d))))
(defn- from-CD [_ CD _ _] (chords-of (e/* 2 (deg (asin* (e// (clamp CD (lo :CD) (hi :CD)) 2))))))
(defn- from-BD [_ _ BD _] (chords-of (e/- (e/* 2 (deg (asin* (e// (clamp BD (lo :BD) (hi :BD)) 2)))) 60)))
(defn- from-AD [_ _ _ AD] (chords-of (e/* 2 (deg (acos* (e// (clamp AD (lo :AD) (hi :AD)) 2))))))

(defn tie
  "The tie kernel: the solved [d CD BD AD] for each source, in the order of `tied`."
  [d CD BD AD]
  (fn [_] (vec (mapcat #(% d CD BD AD) [from-d from-CD from-BD from-AD]))))

(defn- unit [deg'] (let [t (e/* deg' (/ Math/PI 180))] [(e/cos t) (e/sin t)]))

(defn- len [p q] (e/sqrt (chords/dist2 p q)))

(defn tie-readout
  "From d: [AB BC CD DA AC BD AC.BD AB.CD AD.BC sum residual Dx Dy]."
  [d]
  (fn [_]
    (let [A (unit (:A angles)) B (unit (:B angles)) C (unit (:C angles)) D (unit (e/- (:C angles) d))
          AB (len A B) BC (len B C) CD (len C D) DA (len D A) AC (len A C) BD (len B D)
          lhs (e/* AC BD) p1 (e/* AB CD) p2 (e/* DA BC)]
      [AB BC CD DA AC BD lhs p1 p2 (e/+ p1 p2) (e/- lhs (e/+ p1 p2)) (first D) (second D)])))

(def tie-index {:AB 0 :BC 1 :CD 2 :DA 3 :AC 4 :BD 5 :lhs 6 :p1 7 :p2 8 :rhs 9 :residual 10 :Dx 11 :Dy 12})

;; ---------------------------------------------------------------------------
;; The playground: four free points

(defn- cross [[ax ay] [bx by] [cx cy]]
  (e/- (e/* (e/- bx ax) (e/- cy ay)) (e/* (e/- by ay) (e/- cx ax))))

(defn circumcircle
  "[ox oy r 2D]: centre and radius of the circle through p, q, s and the
   doubled signed area D; the denominator is kept off 0 so nearly collinear
   points give a huge circle, not NaN."
  [[ax ay] [bx by] [cx cy]]
  (let [dd (e/* 2 (e/+ (e/* ax (e/- by cy)) (e/* bx (e/- cy ay)) (e/* cx (e/- ay by))))
        dd' (e/+ dd (e/* 1e-12 (e/- (e/* 2 (step-up dd)) 1)))
        a2 (e/+ (e/square ax) (e/square ay)) b2 (e/+ (e/square bx) (e/square by)) c2 (e/+ (e/square cx) (e/square cy))
        ux (e// (e/+ (e/* a2 (e/- by cy)) (e/* b2 (e/- cy ay)) (e/* c2 (e/- ay by))) dd')
        uy (e// (e/+ (e/* a2 (e/- cx bx)) (e/* b2 (e/- ax cx)) (e/* c2 (e/- bx ax))) dd')]
    [ux uy (e/sqrt (chords/dist2 [ux uy] [ax ay])) dd]))

(defn quad
  "For A B C D (8 numbers): [AB BC CD DA AC BD AC.BD AB.CD+AD.BC residual
   ox oy r |OD|-r cross(AC,B) cross(AC,D) cross(BD,A) cross(BD,C) 2*area(ABC)
   min-side]."
  [ax ay bx by cx cy dx dy]
  (fn [_]
    (let [A [ax ay] B [bx by] C [cx cy] D [dx dy]
          AB (len A B) BC (len B C) CD (len C D) DA (len D A) AC (len A C) BD (len B D)
          lhs (e/* AC BD) rhs (e/+ (e/* AB CD) (e/* DA BC))
          [ox oy r dd] (circumcircle A B C)]
      [AB BC CD DA AC BD lhs rhs (e/- lhs rhs)
       ox oy r (e/- (len [ox oy] D) r)
       (cross A C B) (cross A C D) (cross B D A) (cross B D C) dd
       (at-most (at-most AB BC) (at-most (at-most CD DA) (at-most AC BD)))])))

(def quad-index {:AB 0 :BC 1 :CD 2 :DA 3 :AC 4 :BD 5 :lhs 6 :rhs 7 :residual 8 :ox 9 :oy 10 :r 11
                 :off 12 :acb 13 :acd 14 :bda 15 :bdc 16 :area2 17 :min-side 18})

(defn case-of
  "The case the playground names, from quad's output (JVM side; the page
   carries the same rule as a browser form)."
  [out]
  (let [g (fn [k] (nth out (quad-index k)))
        convex? (and (neg? (* (g :acb) (g :acd))) (neg? (* (g :bda) (g :bdc))))]
    (cond (< (g :min-side) 1e-3) :degenerate
          (< (Math/abs (double (g :area2))) 1e-6) :collinear
          (< (Math/abs (double (g :off))) 2e-3) (if convex? :cyclic :cyclic-crossed)
          (neg? (g :off)) :inside
          :else :outside)))

;; ---------------------------------------------------------------------------
;; Derivation and grades

(def identities
  "[label difference] pairs reduced to 0 by Emmy. The first writes each
   chord 2 sin(y - x) through the sines and cosines of the four half-angles
   (sin(y - x) = sin y cos x - cos y sin x), a polynomial identity."
  [["Ptolemy on the circle, points at half-angles a < b < c < d: AC·BD − AB·CD − AD·BC = 0"
    (let [s (fn [x y] (e/* 2 (e/- (e/* (symbol (str "s" y)) (symbol (str "c" x)))
                                 (e/* (symbol (str "c" y)) (symbol (str "s" x))))))]
      (e/- (e/* (s 'a 'c) (s 'b 'd)) (e/+ (e/* (s 'a 'b) (s 'c 'd)) (e/* (s 'a 'd) (s 'b 'c)))))]
   ["the tie: AD = 2 cos(d/2) and CD = 2 sin(d/2) lie on one circle, CD² + AD² = AC² = 4 (AC a diameter)"
    (let [[_ CD _ AD] (chords-of 'd)] (e/- (e/+ (e/square CD) (e/square AD)) 4))]
   ["the rectangle (Euclid I.47 from Ptolemy): AC = BD = diameter, so AC² = AB² + BC²"
    (let [A [-1 0] C [1 0] B [(e/cos 't) (e/sin 't)]]
      (e/- (chords/dist2 A C) (e/+ (chords/dist2 A B) (chords/dist2 B C))))]])

(defn- variant [res] (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails))

(defn graded []
  (mapv (fn [[label diff]] {:label label :grade (variant (grade/grade :symbolic [diff]))}) identities))

(def tex
  "The relation and the tie, as TeX."
  {:theorem "AC\\cdot BD = AB\\cdot CD + AD\\cdot BC"
   :inequality "AC\\cdot BD \\le AB\\cdot CD + AD\\cdot BC"
   :tie "CD = 2\\sin\\tfrac d2,\\quad BD = 2\\sin\\tfrac{60^\\circ + d}{2},\\quad AD = 2\\cos\\tfrac d2"})
