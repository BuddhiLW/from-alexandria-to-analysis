(ns alexandria.galileo.shot
  "Galileo's projectile (Two New Sciences, Fourth Day) as tied quantities:
   the derivation behind the Galileo widget.

   A shot leaves the ground at speed v and elevation theta, under the
   uniform acceleration g of the Third Day. Sideways it moves uniformly,
   downward it falls from rest (Fourth Day, Prop. I):

     x = v cos(theta) t,   y = v sin(theta) t - g t^2 / 2

   Emmy derives, nothing typed in as a result:

     landing    y(t) = 0 is t (w - g t / 2) = 0, w = v sin(theta); the
                second root, by Emmy's solve-linear on the bracket, is the
                flight time t* = 2 w / g
     range      R = x(t*) = 2 c w / g, c = v cos(theta); with Euclid II.5
                2 c w = v^2 - (c - w)^2, so R is greatest when c = w, at 45
                degrees (Prop. VII), and R = v^2 sin(2 theta) / g
     apex       y'(t) = 0 is linear in t (Emmy's D, then solve-linear):
                t = w / g, H = y(w / g) = w^2 / (2 g)
     the curve  eliminating t: y = x tan(theta) - g x^2 / (2 c^2), a
                parabola (Prop. I); its parameter about the apex is
                2 c^2 / g (Apollonius' latus rectum)
     4 H / R    = tan(theta): at 45 degrees the amplitude is twice the
                altitude (Prop. VII)

   The tie of the widget: v, theta, R, H, every one a control.

     v moves       theta fixed: R and H forward
     theta moves   v fixed: R and H forward
     R moves       theta fixed: v = sqrt(g R / sin 2theta)
     H moves       theta fixed: v = sqrt(2 g H) / sin(theta)

   forward then inverse = identity, :grade/proved.

   The falling body (Third Day, Prop. II, Cor. I): distances from rest in
   equal times tau are s_n = g (n tau)^2 / 2, and the n-th time adds
   s_n - s_(n-1) = (2 n - 1) s_1: the odd numbers 1 : 3 : 5 : 7. The tie of
   the fall widget: the drop height D and the fall time T = sqrt(2 D / g)
   (g fixed), proved both ways.

   Reuse: alexandria.apollonius.projection/solve-linear-in (Emmy's D and
   solve-linear), alexandria.galileo.motion (the speed and distance laws)."
  (:require [alexandria.apollonius.projection :as pj]
            [alexandria.galileo.motion :as motion]
            [alexandria.grade :as grade]
            [emmy.env :as e]
            [hive-dsl.result :as r]))

(def g
  "Acceleration of a falling body, m/s^2 (the widget's unit)."
  9.81)

;; ---------------------------------------------------------------------------
;; The shot

(defn position
  "[x y] at time t of the shot (v, theta)."
  [v theta g t]
  [(e/* v (e/cos theta) t)
   (e/- (e/* v (e/sin theta) t) (e/* 1/2 g (e/square t)))])

(defn flight-time
  "The second root of y(t) = t (w - g t / 2): Emmy's solve-linear on the
   bracket."
  [v theta g]
  (:root (pj/solve-linear-in (fn [t] (e/- (e/* v (e/sin theta)) (e/* 1/2 g t))))))

(defn apex-time
  "y'(t) = 0, linear in t: Emmy's D, then solve-linear."
  [v theta g]
  (:root (pj/solve-linear-in (fn [t] ((e/D (fn [s] (second (position v theta g s)))) t)))))

(defn range-of
  "R = 2 v^2 sin(theta) cos(theta) / g (= v^2 sin 2theta / g; written with
   sin and cos of theta so Emmy's simplifier meets the derivation's terms)."
  [v theta g]
  (e// (e/* 2 (e/square v) (e/sin theta) (e/cos theta)) g))

(defn height-of
  "H = v^2 sin^2(theta) / (2 g)."
  [v theta g]
  (e// (e/square (e/* v (e/sin theta))) (e/* 2 g)))

(defn speed-of-range
  "v from R at elevation theta: sqrt(g R / (2 sin theta cos theta))."
  [R theta g]
  (e/sqrt (e// (e/* g R) (e/* 2 (e/sin theta) (e/cos theta)))))

(defn speed-of-height
  "v from H at elevation theta: sqrt(2 g H) / sin(theta)."
  [H theta g]
  (e// (e/sqrt (e/* 2 g H)) (e/sin theta)))

(defn trajectory
  "The curve y(x) of the shot: x tan(theta) - g x^2 / (2 v^2 cos^2 theta)."
  [v theta g]
  (fn [x] (e/- (e/* x (e/tan theta)) (e// (e/* g (e/square x)) (e/* 2 (e/square (e/* v (e/cos theta))))))))

;; ---------------------------------------------------------------------------
;; The fall

(defn fall-distance
  "Distance from rest after time t: g t^2 / 2 (alexandria.galileo.motion)."
  [g t]
  ((motion/distance g) t))

(defn fall-time
  "T with g T^2 / 2 = D: sqrt(2 D / g)."
  [D g]
  (e/sqrt (e// (e/* 2 D) g)))

(defn fall-steps
  "Distances fallen in the n equal times of T/n, in units of the first."
  [n]
  (mapv (fn [k] (e// (e/- (fall-distance 'g (e/* (inc k) 'tau)) (fall-distance 'g (e/* k 'tau)))
                     (fall-distance 'g 'tau)))
        (range n)))

;; ---------------------------------------------------------------------------
;; The derivation, with symbols

(def derivation
  (delay
    (let [[x y] (position 'v 'theta 'g 't)
          t* (flight-time 'v 'theta 'g)
          ta (apex-time 'v 'theta 'g)
          c (e/* 'v (e/cos 'theta)) w (e/* 'v (e/sin 'theta))]
      {:x x :y y
       :landing (e/simplify t*)
       :range (e/simplify (first (position 'v 'theta 'g t*)))
       :apex-t (e/simplify ta)
       :apex (e/simplify (second (position 'v 'theta 'g ta)))
       :curve (e/simplify ((trajectory 'v 'theta 'g) 'x))
       :ii-5 (e/- (e/+ (e/square 'c) (e/square 'w)) (e/square (e/- 'c 'w)))
       :ratio (e/simplify (e// (e/* 4 (height-of 'v 'theta 'g)) (range-of 'v 'theta 'g)))
       :v-of-R (speed-of-range 'R 'theta 'g)
       :v-of-H (speed-of-height 'H 'theta 'g)
       :fall-T (fall-time 'D 'g)})))

(defn- variant [res] (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails))

(defn- proved [label diff]
  {:label label :grade (variant (grade/grade :symbolic [diff]))})

(defn grades
  "[{:label :grade}]: the derivation and the ties, each a difference Emmy
   simplifies to exact 0. The inverse maps go through squares (v^2, sin^2):
   they are graded on v^2 and on the forward map, where no sign choice
   enters. Prop. VIII uses cos and sin of 45 ± delta through C = cos delta,
   S = sin delta: (C ∓ S)/√2 and (C ± S)/√2."
  []
  (let [d @derivation
        [x y] (position 'v 'theta 'g 't)
        t* (flight-time 'v 'theta 'g)
        c (e/* 'v (e/cos 'theta)) w (e/* 'v (e/sin 'theta))
        R (range-of 'v 'theta 'g) H (height-of 'v 'theta 'g)
        shot (fn [co si] (e// (e/* 2 (e/square 'v) co si) 'g))]
    [(proved "the shot lands at t* = 2 v sin θ / g: y(t*) = 0" (second (position 'v 'theta 'g t*)))
     (proved "range x(t*) = 2 c w / g, c = v cos θ, w = v sin θ" (e/- (:range d) (e// (e/* 2 c w) 'g)))
     (proved "= 2 v² sin θ cos θ / g" (e/- (:range d) R))
     (proved "Euclid II.5: 2cw = v² − (c − w)², greatest when c = w (45°)"
             (e/- (e/* 2 c w) (e/- (e/square 'v) (e/square (e/- c w)))))
     (proved "the apex: y′(t) = 0 at t = w / g, height w² / (2g)" (e/- (:apex d) H))
     (proved "the path is the parabola y = x tan θ − g x² / (2 c²) (Fourth Day, Prop. I)"
             (e/- ((trajectory 'v 'theta 'g) x) y))
     (proved "4H / R = tan θ: at 45° the range is four times the height (Prop. VII)"
             (e/- (e/* 4 H (e/cos 'theta)) (e/* R (e/sin 'theta))))
     (proved "shots at 45° ± δ reach the same range (Prop. VIII)"
             (e/- (shot (e/- 'C 'S) (e/+ 'C 'S)) (shot (e/+ 'C 'S) (e/- 'C 'S))))
     (proved "R then v: (v(R))² = v², the inverse of the range" (e/- (e/square (speed-of-range R 'theta 'g)) (e/square 'v)))
     (proved "v then R: R(v(R)) = R" (e/- (range-of (speed-of-range 'R 'theta 'g) 'theta 'g) 'R))
     (proved "H then v: (v(H))² = v², the inverse of the height" (e/- (e/square (speed-of-height H 'theta 'g)) (e/square 'v)))
     (proved "v then H: H(v(H)) = H" (e/- (height-of (speed-of-height 'H 'theta 'g) 'theta 'g) 'H))
     (proved "the fall: g T² / 2 = D at T = √(2D/g)" (e/- (fall-distance 'g (fall-time 'D 'g)) 'D))
     (proved "the fall: T(D(T))² = T²" (e/- (e/square (fall-time (fall-distance 'g 'T) 'g)) (e/square 'T)))
     (proved "equal times add 1 : 3 : 5 : 7 (Third Day, Prop. II, Cor. I)"
             (reduce e/+ (map e/- (fall-steps 4) [1 3 5 7])))
     (proved "the n-th time adds (2n − 1) times the first"
             (e// (e/- (e/- (fall-distance 'g (e/* 'n 'tau)) (fall-distance 'g (e/* (e/- 'n 1) 'tau)))
                       (e/* (e/- (e/* 2 'n) 1) (fall-distance 'g 'tau)))
                  'g))]))

;; ---------------------------------------------------------------------------
;; The derivation on the page

(defn- eq [lhs rhs] (e/literal-number (list '= (e/freeze lhs) (e/freeze rhs))))

(defn equations
  "[{:label :eqs}] for emmy.clerk/->TeX."
  []
  (let [d @derivation]
    [{:label "carried uniformly sideways, falling from rest downward (Fourth Day, Prop. I)"
      :eqs [(eq 'x (:x d)) (eq 'y (:y d))]}
     {:label "y = 0 again: the bracket is linear in t, Emmy's solve-linear gives the flight time, then the range"
      :eqs [(eq (symbol "t_*") (:landing d)) (eq 'R (:range d))]}
     {:label "y′ = 0 is linear in t: the apex"
      :eqs [(eq (symbol "t_H") (:apex-t d)) (eq 'H (:apex d))]}
     {:label "t eliminated: a parabola"
      :eqs [(eq 'y (:curve d))]}
     {:label "Euclid II.5 on c = v cos θ, w = v sin θ: 2cw = v² − (c − w)², so R = (v² − (c − w)²)/g, greatest at c = w"
      :eqs [(eq (list '* 2 'c 'w) (:ii-5 d))]}
     {:label "the ratio of height to range"
      :eqs [(eq (list '/ (list '* 4 'H) 'R) (:ratio d))]}
     {:label "solved back for v (θ held)"
      :eqs [(eq 'v (:v-of-R d)) (eq 'v (:v-of-H d))]}
     {:label "the fall from rest, solved for its time"
      :eqs [(eq 'T (:fall-T d))]}]))
