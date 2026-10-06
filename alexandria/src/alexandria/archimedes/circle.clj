(ns alexandria.archimedes.circle
  "Archimedes, Measurement of a Circle: the figures that move in its proofs,
   the numbers of Proposition 3, and the identities the proofs rest on.

     figures     Emmy functions (fn [& params] (fn [state] [x y])) of one
                 point of a moving figure; any medium compiles or samples them
     tables      Archimedes' own ratios for Proposition 3, as exact rationals
                 (Heath 1897, pp. 93-98)
     identities  the steps of the proofs as Emmy differences, graded by
                 alexandria.grade"
  (:require [alexandria.grade :as grade]
            [emmy.env :as e]
            [emmy.expression :as x]
            [emmy.simplify :as simp]
            [emmy.simplify.rules :as rules]
            [hive-dsl.result :as r]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [alexandria.proofs :as proofs]))

;; ---------------------------------------------------------------------------
;; Branch-free arithmetic: a figure stays one expression

(defn- odd-indicator
  "1 when integer m is odd, 0 when even."
  [m]
  (e// (e/- 1 (e/cos (e/* Math/PI m))) 2))

(defn- step
  "1 when x is positive, 0 when negative."
  [x]
  (e// (e/+ 1 (e// x (e/+ (e/abs x) 1e-12))) 2))

(defn- positive-part [x] (e// (e/+ x (e/abs x)) 2))

(defn- clamp01 [x] (e// (e/+ 1 (e/- (e/abs x) (e/abs (e/- x 1)))) 2))

(defn- polar [rho theta] [(e/* rho (e/cos theta)) (e/* rho (e/sin theta))])

;; ---------------------------------------------------------------------------
;; Figures

(defn inscribed-bisection
  "Vertex m (0 <= m < 2n) of the inscribed 2n-gon of radius r while the arcs
   of the n-gon are bisected: even m are the n-gon's vertices, odd m rise
   from the midpoint of a side (t = 0) to the middle of its arc (t = 1)."
  [n t r]
  (fn [[m]]
    (let [odd (odd-indicator m)
          rho (e/* r (e/+ (e/- 1 odd)
                          (e/* odd (e/+ (e/* (e/- 1 t) (e/cos (e// Math/PI n))) t))))]
      (polar rho (e// (e/* Math/PI m) n)))))

(defn circumscribed-bisection
  "Point m (0 <= m < 4n) of the polygon circumscribed about the circle of
   radius r while the n-gon becomes the 2n-gon: m = 0 mod 4 are the n-gon's
   points of contact, odd m the 2n-gon's vertices (on the n-gon's sides),
   m = 2 mod 4 the n-gon's corners, cut back by the new tangents to the new
   points of contact at t = 1."
  [n t r]
  (fn [[m]]
    (let [odd (odd-indicator m)
          corner (e/* (e/- 1 odd) (odd-indicator (e// m 2)))
          contact (e/- 1 odd corner)
          rho (e/* r (e/+ contact
                          (e// odd (e/cos (e// Math/PI (e/* 2 n))))
                          (e/* corner (e/+ (e// (e/- 1 t) (e/cos (e// Math/PI n))) t))))]
      (polar rho (e// (e/* Math/PI m) (e/* 2 n))))))

(defn unroll
  "Point s (0 <= s <= 1, a fraction of the circumference) of the circle of
   radius r resting on the ground at the origin, after rolling a fraction t
   of a turn to the right: the arc rolled over lies flat on the ground."
  [t r]
  (fn [[s]]
    (let [u (positive-part (e/- s t))
          flat (e/- s u)]
      [(e/+ (e/* 2 Math/PI r flat) (e/* r (e/sin (e/* 2 Math/PI u))))
       (e/* r (e/- 1 (e/cos (e/* 2 Math/PI u))))])))

(defn sector-row
  "Vertex j (0 the apex at the centre, 1 and 2 the ends of the side) of
   triangle k of the regular n-gon about the circle of radius r, inscribed
   when out = 0, circumscribed when out = 1. t1 = 0: in place, centre at the
   origin. t1 = 1: standing on the ground y = y0, the n triangles in a row
   from x0. t2 slides every apex to the one above x0, bases fixed: one
   triangle, base the perimeter, height the apothem."
  [n t1 t2 r out x0 y0]
  (fn [[k j]]
    (let [half (e// Math/PI n)
          big-r (e/+ (e/* (e/- 1 out) r) (e// (e/* out r) (e/cos half)))
          apothem (e/* big-r (e/cos half))
          side (e/* 2 big-r (e/sin half))
          phi (e// (e/* 2 Math/PI (e/+ k 1/2)) n)
          apex? (e// (e/* (e/- j 1) (e/- j 2)) 2)
          delta (e/* half (e/- j 1) (e/- 1 apex?))
          rho (e/* big-r (e/- 1 apex?))
          turn (e/+ (e/- (e/* -1/2 Math/PI) phi)
                    (e/* 2 Math/PI (step (e/- phi (e// Math/PI 2)))))
          theta (e/+ phi delta (e/* t1 turn))
          [vx vy] (polar rho theta)
          x (e/+ (e/* t1 (e/+ x0 (e/* (e/+ k 1/2) side))) vx)
          y (e/+ (e/* t1 (e/+ y0 apothem)) vy)
          lean (e/* -1 t2 (e/+ k 1/2) side (e// (e/- y y0) apothem))]
      [(e/+ x lean) y])))

(defn- smoothstep [x] (let [x (clamp01 x)] (e/* x x (e/- 3 (e/* 2 x)))))

(defn ring-open
  "A point of ring k (0 the outermost) of the disk of radius r cut into n
   rings of width e = r/n, while the rings are cut along the radius at the
   top and straightened. State [k a b]: a in [-1/2 1/2] runs along the ring
   from one side of the cut to the other, b in [0 1] from its inner edge to
   its outer edge.

   Ring 0 opens with s0, every other ring with s, one after another (lag).
   Opening bends every concentric line about a centre that recedes (to 20 r,
   keeping its length), then settles onto the straight line (the last 15%):
   at u = 1 the ring is an isosceles trapezoid on the line y = -rho, and the
   n trapezoids stack into the triangle with base the circumference on
   y = -r and apex at the centre. lean slides that apex sideways by half the
   base (Euclid I.38), onto the triangle K. The centre stops at 20 r because
   the browser's sine is good to about 5e-6, an error the radius multiplies."
  [n s0 s r lean]
  (fn [[k a b]]
    (let [e-width (e// r n)
          rho (e/+ (e/- r (e/* (e/+ k 1) e-width)) (e/* b e-width))
          first? (e/- 1 (step (e/- k 1/2)))
          lag 0.3
          u-rest (smoothstep (e/- (e/* s (e/+ 1 (e/* lag (e/- n 1)))) (e/* lag (e/- k 1))))
          u (e/+ (e/* first? (smoothstep s0)) (e/* (e/- 1 first?) u-rest))
          kappa (e/- 1 (e/* 0.95 u))
          d (e/* r (e/- (e// 1 kappa) 1))
          rho' (e/+ rho d)
          arc (e/* a 2 Math/PI rho)
          theta (e// arc (e/+ rho' 1e-12))
          bent-x (e/* rho' (e/sin theta))
          bent-y (e/- d (e/* rho' (e/cos theta)))
          w (smoothstep (e// (e/- u 0.85) 0.15))
          x (e/+ bent-x (e/* w (e/- arc bent-x)))
          y (e/+ bent-y (e/* w (e/- (e/- rho) bent-y)))]
      [(e/- x (e/* lean Math/PI r (e// (e/+ y r) r))) y])))

(defn polygon-measures
  "Figure: [side apothem] of the regular n-gon about the circle of radius r,
   inscribed when out = 0 (2 r sin(pi/n), r cos(pi/n)), circumscribed when
   out = 1 (2 r tan(pi/n), r). State unused: [0]."
  [n r out]
  (fn [[_]]
    (let [half (e// Math/PI n)
          big-r (e/+ (e/* (e/- 1 out) r) (e// (e/* out r) (e/cos half)))]
      [(e/* 2 big-r (e/sin half)) (e/* big-r (e/cos half))])))

(def figures
  "Every figure, by name: {:f figure :params initial-params :state
   initial-state}, and :opts for the compiler when the default does not suit."
  {:inscribed {:f inscribed-bisection :params [6 0 1] :state [0]}
   :circumscribed {:f circumscribed-bisection :params [6 0 1] :state [0]}
   :unroll {:f unroll :params [0 1] :state [0]}
   :sectors {:f sector-row :params [6 0 0 1 0 0 0] :state [0 0]}
   :polygon {:f polygon-measures :params [6 1 0] :state [0]}
   :rings {:f ring-open :params [4 0 0 1 0] :state [0 0 0] :opts {:simplify? false}}})

;; ---------------------------------------------------------------------------
;; Proposition 3 in Archimedes' numbers

(def upper-table
  "The circumscribed polygons. In the right triangle O A C (A the point of
   contact, C a corner), OA : AC > a : 153 and OC : CA > c : 153. Bisecting
   the angle at O gives a' = a + c (Euclid VI.3); c' is taken below
   sqrt(a'^2 + 153^2)."
  [{:sides 6 :a 265 :c 306 :b 153}
   {:sides 12 :a 571 :c 4729/8 :b 153}
   {:sides 24 :a 9297/8 :c 9377/8 :b 153}
   {:sides 48 :a 9337/4 :c 9357/4 :b 153}
   {:sides 96 :a 9347/2 :b 153}])

(def lower-table
  "The inscribed polygons. In the right triangle A C B in the semicircle (AB
   the diameter, BC a side), AC : CB < a : b and AB : BC < h : b. Bisecting
   the angle at A gives a' = a + h; h' is taken above sqrt(a'^2 + b^2); the
   pair is rescaled by a common factor when the numbers grow (4/13, 11/40)."
  [{:sides 6 :a 1351 :h 1560 :b 780}
   {:sides 12 :a 2911 :h 12055/4 :b 780}
   {:sides 24 :a 1823 :h 20227/11 :b 240}
   {:sides 48 :a 1007 :h 6055/6 :b 66}
   {:sides 96 :a 12097/6 :h 8069/4 :b 66}])

(def lower-bound "3 10/71" 223/71)
(def upper-bound "3 1/7" 22/7)
(def prop-2-ratio "circle : square on the diameter" 11/14)

(defn upper-ratio
  "Perimeter : diameter of the circumscribed polygon, from a table row."
  [{:keys [sides a b]}]
  (/ (* sides b) a))

(defn lower-ratio
  "Perimeter : diameter of the inscribed polygon, from a table row."
  [{:keys [sides h b]}]
  (/ (* sides b) h))

(defn table-checks
  "Every inequality of Proposition 3, decided exactly in rationals:
   [{:label :holds?}]."
  []
  (let [sq #(* % %)
        up-steps (map vector upper-table (rest upper-table))
        lo-steps (map vector lower-table (rest lower-table))
        scale (fn [{:keys [b]} {b' :b}] (/ b' b))]
    (concat
     [{:label "265 : 153 < sqrt 3, so OA : AC > 265 : 153" :holds? (< (sq 265) (* 3 (sq 153)))}
      {:label "1351 : 780 > sqrt 3, so AC : CB < 1351 : 780" :holds? (> (sq 1351) (* 3 (sq 780)))}]
     (for [[{:keys [a c b]} {a' :a c' :c sides :sides}] up-steps]
       {:label (str sides "-gon about: a' = a + c, and c' below the hypotenuse")
        :holds? (and (= a' (+ a c)) (or (nil? c') (<= (sq c') (+ (sq a') (sq b)))))})
     (for [[{:keys [a h] :as row} {a' :a h' :h b' :b sides :sides :as row'}] lo-steps]
       {:label (str sides "-gon in: a' = (a + h) rescaled, and h' above the hypotenuse")
        :holds? (and (= a' (* (scale row row') (+ a h)))
                     (>= (sq h') (+ (sq a') (sq b'))))})
     [{:label "96-gon about: perimeter : diameter < 3 1/7"
       :holds? (< (upper-ratio (last upper-table)) upper-bound)}
      {:label "96-gon in: perimeter : diameter > 3 10/71"
       :holds? (> (lower-ratio (last lower-table)) lower-bound)}
      {:label "11 : 14 = (22/7) : 4" :holds? (= prop-2-ratio (/ upper-bound 4))}])))

;; ---------------------------------------------------------------------------
;; Identities, graded

(defn- expand-angles
  "expr with multiple-angle sines and cosines expanded, then simplified."
  [expr]
  (-> expr x/expression-of rules/expand-multiangle
      simp/simplify-expression simp/simplify-expression))

(def identities
  "The equalities the proofs use, as [label difference] pairs, each zero."
  (let [n 'n rr 'r phi 'phi]
    [["polygon cut at the centre: n triangles (1/2) r^2 sin(2 pi/n) = (1/2) perimeter x apothem"
      (expand-angles (e/- (e/* n 1/2 rr rr (e/sin (e/* 2 (e// 'pi n))))
                          (e/* 1/2 (e/* 2 n rr (e/sin (e// 'pi n))) (e/* rr (e/cos (e// 'pi n))))))]
     ["Euclid VI.3 at the bisection, angle 2 phi halved: sin(2 phi) cot(phi) = cos(2 phi) + 1, that is cot(phi) = cot(2 phi) + csc(2 phi)"
      (expand-angles (e/- (e/* (e/sin (e/* 2 phi)) (e// (e/cos phi) (e/sin phi)))
                          (e/+ (e/cos (e/* 2 phi)) 1)))]
     ["Proposition 2 from Proposition 3: (1/2) r (22/7)(2r) : (2r)^2 = 11 : 14"
      (e/- (e// (e/* 1/2 rr (e/* 22/7 2 rr)) (e/square (e/* 2 rr))) 11/14)]]))

(def ring-identities
  "The rings argument (an illustration, not Archimedes' text), as [label
   difference] pairs, each zero: e the width of a ring, r its outer radius."
  (let [r 'r w 'e pi 'pi]
    [["a straightened ring is a trapezoid of the ring's area: e (2 pi r + 2 pi (r - e)) / 2 = pi (r^2 - (r - e)^2)"
      (e/- (e// (e/* w (e/+ (e/* 2 pi r) (e/* 2 pi (e/- r w)))) 2)
           (e/* pi (e/- (e/square r) (e/square (e/- r w)))))]
     ["its slanted side is e sqrt(1 + pi^2), longer than the cut e"
      (e/- (e/square (e/sqrt (e/+ (e/square w) (e/square (e/* pi w)))))
           (e/* (e/square w) (e/+ 1 (e/square pi))))]
     ["outer minus inner rectangles over n rings: n (2 pi e) e = C e, C = 2 pi r, e = r / n"
      (let [n 'n] (e/- (e/* n (e/* 2 pi (e// r n)) (e// r n)) (e/* 2 pi r (e// r n))))]]))

(defn graded-ring-identities
  "[{:label :grade}] of ring-identities."
  []
  (mapv (fn [[label diff]]
          {:label label
           :grade (let [res (grade/grade :symbolic [diff])]
                    (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails))})
        ring-identities))

(defn graded-identities
  "[{:label :grade}] where :grade is the variant keyword of
   alexandria.grade/grade on the identity's difference."
  []
  (mapv (fn [[label diff]]
          {:label label
           :grade (let [res (grade/grade :symbolic [diff])]
                    (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails))})
        identities))

;; ---------------------------------------------------------------------------
;; The proofs as data

(def proofs-resource "alexandria/archimedes/measurement_of_a_circle.proofs.edn")

(defn proofs
  "Result of {proposition-id {:steps [{:claim :why :stage}] ...}} read from
   proofs-resource."
  []
  (proofs/read-proofs proofs-resource))
