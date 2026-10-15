(ns alexandria.delian.playgrounds
  "Playgrounds for the doubling of the cube: each section's setup free.

     means      Hippocrates' reduction read through Menaechmus' loci. The
                two given lines a and b are free (not only b = 2a). The
                three loci of the means, x^2 = a y, y^2 = b x, x y = a b,
                are drawn; their meeting point Theta has the two means as
                coordinates, x^3 = a^2 b. The reader also drags a point P
                and sees on which loci it lies (the residual of each).
     conchoid   Nicomedes' curve: pole E, ruler, interval l free; a line
                through E turned by the reader cuts the ruler at R and the
                curve at Q, RQ = l. The inner branch is named live: loop
                (l > d), cusp (l = d), no loop (l < d).
     cissoid    Diocles' curve in the circle of diameter 2r: the abscissa u
                is dragged; h, u, y are read off and the continued
                proportion (2r - u) : h = h : u = u : y shown.

   Every figure is an Emmy function compiled by raster for the browser;
   curve kernels take a sample parameter as their state."
  (:require [emmy.env :as e]))

(def tiny 1e-9)

(defn- cbrt "The real cube root of a positive x, branch-free." [x]
  (e/exp (e// (e/log (e/+ x tiny)) 3)))

;; ---------------------------------------------------------------------------
;; Hippocrates' two means, on Menaechmus' loci

(defn means-curves
  "For given lines a, b and a sample t > 0: the points of the three loci at
   t, [x1 y1 x2 y2 x3 y3]: parabola x^2 = a y (x = t), parabola y^2 = b x
   (y = t), hyperbola x y = a b (x = t)."
  [a b]
  (fn [[t]]
    [t (e// (e/square t) a)
     (e// (e/square t) b) t
     t (e// (e/* a b) t)]))

(def means-scalars
  "The numbers of `means-figure`, in order."
  [:x :y :ratio :ax :xy :yb :r1 :r2 :r3 :cube-x :cube-a :px :py])

(defn means-figure
  "For given lines a, b and the dragged point P = (px, py): the means x =
   cbrt(a^2 b), y = x^2/a; b/a; the three ratios a/x, x/y, y/b; P's
   residual on each locus (scaled to lengths: (px^2 - a py)/a, ...); the
   cubes on x and on a; P."
  [a b px py]
  (fn [_]
    (let [x (cbrt (e/* (e/square a) b))
          y (e// (e/square x) a)]
      [x y (e// b a) (e// a x) (e// x y) (e// y b)
       (e// (e/- (e/square px) (e/* a py)) a)
       (e// (e/- (e/square py) (e/* b px)) b)
       (e// (e/- (e/* px py) (e/* a b)) (e/sqrt (e/* a b)))
       (e/cube x) (e/cube a) px py])))

(def means-start "Eutocius' Delian case: a = 1, b = 2a, P off the curves." {:a 1 :b 2 :px 2 :py 1})

(def means-presets
  "The 'try this' states. :eight keeps b inside its slider (b <= 4.6)."
  {:delian means-start
   :eight {:a 0.5 :b 4 :px 2 :py 1}
   :half {:a 2 :b 1 :px 2 :py 1}
   :equal {:a 1.5 :b 1.5 :px 2 :py 1}
   :theta {:a 1 :b 2 :px 1.26 :py 1.587}})

(defn means-kind
  "The case of b : a: :equal (no means needed), :delian (b = 2a), :cube
   (b/a or a/b a whole cube n^3: x = n a, found by ruler alone), :solid
   (any other ratio)."
  [ratio]
  (let [r (double ratio)
        whole-cube? (fn [q] (some #(< (Math/abs (- q (* % % %))) 0.01) (range 2 6)))]
    (cond (< (Math/abs (- r 1)) 0.01) :equal
          (< (Math/abs (- r 2)) 0.01) :delian
          (or (whole-cube? r) (whole-cube? (/ 1 r))) :cube
          :else :solid)))

;; ---------------------------------------------------------------------------
;; Nicomedes' conchoid: the ruler y = 0, the pole E = (0, -d), the interval l

(defn conchoid-curves
  "Both branches at angle phi of the line through E: [Qx Qy Q'x Q'y], Q l
   beyond the ruler, Q' l before it (the inner branch, with its loop)."
  [d l]
  (fn [[phi]]
    (let [c (e/cos phi) s (e/sin phi) rx (e// (e/* d c) (e/+ s tiny))]
      [(e/+ rx (e/* l c)) (e/* l s) (e/- rx (e/* l c)) (e/negate (e/* l s))])))

(def conchoid-scalars [:rx :qx :qy :q'x :q'y :RQ :RQ' :gap :ex :ey])

(defn conchoid-figure
  "For d, l, the reader's angle phi (degrees) and the given line x = c: R
   on the ruler, Q and Q', |RQ| and |RQ'| (both l: Nicomedes' property),
   Qx - c (the neusis residual), and E."
  [d l phi c]
  (fn [_]
    (let [t (e/* phi (/ Math/PI 180))
          co (e/cos t) s (e/sin t)
          rx (e// (e/* d co) (e/+ s tiny))
          qx (e/+ rx (e/* l co)) qy (e/* l s)
          q'x (e/- rx (e/* l co)) q'y (e/negate qy)]
      [rx qx qy q'x q'y
       (e/sqrt (e/+ (e/square (e/- qx rx)) (e/square qy)))
       (e/sqrt (e/+ (e/square (e/- q'x rx)) (e/square q'y)))
       (e/- qx c) 0 (e/negate d)])))

(def conchoid-start {:d 1 :l 2 :phi 60 :c 2.2})

(def conchoid-presets
  "The 'try this' states. :neusis turns the line until Q lands on x = 2.2
   (cot phi + 2 cos phi = 2.2 at phi = 48.68 degrees)."
  {:nicomedes conchoid-start
   :cusp {:d 1 :l 1 :phi 60 :c 2.2}
   :no-loop {:d 1.5 :l 0.8 :phi 60 :c 2.2}
   :neusis {:d 1 :l 2 :phi 48.68 :c 2.2}})

(defn conchoid-kind
  "The inner branch's shape: :loop (l > d), :cusp (l = d), :smooth (l < d)."
  [d l]
  (let [q (- (double l) (double d))]
    (cond (< (Math/abs q) 0.01) :cusp (pos? q) :loop :else :smooth)))

;; ---------------------------------------------------------------------------
;; Diocles' cissoid: the circle of diameter 2r, vertex at the origin

(defn cissoid-curves
  "At abscissa u (0 < u < 2r): the circle's upper point and the cissoid's,
   [u h u y]."
  [r]
  (fn [[u]]
    (let [w (e/- (e/* 2 r) u)
          h (e/sqrt (e/* u (e/+ w tiny)))]
      [u h u (e// (e/* u h) (e/+ w tiny))])))

(def cissoid-scalars [:w :h :u :y :r1 :r2 :r3 :ratio :mean])

(defn cissoid-figure
  "For r and the dragged u: 2r - u, h, u, y; the three ratios (2r - u)/h,
   h/u, u/y; the ratio of the extremes y/(2r - u); u/h, its cube root."
  [r u]
  (fn [_]
    (let [w (e/- (e/* 2 r) u)
          h (e/sqrt (e/* u w))
          y (e// (e/* u h) w)]
      [w h u y (e// w h) (e// h u) (e// u y) (e// y w) (e// u h)])))

(def cissoid-start {:r 1.2 :u 1.0})

(def cissoid-presets
  "The 'try this' states. :delian: y / (2r - u) = (u / (2r - u))^(3/2) = 2
   at u = 2r 2^(2/3) / (1 + 2^(2/3)) = 1.4724 for r = 1.2."
  {:diocles cissoid-start
   :centre {:r 1.2 :u 1.2}
   :delian {:r 1.2 :u 1.4724}
   :edge {:r 1.2 :u 2.3}})

(defn cissoid-kind
  "Where u stands: :centre (u = r: all four equal), :delian (the extremes
   in ratio 2, so u/h is the cube root of 2), :near (u < r), :far."
  [r u ratio]
  (cond (< (Math/abs (- (double u) (double r))) 0.01) :centre
        (< (Math/abs (- (double ratio) 2)) 0.01) :delian
        (< u r) :near
        :else :far))

;; ---------------------------------------------------------------------------
;; Archytas: AD on the x axis from A, the circle ABD in z = 0, z up

(defn- at-least [x lo] (e/* 0.5 (e/+ x lo (e/abs (e/- x lo)))))
(defn- at-most [x hi] (e/* 0.5 (e/- (e/+ x hi) (e/abs (e/- x hi)))))

(def archytas-scalars
  "The numbers of `archytas-figure`, in order: B; K; I (K's foot); P (on
   the curve where the ring cuts the half-cylinder, at azimuth u) and its
   foot; AI, AK, AI^3, AD.AB^2; the three ratios AB:AI, AI:AK, AK:AD at K;
   at P: AI_P, AK_P, AB:AI_P, AI_P:AK_P, AK_P:AD; the angle PAD and the
   cone's half-angle (degrees), their difference; AD/AB; the azimuth of K."
  [:bx :by :kx :ky :kz :ix :iy :px :py :pz :pix :piy
   :ai :ak :ai3 :ad-ab2 :r1 :r2 :r3
   :pai :pak :p1 :p2 :p3 :p-angle :cone-angle :p-gap :ratio :uk])

(defn archytas-figure
  "Every point and number of the Archytas playground for AD = ad, AB = ab
   and P's azimuth u (radians). AB longer than AD is no chord: the cone is
   drawn with AB held at AD (it closes onto the axis) and the case says so."
  [ad ab u]
  (fn [_]
    (let [b (at-most ab ad)
          sq (fn [x] (e/sqrt (at-least x 0)))
          m (e/expt (e/* ad (e/square b)) (e// 1 3))
          kx (e// (e/square m) ad)
          ky (sq (e/- (e/square m) (e/square kx)))
          r (e// (e/square m) b)
          kz (sq (e/- (e/square r) (e/square m)))
          pm (e/* ad (e/cos u))
          px (e/* pm (e/cos u)) py (e/* pm (e/sin u))
          pz (sq (e/- (e/* ad pm) (e/square pm)))
          pr (sq (e/* ad pm))
          deg (/ 180 Math/PI)
          pa (e/* deg (e/atan (sq (e/- (e/square pr) (e/square px))) px))
          ca (e/* deg (e/atan (sq (e/- (e/square ad) (e/square b))) b))]
      [(e// (e/square b) ad) (e// (e/* b (sq (e/- (e/square ad) (e/square b)))) ad)
       kx ky kz kx ky px py pz px py
       m r (e/cube m) (e/* ad (e/square b))
       (e// b m) (e// m r) (e// r ad)
       pm pr (e// b (e/+ pm tiny)) (e// pm (e/+ pr tiny)) (e// pr ad)
       pa ca (e/- pa ca) (e// ad ab)
       (e/atan (sq (e/- (e/square ad) (e/square m))) m)])))

(def archytas-start "Archytas' Delian case: AD = 2, AB = 1, P short of K." {:ad 2 :ab 1 :u 0.45})

(def archytas-presets
  "The 'try this' states. :on-k puts P at K's azimuth, acos(cbrt(1/4)) =
   0.889294 for AD = 2AB."
  {:delian archytas-start
   :on-k {:ad 2 :ab 1 :u 0.889294}
   :eight {:ad 2 :ab 0.25 :u 0.45}
   :equal {:ad 2 :ab 2 :u 0.45}
   :no-chord {:ad 1.5 :ab 2 :u 0.45}})

(defn archytas-kind
  "The case of the given lines: :no-chord when AB exceeds the diameter AD
   (Archytas' hypothesis broken), else `means-kind` of AD : AB."
  [ad ab]
  (if (> (- (double ab) (double ad)) 0.01) :no-chord (means-kind (/ (double ad) (double ab)))))

(defn p-kind
  "Where P stands against the cone: :on (P is K), :inside (closer to the
   axis AD than the cone), :outside."
  [gap]
  (cond (< (Math/abs (double gap)) 0.25) :on (neg? gap) :inside :else :outside))

(def kernels
  "Every playground kernel, for alexandria.medium.kernel."
  {:curves {:f means-curves :params [1 2] :state [1] :opts {:simplify? false}}
   :fig {:f means-figure :params [1 2 2 1] :state [0] :opts {:simplify? false}}
   :conchoid {:f conchoid-curves :params [1 2] :state [1] :opts {:simplify? false}}
   :cfig {:f conchoid-figure :params [1 2 60 2.2] :state [0] :opts {:simplify? false}}
   :cissoid {:f cissoid-curves :params [1.2] :state [1] :opts {:simplify? false}}
   :sfig {:f cissoid-figure :params [1.2 1] :state [0] :opts {:simplify? false}}
   :afig {:f archytas-figure :params [2 1 0.45] :state [0] :opts {:simplify? false}}})
