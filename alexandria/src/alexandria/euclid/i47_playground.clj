(ns alexandria.euclid.i47-playground
  "Euclid I.47 as a playground: the triangle ABC with every vertex free.

   I.47's own construction (Heath 1908): the squares BDEC on BC, GB on BA
   and HC on AC; through A the parallel AL to BD, cutting BC at K (the
   foot of the perpendicular from A); the parallelogram BL equal to the
   square GB and CL equal to HC. With the vertices free the reader can
   open or close the angle at A and watch the two halves of the windmill
   stop matching:

     square on AB - rectangle BL = square on AC - rectangle CL
                                 = AB . AC cos A   (= (B - A).(C - A))

   which is 0 only when the angle at A is right (I.47). Off the right
   angle Book II takes over: the perpendicular BD from B on CA, and
   a^2 = b^2 + c^2 - 2 CA.AD with AD signed (II.12 obtuse, II.13 acute).

   `figure` is an Emmy function of the six coordinates; raster compiles it
   for the browser, and the tests evaluate it on the JVM. Branch-free: the
   outward side of each square comes from the sign of an orientation
   (x / (|x| + tiny)), every division is guarded by `tiny`."
  (:require [emmy.env :as e]))

(def tiny 1e-12)

(defn- sub [[px py] [qx qy]] [(e/- px qx) (e/- py qy)])
(defn- add [[px py] [qx qy]] [(e/+ px qx) (e/+ py qy)])
(defn- scale [s [x y]] [(e/* s x) (e/* s y)])
(defn- dot [[px py] [qx qy]] (e/+ (e/* px qx) (e/* py qy)))
(defn- cross [[px py] [qx qy]] (e/- (e/* px qy) (e/* py qx)))
(defn- sgn [x] (e// x (e/+ (e/abs x) tiny)))

(defn- outward
  "The side vector of the square on PQ standing away from R: PQ turned a
   right angle, to the side opposite R (same length as PQ)."
  [P Q R]
  (let [[vx vy] (sub Q P)
        n [(e/negate vy) vx]]
    (scale (e/negate (sgn (dot n (sub R P)))) n)))

(def points
  "Point outputs, in order, two numbers each (plane, y up)."
  [:A :B :C
   :ab1 :ab2 :ac1 :ac2 :bc1 :bc2   ; outer corners of the squares (Q + w, P + w)
   :K :L                           ; AL: K on BC, L on the far side of the square on BC
   :D :r1 :r2])                    ; II.12-13: foot D of B on CA; far corners of CA.AD

(def scalars
  "Then: a^2, b^2, c^2, BL, CL, AB.AC cos A, cos A, the angle A in degrees,
   a^2 - (b^2 + c^2), and AD (signed, along AC)."
  [:a2 :b2 :c2 :BL :CL :dot :cos :angle :excess :ad])

(defn figure
  "Every point and number of the playground for A = (ax, ay), B = (bx, by),
   C = (cx, cy). State is ignored (one row)."
  [ax ay bx by cx cy]
  (fn [_]
    (let [A [ax ay] B [bx by] C [cx cy]
          a2 (dot (sub C B) (sub C B))
          b2 (dot (sub C A) (sub C A))
          c2 (dot (sub B A) (sub B A))
          w-ab (outward A B C) w-ac (outward A C B) w-bc (outward B C A)
          t (e// (dot (sub A B) (sub C B)) (e/+ a2 tiny))
          K (add B (scale t (sub C B)))
          L (add K w-bc)
          d (dot (sub B A) (sub C A))
          u (e// d (e/+ b2 tiny))
          D (add A (scale u (sub C A)))
          pts [A B C (add B w-ab) (add A w-ab) (add C w-ac) (add A w-ac)
               (add C w-bc) (add B w-bc) K L D (add D w-ac) (add A w-ac)]]
      (vec (concat (mapcat identity pts)
                   [a2 b2 c2 (e/* t a2) (e/* (e/- 1 t) a2) d
                    (e// d (e/+ (e/sqrt (e/* b2 c2)) tiny))
                    (e/* (/ 180 Math/PI) (e/atan (e/abs (cross (sub B A) (sub C A))) d))
                    (e/- a2 (e/+ b2 c2))
                    (e// d (e/+ (e/sqrt b2) tiny))])))))

(defn named
  "{name value} of one output row of `figure`."
  [row]
  (let [n (count points)]
    (merge (zipmap points (map vec (partition 2 (take (* 2 n) row))))
           (zipmap scalars (drop (* 2 n) row)))))

(def right-band "|cos A| below this names the angle right." 0.004)

(defn kind
  "The proposition the triangle stands under, from cos A; :flat when the
   three points are (nearly) on one line, so there is no triangle."
  [{:keys [cos angle]}]
  (cond (or (< angle 1.0) (> angle 179.0)) :flat
        (< (Math/abs (double cos)) right-band) :right
        (pos? cos) :acute
        :else :obtuse))

(def start
  "Euclid's figure: the right angle at A, AB = 3 up, AC = 4 across."
  [0 0 0 3 4 0])

(def presets
  "The 'try this' states [ax ay bx by cx cy]."
  {:euclid start
   :obtuse [0 0 -1.5 2.6 4 0]
   :acute [0 0 1.8 2.4 4 0]
   :meno [0 0 0 3 3 0]
   :flat [0 0 -2 0 4 0]})

(def kernel
  "The figure for alexandria.medium.kernel."
  {:f figure :params start :state [0] :opts {:simplify? false}})
