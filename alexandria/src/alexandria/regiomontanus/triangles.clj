(ns alexandria.regiomontanus.triangles
  "Regiomontanus, De triangulis omnimodis (written 1464, printed Nuremberg
   1533), Book II, theorem 1: in every rectilinear triangle the ratio of
   side to side is the ratio of the sines of the angles opposite them,

     a / sin A = b / sin B = c / sin C  (= 2R, the circumscribed diameter),

   and the five ways of solving a triangle from three data, all through
   that law and the law of cosines.

   Two Emmy setups, both lowered to raster kernels:

   - the LAW OF SINES on the circumscribed circle: the vertices are three
     points on a circle of radius R at polar angles tA, tB, tC (degrees).
     `readout` measures the sides as distances and the angles as angles
     between vectors (atan2 of cross and dot), so the equal ratios are a
     measurement, not an assumption. `tie` ties the angle A, the side a and
     R (a = 2R sin A) to the positions: moving A (or a) keeps the vertex A
     and R and swings B and C symmetrically about the midpoint of their arc;
     moving R scales; dragging a vertex (its polar angle a source) re-reads
     A and a.
   - the SOLVER: one kernel per case (SSS, SAS, ASA, AAS, SSA), each from
     its three data to the whole triangle [a b c A B C] with a validity
     flag; SSA carries the second solution (B' = 180 - B) and its flag, so
     the page shows two, one or none. The triangle is drawn with A at the
     origin and AB along the axis: C = (b cos A, b sin A), B = (c, 0); in
     SSA the two solutions share C and differ in B."
  (:require [alexandria.grade :as grade]
            [alexandria.raster :as raster]
            [emmy.env :as e]
            [hive-dsl.result :as r]))

;; ---------------------------------------------------------------------------
;; Branch-free helpers (raster compiles them)

(defn at-least [x lo] (e/* 0.5 (e/+ x lo (e/abs (e/- x lo)))))
(defn at-most [x hi] (e/* 0.5 (e/- (e/+ x hi) (e/abs (e/- x hi)))))
(defn clamp [x lo hi] (at-most (at-least x lo) hi))
(defn sgn "x/|x|, kept finite at 0." [x] (e/divide x (e/+ (e/abs x) 1e-15)))
(defn step-up "1 where x > 0, 0 where x < 0." [x] (e/* 0.5 (e/+ 1 (sgn x))))

(defn value
  "raster/value unsimplified: the kernels here are branch-free (sums of
   abs), which Emmy's simplifier expands past the JVM's method size."
  [f & xs]
  (binding [raster/*simplify?* false] (apply raster/value f xs)))

(defn rad [d] (e/* d (/ Math/PI 180)))
(defn deg [r] (e/* r (/ 180 Math/PI)))

(defn sin-d [d] (e/sin (rad d)))
(defn cos-d [d] (e/cos (rad d)))

(defn asin-d "arcsin in degrees, atan2 form, finite at 1." [x]
  (deg (e/atan x (e/sqrt (at-least (e/- 1 (e/square x)) 0)))))
(defn acos-d "arccos in degrees, atan2 form." [x]
  (deg (e/atan (e/sqrt (at-least (e/- 1 (e/square x)) 0)) x)))

(defn- dist [[x0 y0] [x1 y1]] (e/sqrt (e/+ (e/square (e/- x1 x0)) (e/square (e/- y1 y0)))))

(defn- angle-at
  "The angle at p of the triangle p q s, degrees (atan2 of |cross|, dot)."
  [[px py] [qx qy] [sx sy]]
  (let [ux (e/- qx px) uy (e/- qy py) vx (e/- sx px) vy (e/- sy py)]
    (deg (e/atan (e/abs (e/- (e/* ux vy) (e/* uy vx))) (e/+ (e/* ux vx) (e/* uy vy))))))

(defn- on-circle [R t] [(e/* R (cos-d t)) (e/* R (sin-d t))])

;; ---------------------------------------------------------------------------
;; The law of sines on the circumscribed circle

(defn readout
  "From the polar angles tA tB tC (degrees) and R: [Ax Ay Bx By Cx Cy
   a b c A B C sinA sinB sinC a/sinA b/sinB c/sinC 2R A+B+C]."
  [tA tB tC R]
  (fn [_]
    (let [P (on-circle R tA) Q (on-circle R tB) S (on-circle R tC)
          a (dist Q S) b (dist P S) c (dist P Q)
          A (angle-at P Q S) B (angle-at Q P S) C (angle-at S P Q)
          sA (sin-d A) sB (sin-d B) sC (sin-d C)]
      [(first P) (second P) (first Q) (second Q) (first S) (second S)
       a b c A B C sA sB sC (e/divide a sA) (e/divide b sB) (e/divide c sC) (e/* 2 R) (e/+ A B C)])))

(def readout-index
  {:Ax 0 :Ay 1 :Bx 2 :By 3 :Cx 4 :Cy 5 :a 6 :b 7 :c 8 :A 9 :B 10 :C 11
   :sinA 12 :sinB 13 :sinC 14 :ra 15 :rb 16 :rc 17 :D 18 :sum 19})

(def tied
  "The tied keys: the three angles, the three sides and R. The vertices
   sit on the circle at polar angles phi (A), phi + 2C (B), phi + 2C + 2A
   (C): by the inscribed angle (Euclid III.20) the arc a side cuts off is
   twice the angle opposite it."
  [:A :B :C :a :b :c :R])

(def ranges
  "[min max step]; a side's range is for the largest R, the tie clamps it to 2R."
  {:A [2 176 0.5] :B [2 176 0.5] :C [2 176 0.5]
   :a [0.02 4 0.01] :b [0.02 4 0.01] :c [0.02 4 0.01] :R [0.5 2 0.01]})

(def labels {:A "angle A (°)" :B "angle B (°)" :C "angle C (°)" :a "side a = BC" :b "side b = CA" :c "side c = AB" :R "radius R"})

(def start "The opening triangle." {:A 50 :B 65 :R 1 :phi 110})

(defn of-angles
  "[A B C a b c R] from two angles and R (C = 180 - A - B)."
  [A B R]
  (let [C (e/- 180 A B)]
    [A B C (e/* 2 R (sin-d A)) (e/* 2 R (sin-d B)) (e/* 2 R (sin-d C)) R]))

(def ^:private room 2)

(defn- side->angle
  "The angle opposite a side x on the circle of radius R, on the branch of
   the current angle (obtuse stays obtuse)."
  [x R cur]
  (let [acute (asin-d (clamp (e/divide x (e/* 2 R)) 0.02 0.9999))]
    (e/+ 90 (e/* (sgn (e/- cur 90)) (e/- 90 acute)))))

(defn- keep-room
  "Clamp the moved angle x so the held angle h leaves room for the third."
  [x h]
  (clamp x room (e/- 180 h room)))

(defn- from-A [A B _ _ _ _ R] (let [A (keep-room A B)] (of-angles A B R)))
(defn- from-B [A B _ _ _ _ R] (let [B (keep-room B A)] (of-angles A B R)))
(defn- from-C [A _ C _ _ _ R] (let [C (keep-room C A)] (of-angles A (e/- 180 A C) R)))
(defn- from-a [A B _ a _ _ R] (let [A (keep-room (side->angle a R A) B)] (of-angles A B R)))
(defn- from-b [A B _ _ b _ R] (let [B (keep-room (side->angle b R B) A)] (of-angles A B R)))
(defn- from-c [A B C _ _ c R] (let [C (keep-room (side->angle c R C) A)] (of-angles A (e/- 180 A C) R)))
(defn- from-R [A B _ _ _ _ R] (of-angles A B (clamp R 0.5 2)))

(defn tie
  "The tie kernel: the solved [A B C a b c R] for each source, in the
   order of `tied` (49 numbers). Policies: an angle moves with R and A held
   (B held when A moves), the third angle absorbing the change; a side
   moves its opposite angle (a = 2R sin A, inverse on the branch of the
   current angle), the same angle held; R scales the sides, the angles
   held."
  [A B C a b c R]
  (fn [_] (vec (mapcat #(% A B C a b c R) [from-A from-B from-C from-a from-b from-c from-R]))))

(defn positions
  "The vertex polar angles (degrees) of the tied state: [tA tB tC]."
  [phi A C]
  [phi (e/+ phi (e/* 2 C)) (e/+ phi (e/* 2 C) (e/* 2 A))])

(defn- arc-ccw
  "The counter-clockwise arc from polar angle s to t, in (0, 360), degrees
   (atan2 of the turned difference, shifted into the positive range)."
  [s t]
  (let [d (e/- t s) x (cos-d d) y (sin-d d)
        g (deg (e/atan y x))]
    (e/+ g (e/* 360 (step-up (e/- 0 g))))))

(defn drag
  "A vertex dragged on the circle: from the three polar angles (A at tA,
   then B, then C counter-clockwise) the angles by the inscribed-angle
   theorem, [A B C phi] with A = arc(B->C)/2, C = arc(A->B)/2,
   B = 180 - A - C, phi = tA. An order crossed by the drag is refused in
   the page (an arc beyond 360 - 2 room)."
  [tA tB tC]
  (fn [_]
    (let [C (e/* 0.5 (arc-ccw tA tB)) A (e/* 0.5 (arc-ccw tB tC))]
      [A (e/- 180 A C) C tA])))

(defn initial-state "The tied state of `start`, by raster." []
  (assoc (zipmap tied (value (fn [A B R] (of-angles A B R)) (:A start) (:B start) (:R start))) :phi (:phi start)))

;; ---------------------------------------------------------------------------
;; Solving a triangle: the five cases

(def cases
  "The cases, each with its three data in order and their ranges and
   starting values."
  [{:id :SSS :name "three sides" :data [:a :b :c] :init [4 5 6]}
   {:id :SAS :name "two sides and the angle between them" :data [:b :A :c] :init [5 60 6]}
   {:id :ASA :name "two angles and the side between them" :data [:B :a :C] :init [50 6 70]}
   {:id :AAS :name "two angles and a side opposite one of them" :data [:A :B :a] :init [40 60 5]}
   {:id :SSA :name "two sides and an angle opposite one of them (the ambiguous case)" :data [:a :b :A] :init [4 5 40]}])

(def data-ranges
  {:a [0.5 10 0.05] :b [0.5 10 0.05] :c [0.5 10 0.05] :A [1 178 0.5] :B [1 178 0.5] :C [1 178 0.5]})

(defn- whole
  "[ok a b c A B C Cx Cy] with A at the origin and B = (c, 0)."
  [ok a b c A B C]
  [ok a b c A B C (e/* b (cos-d A)) (e/* b (sin-d A))])

(defn sss [a b c]
  (let [A (acos-d (clamp (e/divide (e/- (e/+ (e/square b) (e/square c)) (e/square a)) (e/* 2 b c)) -1 1))
        B (acos-d (clamp (e/divide (e/- (e/+ (e/square a) (e/square c)) (e/square b)) (e/* 2 a c)) -1 1))
        ok (step-up (at-most (at-most (e/- (e/+ b c) a) (e/- (e/+ a c) b)) (e/- (e/+ a b) c)))]
    (whole ok a b c A B (e/- 180 A B))))

(defn sas [b A c]
  (let [a (e/sqrt (at-least (e/- (e/+ (e/square b) (e/square c)) (e/* 2 b c (cos-d A))) 0))
        B (deg (e/atan (e/* b (sin-d A)) (e/- c (e/* b (cos-d A)))))]
    (whole 1 a b c A B (e/- 180 A B))))

(defn asa [B a C]
  (let [A (e/- 180 B C) sA (sin-d (at-least A 0.01))]
    (whole (step-up A) a (e/divide (e/* a (sin-d B)) sA) (e/divide (e/* a (sin-d C)) sA) A B C)))

(defn aas [A B a]
  (let [C (e/- 180 A B) sA (sin-d A)]
    (whole (step-up C) a (e/divide (e/* a (sin-d B)) sA) (e/divide (e/* a (sin-d (at-least C 0.01))) sA) A B C)))

(defn ssa
  "Given a, b and A (opposite a): sin B = b sin A / a. None when that
   exceeds 1, or when A is not acute and a <= b; otherwise B = arcsin, and
   when A is acute and a < b also B' = 180 - B, a second triangle with the
   same C and a shorter c. The flags are read off the data, not off the
   solved angles, so the kernel stays small."
  [a b A]
  (let [sB (e/divide (e/* b (sin-d A)) a)
        B1 (asin-d (clamp sB -1 1)) B2 (e/- 180 B1)
        C1 (e/- 180 A B1) C2 (e/- 180 A B2)
        sA (sin-d A)
        c-of (fn [C] (e/divide (e/* a (sin-d C)) sA))
        fits (step-up (e/- 1 sB))
        acute (step-up (e/- 90 A))
        shorter (step-up (e/- b a))
        ok1 (e/* fits (e/- 1 (e/* (e/- 1 acute) shorter)))
        ok2 (e/* fits acute shorter)]
    (into (whole ok1 a b (c-of C1) A B1 C1) [ok2 (c-of C2) B2 C2 sB])))

(def solve-index {:ok 0 :a 1 :b 2 :c 3 :A 4 :B 5 :C 6 :Cx 7 :Cy 8 :ok2 9 :c2 10 :B2 11 :C2 12 :sinB 13})

(def solvers {:SSS sss :SAS sas :ASA asa :AAS aas :SSA ssa})

(defn solve-kernel
  "(x y z) -> (fn [_] out) for case id, the SSA width padded so every case
   has the same 14 outputs."
  [id]
  (fn [x y z]
    (fn [_]
      (let [out ((solvers id) x y z)]
        (vec (take 14 (concat out (repeat 0))))))))

(defn solve
  "JVM: the solved triangle for case id and data [x y z], by raster."
  [id xyz]
  (let [k (solve-kernel id)]
    (zipmap (sort-by solve-index (keys solve-index))
            (apply value (fn [x y z] ((k x y z) 0)) xyz))))

(defn case-name
  "The case as the page names it, from the solved output (JVM side; the
   page carries the same rule)."
  [id out]
  (let [ok (> (:ok out) 0.5) ok2 (> (get out :ok2 0) 0.5)]
    (cond (not ok) :none
          (and (= id :SSA) ok2) :two
          :else :one)))

;; ---------------------------------------------------------------------------
;; Emmy: identities and grades

(def identities
  "[label difference] pairs reduced to 0 by Emmy. Angle sums are written
   out by the addition theorem (cos(u +- A) = cos u cos A -+ sin u sin A),
   so each difference is a polynomial in sines and cosines."
  (let [R 'R cu (e/cos 'u) su (e/sin 'u) cA (e/cos 'A) sA (e/sin 'A)
        P1 [(e/* R (e/- (e/* cu cA) (e/* su sA))) (e/* R (e/+ (e/* su cA) (e/* cu sA)))]
        P2 [(e/* R (e/+ (e/* cu cA) (e/* su sA))) (e/* R (e/- (e/* su cA) (e/* cu sA)))]
        d2 (fn [[x0 y0] [x1 y1]] (e/+ (e/square (e/- x1 x0)) (e/square (e/- y1 y0))))]
    [["II.1 on the circle: the chord between the points at u + A and u - A (central angle 2A, inscribed angle A) is 2R sin A, so a / sin A = 2R"
      (e/- (d2 P1 P2) (e/square (e/* 2 R sA)))]
     ["II.1 by Regiomontanus' perpendicular: the height from C is b sin A = a sin B, so a/sin A = b/sin B"
      (let [h1 (e/* 'b (e/sin 'A)) a (e/divide (e/* 'b (e/sin 'A)) (e/sin 'B))]
        (e/- (e/* a (e/sin 'B)) h1))]
     ["law of cosines from the coordinates: |B - C|^2 = b^2 + c^2 - 2bc cos A (C = (b cos A, b sin A), B = (c, 0))"
      (e/- (e/+ (e/square (e/- (e/* 'b (e/cos 'A)) 'c)) (e/square (e/* 'b (e/sin 'A))))
           (e/+ (e/square 'b) (e/square 'c) (e/* -2 'b 'c (e/cos 'A))))]
     ["forward then inverse: a = 2R sin A, then A = arcsin(a / 2R), gives a back"
      (e/- (e/* 2 R (e/sin (e/asin (e/divide 'a (e/* 2 R))))) 'a)]
     ["SSA: B and 180° - B have one sine (sin 180° = 0, cos 180° = -1 in the addition theorem), so both satisfy a sin B = b sin A"
      (e/- (e/- (e/* 0 (e/cos 'B)) (e/* -1 (e/sin 'B))) (e/sin 'B))]]))

(def tex
  {:law "\\frac{a}{\\sin A} = \\frac{b}{\\sin B} = \\frac{c}{\\sin C} = 2R"
   :cosines "a^2 = b^2 + c^2 - 2bc\\cos A"
   :tie "a = 2R\\sin A,\\qquad A = \\arcsin\\frac{a}{2R}\\ \\text{(on the branch of the current } A)"
   :ssa "\\sin B = \\frac{b\\sin A}{a},\\qquad B \\in \\{\\arcsin(\\cdot),\\ 180^\\circ - \\arcsin(\\cdot)\\}"})

(defn derived-tex []
  {:law (e/->TeX (e/simplify (e/divide (e/* 2 'R (e/sin 'A)) (e/sin 'A))))
   :cosines (e/->TeX (e/simplify (e/+ (e/square (e/- (e/* 'b (e/cos 'A)) 'c)) (e/square (e/* 'b (e/sin 'A))))))})

(defn- variant [res] (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails))

(defn- close-all [xs tol] (variant (grade/grade :numeric xs tol)))

(defn graded
  "[{:label :grade}]: identities (proved); the measured ratios equal 2R,
   the tie round trip, and every solver reproducing a known triangle
   (numeric, raster)."
  []
  (let [k (fn [& xs] ((apply readout xs) 0))
        outs (for [[ta tb tc R] [[100 190 290 1] [10 140 200 1.7] [80 95 300 0.6]]]
               (apply value k [ta tb tc R]))
        ratio-err (for [o outs i [15 16 17]] (- (nth o i) (nth o 18)))
        st (initial-state)
        tie-out (apply value (fn [& xs] ((apply tie xs) 0)) (map st tied))
        n (count tied)
        ;; forward then inverse: every source block equals the state
        tie-err (for [i (range n) j (range n)] (- (nth tie-out (+ (* i n) j)) (st (tied j))))
        ;; the drag kernel reads the same angles back from the positions
        [tA tB tC] (map double (value (fn [phi A C] (positions phi A C)) (:phi st) (:A st) (:C st)))
        drag-err (map - (take 3 (value (fn [x y z] ((drag x y z) 0)) tA tB tC)) [(:A st) (:B st) (:C st)])
        ref (solve :SSS [4 5 6])
        {:keys [A B C]} ref
        cases-err (concat
                   (map - (map (solve :SAS [5 A 6]) [:a :B :C]) [4 B C])
                   (map - (map (solve :ASA [B 4 C]) [:b :c :A]) [5 6 A])
                   (map - (map (solve :AAS [A B 4]) [:b :c :C]) [5 6 C])
                   [(- (:c (solve :SSA [4 5 A])) 6)])]
    (-> (mapv (fn [[label diff]] {:label label :grade (variant (grade/grade :symbolic [diff]))}) identities)
        (conj {:label "a/sin A, b/sin B and c/sin C, measured on three inscribed triangles, all equal the diameter 2R (raster)"
               :grade (close-all ratio-err 1e-9) :engine :raster}
              {:label "the tie, forward then inverse: from each of A, B, C, a, b, c and R the kernel returns the same triangle (raster)"
               :grade (close-all (concat tie-err drag-err) 1e-9) :engine :raster}
              {:label "SAS, ASA, AAS and SSA rebuild the SSS triangle 4, 5, 6 from three of its parts (raster)"
               :grade (close-all cases-err 1e-9) :engine :raster}))))
