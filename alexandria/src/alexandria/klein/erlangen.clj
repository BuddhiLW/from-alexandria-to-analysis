(ns alexandria.klein.erlangen
  "Klein's Erlangen programme (Vergleichende Betrachtungen ueber neuere
   geometrische Forschungen, Erlangen 1872; tr. M. W. Haskell, Bull. New
   York Math. Soc. 2, 1893): a geometry is the theory of the invariants of a
   group of transformations.

   One figure: a triangle ABC with its circumcircle, the line DE through C
   parallel to AB, the midpoint M of AB, and four collinear points D, C, F,
   E on DE (their cross-ratio). One transformation, chosen in a group:

     :euclid      motions: rotation theta, translation (tx ty), mirror sigma
     :similarity  + a scale s
     :affine      + a stretch r and a shear k
     :projective  + the perspective row (p q)
     :hyperbolic  Moebius maps of the unit disk
                  z -> (e^{i theta} z + a) / (1 + conj(a) e^{i theta} z),
                  the motions of Poincare's disk (alexandria.non-euclidean.disk).

   A group is four 0/1 flags [es ea ep h] that switch its parameters on; the
   kernels see every parameter and the flags mask those the group lacks. All
   kernels are Emmy arithmetic, branch-free, one-argument atan only (through
   alexandria.non-euclidean.disk/angle2), so alexandria.raster and the
   :raster backend of emmy-viewers compile them; every number the page
   shows comes from them.

   Properties compared before and after the map: the length |AB|, the angle
   ACB (measured between the image curves at C', so a conformal Moebius map
   keeps it), the ratio |AB| : |BC|, AB parallel to DE, M the midpoint of
   AB, the cross-ratio (D, E; C, F) by lengths (for a Moebius map the modulus
   of the complex cross-ratio, which it keeps), the circle through A B C
   (does the image of a fourth circle point lie on the circle through the
   images?), and the hyperbolic length d(A, B) of Poincare's disk."
  (:require [alexandria.grade :as grade]
            [alexandria.non-euclidean.disk :as d]
            [alexandria.raster :as raster]
            [emmy.env :as e]
            [hive-dsl.result :as r]
            [alexandria.structure.arith :as a]
            [alexandria.structure.group :as g]
            [alexandria.structure.groups :as gs]
            [alexandria.structure.invariant :as inv]))

;; ---------------------------------------------------------------------------
;; Groups

(def groups
  "The five groups of the playground: Klein's chain of the plane, smallest
   first, and the hyperbolic group beside it."
  [:euclid :similarity :affine :projective :hyperbolic])

(def chain [:euclid :similarity :affine :projective])

(def flags
  "[es ea ep h]: which parameters each group switches on."
  {:euclid [0 0 0 0] :similarity [1 0 0 0] :affine [1 1 0 0]
   :projective [1 1 1 0] :hyperbolic [0 0 0 1]})

(def group-names
  {:euclid "Euclidean motions"
   :similarity "similarities (Klein's principal group)"
   :affine "affine maps"
   :projective "projective maps (collineations)"
   :hyperbolic "hyperbolic motions of the disk (Moebius maps)"})

(def geometry-names
  {:euclid "Euclidean metric geometry" :similarity "elementary (similarity) geometry"
   :affine "affine geometry" :projective "projective geometry"
   :hyperbolic "hyperbolic (Lobachevsky) geometry"})

(def dimension
  "Real parameters of each group of the plane."
  {:euclid 3 :similarity 4 :affine 6 :projective 8 :hyperbolic 3})

(def params
  "The transformation parameters, in kernel order."
  [:theta :s :r :k :sigma :tx :ty :p :q :ax :ay])

(def param-ranges
  {:theta [-3.14 3.14 0.01] :s [0.3 2.5 0.01] :r [0.3 2.5 0.01] :k [-1.5 1.5 0.01]
   :sigma [-1 1 2] :tx [-1.5 1.5 0.01] :ty [-1.5 1.5 0.01] :p [-0.6 0.6 0.01]
   :q [-0.6 0.6 0.01] :ax [-0.9 0.9 0.01] :ay [-0.9 0.9 0.01]})

(def identity-params
  {:theta 0 :s 1 :r 1 :k 0 :sigma 1 :tx 0 :ty 0 :p 0 :q 0 :ax 0 :ay 0})

;; ---------------------------------------------------------------------------
;; Vectors

(def ^:private v+ a/c+)
(def ^:private v- a/c-)
(def ^:private sc a/cscale)
(defn- len [v] (e/sqrt (e/+ (a/dot v v) 1e-300)))
(def ^:private angle-between a/angle-between)

;; ---------------------------------------------------------------------------
;; The map

(defn effective
  "The parameters the group uses, the others masked to the identity's
   values. f = [es ea ep h], ps in the order of `params`."
  [[es ea ep h] [theta s rr k sigma tx ty p q ax ay]]
  (let [planar (e/- 1 h)]
    {:theta theta
     :s (e/+ 1 (e/* es (e/- s 1)))
     :r (e/+ 1 (e/* ea (e/- rr 1)))
     :k (e/* ea k)
     :sigma (e/+ 1 (e/* planar (e/- sigma 1)))
     :tx (e/* planar tx) :ty (e/* planar ty)
     :p (e/* ep p) :q (e/* ep q)
     :ax (e/* h ax) :ay (e/* h ay)}))

(defn projective-map
  "The planar map of parameters m: the linear part R(theta) s [[r k] [0 sigma]],
   the translation, then division by w = p x + q y + 1."
  [{:keys [theta s r k sigma tx ty p q]} [x y]]
  (let [u (e/+ (e/* r x) (e/* k y))
        v (e/* sigma y)
        c (e/cos theta)
        sn (e/sin theta)
        X (e/+ (e/* s (e/- (e/* c u) (e/* sn v))) tx)
        Y (e/+ (e/* s (e/+ (e/* sn u) (e/* c v))) ty)
        w (e/+ 1 (e/* p x) (e/* q y))]
    [(e/divide X w) (e/divide Y w)]))

(def hyperbolic-motions
  "The motions of Poincare's disk: alexandria.structure.groups/moebius-disk -1."
  (gs/moebius-disk -1))

(defn moebius-element
  "The motion of parameters m: rotate by theta, then carry 0 to a."
  [{:keys [theta ax ay]}]
  (g/compose hyperbolic-motions (gs/moebius-translation [ax ay]) (gs/moebius-rotation theta)))

(defn moebius-map
  "z -> T_a^-1(e^{i theta} z) = (e^{i theta} z + a) / (1 + conj(a) e^{i theta} z):
   the action of (moebius-element m), applied as the rotation (SO(2)) then
   the motion of the K = -1 space form carrying 0 to a."
  [{:keys [theta ax ay]} z]
  (d/from-origin [ax ay] (g/act gs/rotations theta z)))

(defn moebius-derivative
  "The complex derivative of `moebius-map` at z:
   e^{i theta} (1 - |a|^2) / (1 + conj(a) e^{i theta} z)^2."
  [{:keys [theta ax ay]} z]
  (let [aa [ax ay]
        ez (gs/rotate z theta)
        den (a/c+ a/one (a/c* (a/cconj aa) ez))]
    (a/c-div (a/cscale (e/- 1 (a/norm2 aa)) (a/cis theta)) (a/c* den den))))

(defn transform
  "z under the map of flags f and raw parameters ps: Moebius when h = 1,
   planar when h = 0 (the unused branch is the identity's, finite, so the
   blend is exact)."
  [f ps z]
  (let [m (effective f ps)
        h (nth f 3)
        [px py] (projective-map m z)
        [mx my] (moebius-map m z)]
    [(e/+ (e/* (e/- 1 h) px) (e/* h mx))
     (e/+ (e/* (e/- 1 h) py) (e/* h my))]))

;; ---------------------------------------------------------------------------
;; The figure

(def figure-start
  "The playground's starting triangle, inside the unit disk."
  {:ax0 -0.55 :ay0 -0.3 :bx0 0.6 :by0 -0.35 :cx0 0.1 :cy0 0.45})

(def dE "D = C - dE (B - A), E = C + dE (B - A)." 0.45)
(def fF "F = C + fF (B - A), between C and E." 0.2)
(def q-angle "The fourth circle point Q at this angle on the circumcircle." 2.2)

(defn figure-points
  "The named points from A, B, C: {:A :B :C :M :D :E :F :O :R}, O and R the
   circumcentre and circumradius."
  [A B C]
  (let [b (v- B A)
        c (v- C A)
        M (sc 0.5 (v+ A B))
        D (v- C (sc dE b))
        E (v+ C (sc dE b))
        F (v+ C (sc fF b))
        den (e/* 2 (a/cross b c))
        bb (a/dot b b)
        cc (a/dot c c)
        ox (e/divide (e/- (e/* (second c) bb) (e/* (second b) cc)) den)
        oy (e/divide (e/- (e/* (first b) cc) (e/* (first c) bb)) den)]
    {:A A :B B :C C :M M :D D :E E :F F :O (v+ A [ox oy]) :R (len [ox oy])}))

(defn- cross-ratio
  "(D, E; C, F) = (DC . EF) / (DF . EC), by lengths."
  [D E C F]
  (e/divide (e/* (len (v- C D)) (len (v- F E)))
            (e/+ (e/* (len (v- F D)) (len (v- C E))) 1e-300)))

(defn properties
  "[length angle ratio parallel midpoint cross-ratio circle hyp inside] of
   the figure pts, with q the (image of the) fourth circle point and ang the
   angle at C. parallel is |sin| of the angle between AB and DE, midpoint the
   distance of M from the midpoint of AB over |AB|, circle the relative gap
   of q from the circle through A B C, inside = min(1 - |A|^2, 1 - |B|^2)."
  [{:keys [A B C M D E F O R]} q ang]
  (let [ab (v- B A)
        de (v- E D)]
    [(len ab)
     ang
     (e/divide (len ab) (len (v- C B)))
     (e/divide (e/abs (a/cross ab de)) (e/* (len ab) (len de)))
     (e/divide (len (v- M (sc 0.5 (v+ A B)))) (len ab))
     (cross-ratio D E C F)
     (e/divide (e/abs (e/- (len (v- q O)) R)) R)
     (d/dist A B)
     (a/at-most (e/- 1 (a/norm2 A)) (e/- 1 (a/norm2 B)))]))

(def property-keys [:length :angle :ratio :parallel :midpoint :cross-ratio :circle :hyp :inside])

(defn compare-kernel
  "The playground's kernel: flags [es ea ep h], the raw parameters (order of
   `params`), the vertices A B C. Output [before(9) after(9) A' B' C' M' D' E'
   F' (14) devs(5)], devs = [|s - 1|, |k| + |r - 1|, |p| + |q|, |a|,
   |theta| + |t| + (1 - sigma)]: how far the map is from each smaller group."
  [& args]
  (fn [_]
    (let [[es ea ep h theta s rr k sigma tx ty p q ax ay x1 y1 x2 y2 x3 y3] args
          f [es ea ep h]
          ps [theta s rr k sigma tx ty p q ax ay]
          m (effective f ps)
          T (fn [z] (transform f ps z))
          pts (figure-points [x1 y1] [x2 y2] [x3 y3])
          Q0 (v+ (:O pts) (sc (:R pts) [(e/cos q-angle) (e/sin q-angle)]))
          [A' B' C' M' D' E' F'] (map #(T (pts %)) [:A :B :C :M :D :E :F])
          img (merge (figure-points A' B' C') {:M M' :D D' :E E' :F F'})
          u (v- (:A pts) (:C pts))
          v (v- (:B pts) (:C pts))
          ang0 (angle-between u v)
          dm (moebius-derivative m (:C pts))
          ang-planar (angle-between (v- A' C') (v- B' C'))
          ang-moebius (angle-between (a/c* dm u) (a/c* dm v))
          ang1 (e/+ (e/* (e/- 1 h) ang-planar) (e/* h ang-moebius))]
      (vec (concat (properties pts Q0 ang0)
                   (properties img (T Q0) ang1)
                   A' B' C' M' D' E' F'
                   [(e/abs (e/- (:s m) 1))
                    (e/+ (e/abs (:k m)) (e/abs (e/- (:r m) 1)))
                    (e/+ (e/abs (:p m)) (e/abs (:q m)))
                    (e/sqrt (e/+ (e/square (:ax m)) (e/square (:ay m))))
                    (e/+ (e/abs theta) (e/abs (:tx m)) (e/abs (:ty m)) (e/- 1 (:sigma m)))])))))

(def compare-index
  (merge (zipmap (map #(keyword (str "before-" (name %))) property-keys) (range 9))
         (zipmap (map #(keyword (str "after-" (name %))) property-keys) (range 9 18))
         (zipmap [:A'x :A'y :B'x :B'y :C'x :C'y :M'x :M'y :D'x :D'y :E'x :E'y :F'x :F'y] (range 18 32))
         (zipmap [:dev-s :dev-affine :dev-projective :dev-hyperbolic :dev-motion] (range 32 37))))

(defn image-curve
  "Curve kernel at u in [0 1]: the image of the segment P0 P1 (kind 0) or of
   the circumcircle at angle 2 pi u (kind 1), clamped to |x|, |y| <= 60 so a
   curve through the line at infinity stays drawable."
  [& args]
  (fn [u]
    (let [[es ea ep h theta s rr k sigma tx ty p q ax ay x1 y1 x2 y2 x3 y3 kind px0 py0 px1 py1] args
          f [es ea ep h]
          ps [theta s rr k sigma tx ty p q ax ay]
          {:keys [O R]} (figure-points [x1 y1] [x2 y2] [x3 y3])
          seg (v+ [px0 py0] (sc u (v- [px1 py1] [px0 py0])))
          a (e/* 2 Math/PI u)
          circ (v+ O (sc R [(e/cos a) (e/sin a)]))
          z [(e/+ (e/* (e/- 1 kind) (first seg)) (e/* kind (first circ)))
             (e/+ (e/* (e/- 1 kind) (second seg)) (e/* kind (second circ)))]
          [X Y] (transform f ps z)]
      [(a/clamp X -60 60) (a/clamp Y -60 60)])))

;; ---------------------------------------------------------------------------
;; Verdicts (JVM: the hierarchy table and the tests; the page applies the
;; same tolerance to the same raster numbers)

(def tolerance "Relative tolerance under which a property is unchanged." 1e-6)

(defn invariant?
  "Did property k survive (before, after; inside-after for :hyp)?
   alexandria.structure.invariant/holds? with :vanishes for the defect
   properties and :equal for the rest."
  [k before after inside]
  (case k
    (:parallel :midpoint :circle) (inv/holds? :vanishes before after tolerance)
    :hyp (and (pos? inside) (inv/holds? :equal before after tolerance))
    (inv/holds? :equal before after tolerance)))

(def verdict-keys [:length :angle :ratio :parallel :midpoint :cross-ratio :circle :hyp])

(defn evaluate
  "The compare kernel on the JVM through raster: {:before :after :invariant
   :devs} for group g, parameters pm (a map over `params`) and the figure fig."
  ([g pm] (evaluate g pm figure-start))
  ([g pm fig]
   (let [args (concat (flags g) (map pm params) (map fig [:ax0 :ay0 :bx0 :by0 :cx0 :cy0]))
         o (vec (raster/value (fn [_] ((apply compare-kernel args) nil)) 0))
         before (zipmap property-keys (subvec o 0 9))
         after (zipmap property-keys (subvec o 9 18))]
     {:before before
      :after after
      :invariant (into {} (for [k verdict-keys]
                            [k (invariant? k (before k) (after k) (:inside after))]))
      :images (subvec o 18 32)
      :devs (subvec o 32 37)})))

(def sample
  "One generic parameter set; each group masks what it does not have."
  {:theta 0.7 :s 1.6 :r 1.4 :k 0.6 :sigma -1 :tx 0.4 :ty -0.3 :p 0.25 :q -0.15 :ax 0.3 :ay 0.2})

(def property-labels
  {:length "length |AB|" :angle "angle ACB" :ratio "ratio |AB| : |BC|"
   :parallel "AB ∥ DE" :midpoint "M the midpoint of AB"
   :cross-ratio "cross-ratio (D, E; C, F)" :circle "circle through A, B, C"
   :hyp "hyperbolic length d(A, B)"})

(defn invariant-table
  "[{:group :invariant #{...} :count :dimension}] for the sample map."
  []
  (vec (for [g groups
             :let [{:keys [invariant]} (evaluate g sample)
                   inv (set (keep (fn [[k ok]] (when ok k)) invariant))]]
         {:group g :invariant inv :count (count inv) :dimension (dimension g)})))

;; ---------------------------------------------------------------------------
;; Inverses (for the round-trip grade)

(defn- matrix
  "The 3x3 homogeneous matrix of the planar map of parameters m (numbers)."
  [{:keys [theta s r k sigma tx ty p q]}]
  (let [c (Math/cos theta) sn (Math/sin theta)]
    [[(* s c r) (* s (- (* c k) (* sn sigma))) tx]
     [(* s sn r) (* s (+ (* sn k) (* c sigma))) ty]
     [p q 1.0]]))

(defn- apply-h [M p] (g/act gs/projective M p))

(defn inverse-gaps
  "Map then inverse, minus the start, for the sample map of every group on
   the three vertices: images by raster, inverses by
   alexandria.structure.group/inverse in PGL(3) or in the disk's motions."
  []
  (let [{:keys [ax0 ay0 bx0 by0 cx0 cy0]} figure-start
        pts [[ax0 ay0] [bx0 by0] [cx0 cy0]]]
    (vec (for [gr groups
               :let [f (flags gr)
                     ps (mapv sample params)
                     m (effective f ps)
                     imgs (partition 2 (take 6 (:images (evaluate gr sample))))]
               [z w] (map vector pts imgs)
               :let [back (if (= gr :hyperbolic)
                            (g/act hyperbolic-motions (g/inverse hyperbolic-motions (moebius-element (update-vals m double))) (vec w))
                            (apply-h (g/inverse gs/projective (matrix m)) (vec w)))]
               gap (map - back z)]
           gap))))

;; ---------------------------------------------------------------------------
;; Emmy: invariants proved

(defn- variant [res] (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails))

(defn moebius-1d "x -> (a x + b) / (c x + d)." [x]
  (e/divide (e/+ (e/* 'a x) 'b) (e/+ (e/* 'c x) 'd)))

(defn cross-ratio-1d
  "(x1, x2; x3, x4) = ((x3 - x1)(x4 - x2)) / ((x4 - x1)(x3 - x2))."
  [x1 x2 x3 x4]
  (e/divide (e/* (e/- x3 x1) (e/- x4 x2)) (e/* (e/- x4 x1) (e/- x3 x2))))

(defn cross-ratio-difference []
  (let [xs '[x_1 x_2 x_3 x_4]]
    (e/- (apply cross-ratio-1d (map moebius-1d xs)) (apply cross-ratio-1d xs))))

(defn midpoint-difference
  "An affine map applied to the midpoint of PQ, minus the midpoint of the images."
  []
  (let [m (effective [1 1 0 0] '[theta s r k 1 t_x t_y 0 0 0 0])
        P '[x_1 y_1] Q '[x_2 y_2]]
    (v- (projective-map m (sc 1/2 (v+ P Q)))
        (sc 1/2 (v+ (projective-map m P) (projective-map m Q))))))

(defn distance-difference
  "|P' - Q'|^2 - |P - Q|^2 under a motion."
  []
  (let [m (effective [0 0 0 0] '[theta 1 1 0 1 t_x t_y 0 0 0 0])
        P '[x_1 y_1] Q '[x_2 y_2]
        u (v- (projective-map m P) (projective-map m Q))
        v (v- P Q)]
    (e/- (a/dot u u) (a/dot v v))))

(defn graded
  "[{:label :grade}] for the Erlangen shelf."
  []
  (let [tab (invariant-table)
        by (into {} (map (juxt :group :invariant) tab))
        hyp-gaps (for [[ax ay th] [[0.3 0.2 0.7] [-0.5 0.1 -1.2] [0.0 0.6 2.0]]
                       :let [{:keys [before after]} (evaluate :hyperbolic (assoc identity-params :ax ax :ay ay :theta th))]]
                   (- (:hyp after) (:hyp before)))
        nested? (every? true? (for [[g h] (partition 2 1 chain)] (every? (by g) (by h))))
        grows? (apply < (map #(count (by %)) (reverse chain)))]
    [{:label "the cross-ratio of four points of a line is unchanged by x ↦ (ax + b)/(cx + d)"
      :grade (variant (grade/grade :symbolic [(cross-ratio-difference)] 0))}
     {:label "an affine map sends the midpoint of PQ to the midpoint of P'Q'"
      :grade (variant (grade/grade :symbolic (midpoint-difference) 0))}
     {:label "a motion keeps the squared distance |P − Q|²"
      :grade (variant (grade/grade :symbolic [(distance-difference)] 0))}
     {:label "a Moebius map of the disk keeps the hyperbolic length d(A, B) (three maps, raster)"
      :grade (variant (grade/grade :numeric hyp-gaps 1e-9))}
     {:label "nesting: along Euclid ⊂ similarity ⊂ affine ⊂ projective each larger group keeps a subset of the smaller one's invariants, and strictly fewer (raster)"
      :grade (variant (grade/grade :numeric [(if (and nested? grows?) 0.0 1.0)] 1e-12))}
     {:label "the map then its inverse is the identity, every group (raster)"
      :grade (variant (grade/grade :numeric (inverse-gaps) 1e-9))}]))
