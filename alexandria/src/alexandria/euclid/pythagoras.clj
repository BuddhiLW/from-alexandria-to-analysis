(ns alexandria.euclid.pythagoras
  "Euclid I.47 with its neighbours II.12 and II.13: the square on BC
   against the squares on BA and AC, for any angle at A. Emmy derives the
   relation; the widget (alexandria.euclid.pythagoras-widget) ties the
   sides by it.

   The frame: A at the origin, AC along the x axis, B above it.

     A = (0, 0)   C = (b, 0)   B = (c cos A, c sin A)
     D, the foot of the perpendicular from B on AC: AD = c cos A (signed),
     BD = c sin A

   with Euclid's letters for the sides: a = BC (subtending the angle A),
   b = AC, c = AB. Writing B = (d, h), d = AD, the two distances are

     c^2 = d^2 + h^2,   a^2 = (b - d)^2 + h^2

   and Emmy eliminates h: a^2 = b^2 + c^2 - 2 b d, i.e. 2 CA.AD (Euclid's
   'twice the rectangle contained by CA, AD').

     d = 0   the angle at A is right: a^2 = b^2 + c^2            (I.47)
     d < 0   obtuse at A, D falls outside on CA produced: the square on
             BC is GREATER than the two by 2 CA.AD               (II.12)
     d > 0   acute at A: it is LESS by 2 CA.AD                   (II.13)

   With d = c cos A this is a^2 = b^2 + c^2 - 2 b c cos A.

   The tie (widget brief section 5): a, b, c and the angle A (degrees) are
   live controls, held together by that equation.

     b, c or A moves   the other two of (b, c, A) held, a re-solved
                       (forward: a = sqrt(b^2 + c^2 - 2 b c cos A))
     a moves           b and A held, c re-solved (inverse: c = b cos A +
                       sqrt(a^2 - b^2 sin^2 A), the root with the angle at
                       B not obtuse; a is held at the edge of that branch)
     B dragged         b held; c = |AB| and A = atan2 read off B, a
                       re-solved

   Every map is an Emmy function of numbers or symbols: raster compiles
   them to kernels (the widget's numbers), and `grades` checks them with
   symbols (alexandria.grade, :grade/proved)."
  (:require [alexandria.grade :as grade]
            [emmy.env :as e]
            [hive-dsl.result :as r]))

;; ---------------------------------------------------------------------------
;; Branch-free helpers (numbers and symbols alike; raster has no `if`)

(defn rad "Degrees to radians." [d] (e/* d (/ Math/PI 180)))
(defn deg "Radians to degrees." [x] (e/* x (/ 180 Math/PI)))

(defn at-least "max(x, lo), branch-free." [x lo] (e/* 0.5 (e/+ x lo (e/abs (e/- x lo)))))
(defn at-most "min(x, hi), branch-free." [x hi] (e/* 0.5 (e/- (e/+ x hi) (e/abs (e/- x hi)))))
(defn clamp "x held in [lo hi]." [x lo hi] (at-most (at-least x lo) hi))

;; ---------------------------------------------------------------------------
;; The relation, derived

(defn dist2
  "The square on the straight line PQ."
  [[px py] [qx qy]]
  (e/+ (e/square (e/- qx px)) (e/square (e/- qy py))))

(def derivation
  "Emmy's derivation, step by step, with B = (d, h): the squares from the
   coordinates, h eliminated through the square on AB (h^2 = c^2 - d^2),
   and the cosine form with B = (c cos A, c sin A)."
  (delay
    (let [A [0 0] C ['b 0] B ['d 'h]
          h2 (e/- (e/square 'c) (e/square 'd))
          a2-elim (e/simplify (e/+ (e/square (e/- 'b 'd)) h2))
          Bt [(e/* 'c (e/cos 'A)) (e/* 'c (e/sin 'A))]]
      {:a2 (e/simplify (dist2 B C))
       :c2 (e/simplify (dist2 A B))
       :h2 h2
       :a2-elim a2-elim
       :excess (e/simplify (e/- a2-elim (e/+ (e/square 'b) (e/square 'c))))
       :cosine (e/simplify (dist2 Bt C))})))

(defn- eq
  "lhs = rhs as one Emmy literal (->TeX renders it whole)."
  [lhs rhs]
  (e/literal-number (list '= (e/freeze lhs) (e/freeze rhs))))

(defn equations
  "[{:label :eqs}]: the derivation as Emmy equations, each right-hand side
   as Emmy computed it, for emmy.clerk/->TeX on the page."
  []
  (let [d @derivation]
    [{:label "B = (d, h), D its foot on AC, d = AD: the squares on BC and on AB from the coordinates"
      :eqs [(eq (list 'expt 'a 2) (:a2 d)) (eq (list 'expt 'c 2) (:c2 d))]}
     {:label "eliminate h (h² = c² − d²): the square on BC against the other two"
      :eqs [(eq (list 'expt 'a 2) (:a2-elim d))
            (eq (list '- (list 'expt 'a 2) (list '+ (list 'expt 'b 2) (list 'expt 'c 2))) (:excess d))]}
     {:label "d = 0: I.47; d < 0 (D outside, obtuse): II.12, greater by 2 CA·AD; d > 0 (acute): II.13, less by it. With d = c cos A:"
      :eqs [(eq (list 'expt 'a 2) (:cosine d))]}
     {:label "the tie solved each way (b and A held): forward and inverse"
      :eqs [(eq 'a (e/sqrt (e/- (e/+ (e/square 'b) (e/square 'c)) (e/* 2 'b 'c (e/cos 'A)))))
            (eq 'c (e/+ (e/* 'b (e/cos 'A)) (e/sqrt (e/- (e/square 'a) (e/square (e/* 'b (e/sin 'A)))))))]}]))

;; ---------------------------------------------------------------------------
;; Forward and inverse maps (A in degrees)

(defn side-a
  "BC from AC = b, AB = c and the angle A: the forward map."
  [b c A]
  (e/sqrt (e/- (e/+ (e/square b) (e/square c)) (e/* 2 b c (e/cos (rad A))))))

(defn side-c
  "AB from BC = a, AC = b and the angle A: the inverse map, the root with
   the angle at B not obtuse (c >= b cos A). The radicand is held at 0 or
   above: at the edge of the branch (a = b sin A) rounding would take it
   a hair below."
  [a b A]
  (e/+ (e/* b (e/cos (rad A)))
       (e/sqrt (at-least (e/- (e/square a) (e/square (e/* b (e/sin (rad A))))) 0))))

(defn from-point
  "[c A] of the vertex B = (x, y): c = |AB|, A = atan2(y, x) in degrees."
  [x y]
  [(e/sqrt (e/+ (e/square x) (e/square y))) (deg (e/atan y x))])

;; ---------------------------------------------------------------------------
;; The tie kernel

(def ranges
  "Each control's slider range [min max step]; the tie keeps every key in it."
  {:a [0.1 12 0.01] :b [0.5 6 0.01] :c [0.5 6 0.01] :A [20 160 0.5]})

(def tied [:a :b :c :A])

(defn- lo [k] (first (ranges k)))
(defn- hi [k] (second (ranges k)))

(defn- forward-block [b c A] [(side-a b c A) b c A])

(defn- from-a
  "a moved, b and A held: a held to the branch c in [max(lo c, b cos A),
   hi c], then c by the inverse map."
  [a b A]
  (let [c-lo (at-least (lo :c) (e/* b (e/cos (rad A))))
        a' (clamp a (side-a b c-lo A) (side-a b (hi :c) A))]
    [a' b (side-c a' b A) A]))

(defn- from-B
  "B dragged to (x, y), b held: c and A read off B and clamped to their
   ranges, a forward."
  [b x y]
  (let [[c A] (from-point x y)]
    (forward-block b (clamp c (lo :c) (hi :c)) (clamp A (lo :A) (hi :A)))))

(def source-block
  "Where each source's solved state [a b c A] starts in the tie's output."
  {:a 0 :b 4 :c 8 :A 12 :B 16})

(defn tie
  "The tie kernel: for the state [a b c A] and a dragged B = (x, y), the
   solved state for each source in the order of source-block (20 numbers).
   b, c and A each re-solve a forward (the others held); the page keeps
   the block of the key the reader touched."
  [a b c A x y]
  (fn [_]
    (vec (concat (from-a a b A) (forward-block b c A) (forward-block b c A) (forward-block b c A)
                 (from-B b x y)))))

;; ---------------------------------------------------------------------------
;; The figure kernel: every point and number the widget draws

(def points
  "The figure's points, in order, two numbers each (SVG coordinates, y
   down): the triangle, D, the squares' outer corners, the far corners of
   the rectangle CA.AD (on AD, CA deep, inside the square on AC when A is
   acute, beside it when obtuse), the letters' anchors."
  [:A :C :B :D :ac1 :ac2 :ab1 :ab2 :bc1 :bc2 :r1 :r2
   :l-A :l-B :l-C :l-D :l-a :l-b :l-c])

(def scalars
  "Then: a, a^2, b^2, c^2, AD, 2 CA.AD, b^2 + c^2, cos A, BD."
  [:a :a2 :b2 :c2 :ad :rect2 :sum :cos :bd])

(defn figure
  "The figure for AC = b, AB = c, the angle A (degrees), as a vector:
   `points` then `scalars`, in the plane with y up (Mafs). Squares stand
   outward on each side (A, C, B counterclockwise; the outward side of an
   edge P -> Q is to its right). The rectangle CA.AD stands on AD, CA
   deep, inside the square on AC. The side letters a, b, c sit at the
   centres of the squares on them."
  [b c A]
  (fn [_]
    (let [ca (e/cos (rad A)) sa (e/sin (rad A))
          bx (e/* c ca) by (e/* c sa)
          out (fn [[px py] [qx qy]]
                (let [vx (e/- qx px) vy (e/- qy py)]
                  [[(e/+ qx vy) (e/- qy vx)] [(e/+ px vy) (e/- py vx)]]))
          mid (fn [[px py] [qx qy]] [(e/* 0.5 (e/+ px qx)) (e/* 0.5 (e/+ py qy))])
          A* [0 0] C* [b 0] B* [bx by]
          [ac1 ac2] (out A* C*) [bc1 bc2] (out C* B*) [ab1 ab2] (out B* A*)
          ad bx
          a2 (e/- (e/+ (e/square b) (e/square c)) (e/* 2 b ad))
          pts {:A A* :C C* :B B* :D [ad 0]
               :ac1 ac1 :ac2 ac2 :ab1 ab1 :ab2 ab2 :bc1 bc1 :bc2 bc2
               :r1 [ad (e/negate b)] :r2 [0 (e/negate b)]
               :l-A [-0.35 0.25] :l-C [(e/+ b 0.35) 0.25] :l-B [bx (e/+ by 0.4)]
               :l-D [ad 0.3]
               :l-a (mid C* bc1) :l-b (mid A* ac1) :l-c (mid B* ab1)}]
      (vec (concat (mapcat pts points)
                   [(e/sqrt a2) a2 (e/square b) (e/square c) ad (e/* 2 b ad)
                    (e/+ (e/square b) (e/square c)) ca by])))))

(defn named
  "{name value} of one output row of `figure`."
  [row]
  (let [n (count points)]
    (merge (zipmap points (map vec (partition 2 (take (* 2 n) row))))
           (zipmap scalars (drop (* 2 n) row)))))

(def right-band
  "|cos A| below this names the angle right (Leva's 0.5 degree step lands
   on 90 exactly; a dragged B within about a quarter degree)."
  0.004)

(defn kind
  "The proposition the triangle stands under, from cos A."
  [cos-a]
  (cond (< (Math/abs (double cos-a)) right-band) :right
        (pos? cos-a) :acute
        :else :obtuse))

(def names
  "What each case says, in Heath's words (1908, vol. 1, pp. 349, 403, 406)."
  {:right {:prop "I.47" :name "right-angled"
           :says "In right-angled triangles the square on the side subtending the right angle is equal to the squares on the sides containing the right angle."}
   :obtuse {:prop "II.12" :name "obtuse-angled"
            :says "In obtuse-angled triangles the square on the side subtending the obtuse angle is greater than the squares on the sides containing the obtuse angle by twice the rectangle contained by one of the sides about the obtuse angle, namely that on which the perpendicular falls, and the straight line cut off outside by the perpendicular towards the obtuse angle."}
   :acute {:prop "II.13" :name "acute-angled"
           :says "In acute-angled triangles the square on the side subtending the acute angle is less than the squares on the sides containing the acute angle by twice the rectangle contained by one of the sides about the acute angle, namely that on which the perpendicular falls, and the straight line cut off within by the perpendicular towards the acute angle."}})

(def start
  "Euclid's triangle at rest: the right angle at A, AC = 4, AB = 3."
  {:b 4 :c 3 :A 90})

;; ---------------------------------------------------------------------------
;; Grades

(defn- variant [res] (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails))

(defn- proved [id label diff]
  {:id id :label label :grade (variant (grade/grade :symbolic [diff]))})

(defn grades
  "[{:id :label :grade}]: each step of the derivation and each round trip
   of the tie, graded by alexandria.grade (Emmy simplifies the difference
   to exact 0: :grade/proved). The angle is a symbol in radians here; the
   kernels take degrees through `rad`."
  []
  (let [d @derivation
        fwd (fn [b c t] (e/sqrt (e/- (e/+ (e/square b) (e/square c)) (e/* 2 b c (e/cos t)))))
        inv (fn [a b t] (e/+ (e/* b (e/cos t)) (e/sqrt (e/- (e/square a) (e/square (e/* b (e/sin t)))))))
        ;; the inverse's branch: c = b cos A + s, s >= 0 (the angle at B not obtuse)
        c-branch (e/+ (e/* 'b (e/cos 'A)) 's)
        bx (e/* 'c (e/cos 'A)) by (e/* 'c (e/sin 'A))]
    [(proved :II.12-13 "h eliminated: BC² = AC² + AB² − 2 CA·AD"
             (e/- (:a2-elim d) (e/- (e/+ (e/square 'b) (e/square 'c)) (e/* 2 'b 'd))))
     (proved :I.47 "D on A (AD = 0, the angle at A right): BC² = AC² + AB²"
             (e/- (dist2 [0 'h] ['b 0]) (e/+ (e/square 'b) (dist2 [0 0] [0 'h]))))
     (proved :cosine "AD = AB cos A: BC² = b² + c² − 2bc cos A"
             (e/- (:cosine d) (e/- (e/+ (e/square 'b) (e/square 'c)) (e/* 2 'b 'c (e/cos 'A)))))
     (proved :inverse "the inverse c = b cos A + √(a² − b² sin² A) solves the relation"
             (e/- (e/square (fwd 'b (inv 'a 'b 'A) 'A)) (e/square 'a)))
     (proved :round-trip-c "c → a → c (forward then inverse) is c, for c ≥ b cos A"
             (e/- (inv (fwd 'b c-branch 'A) 'b 'A) c-branch))
     (proved :round-trip-a "a → c → a (inverse then forward) is a"
             (e/- (fwd 'b (inv 'a 'b 'A) 'A) 'a))
     (proved :drag-c "B dragged, read back: |AB| is c"
             (e/- (e/sqrt (e/+ (e/square bx) (e/square by))) 'c))
     (proved :drag-A "B dragged, read back: atan2(B) is the angle A"
             (e/- (e/atan by bx) 'A))]))
