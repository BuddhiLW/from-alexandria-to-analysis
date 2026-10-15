(ns alexandria.algebra.cubic
  "From the square to the cube: al-Khwarizmi (c. 820), Cardano's Ars magna
   (1545, ch. XI), Bombelli's L'Algebra (1572) and Viete's roots and
   coefficients. The first shelf of the algebra strand.

     al-Khwarizmi   a square and b roots equal c:  x^2 + b x = c.
                    Complete the square: add (b/2)^2 to both sides, then
                    x = sqrt(c + (b/2)^2) - b/2  (his 'ten roots, thirty-nine':
                    b = 10, c = 39, x = 3).
     Cardano XI     the cube and things equal to a number: x^3 + p x = q.
                    Two cubes u^3, v^3 with u^3 - v^3 = q and u v = p/3; then
                    x = u - v. u^3 and -v^3 are q/2 +- sqrt(D),
                    D = (q/2)^2 + (p/3)^3.
     Bombelli       D < 0 (casus irreducibilis, three real roots): the cube
                    roots are complex conjugates and their sum is real;
                    x^3 = 15 x + 4 has 2 + 11i = (2 + i)^3, x = 4.
     Viete          (x - r1)(x - r2)(x - r3) = x^3 - e1 x^2 + e2 x - e3,
                    e1, e2, e3 the symmetric functions of the roots.

   Every function here is an Emmy function of numbers or symbols, branch
   free, so alexandria.raster compiles it into the kernels the widgets run
   (alexandria.algebra.cubic-widgets) and the page reads its numbers from.

     square-readout  khwarizmi-tie   the completing of the square, tied
     cardano         cardano-tie     Cardano's rule for any p, q, both cases
     viete                           roots -> coefficients
     derivations     graded          Emmy's derivations (->TeX) and grades"
  (:require [alexandria.grade :as grade]
            [alexandria.raster :as raster]
            [emmy.env :as e]
            [hive-dsl.result :as r]))

;; ---------------------------------------------------------------------------
;; Branch-free helpers (numbers and symbols alike)

(def tiny "Guard for a zero denominator or logarithm." 1e-300)

(defn at-least [x lo] (e/* 0.5 (e/+ x lo (e/abs (e/- x lo)))))

(defn cbrt
  "The real cube root of s, branch free: sign(s) |s|^(1/3)."
  [s]
  (let [a (e/+ (e/abs s) tiny)]
    (e/* (e/divide s a) (e/exp (e/divide (e/log a) 3)))))

(defn positive-part
  "1 where d > 0, 0 where d < 0 (and at d = 0)."
  [d]
  (e/divide (e/+ d (e/abs d)) (e/+ (e/* 2 (e/abs d)) tiny)))

;; ---------------------------------------------------------------------------
;; al-Khwarizmi: x^2 + b x = c

(defn square-root-of
  "x = sqrt(c + (b/2)^2) - b/2, the root of a square and b roots equal c."
  [b c]
  (e/- (e/sqrt (e/+ c (e/square (e/divide b 2)))) (e/divide b 2)))

(defn number-of
  "c = x^2 + b x: the number a square of root x and b roots make (inverse)."
  [b x]
  (e/+ (e/square x) (e/* b x)))

(def square-index
  "Where each quantity sits in the output of `square-readout`."
  {:half 0 :corner 1 :great 2 :side 3 :x 4 :square 5 :roots 6 :c 7 :b 8})

(defn square-readout
  "The numbers of the figure for b roots and the number c: [b/2, (b/2)^2
   (the corner square), c + (b/2)^2 (the great square), its side, x, x^2
   (the square AB), b x (the ten roots, the two rectangles), x^2 + b x
   (back to c), b]."
  [b c]
  (fn [_]
    (let [h (e/divide b 2) great (e/+ c (e/square h)) side (e/sqrt great) x (e/- side h)]
      [h (e/square h) great side x (e/square x) (e/* b x) (e/+ (e/square x) (e/* b x)) b])))

(def square-keys [:b :c :x])

(def square-blocks "Where each source's state starts in `khwarizmi-tie`." {:b 0 :c 3 :x 6})

(defn khwarizmi-tie
  "The tie for {:b :c :x}: b or c moved (the other held), x forward; x
   moved (b held), c by the inverse map. The source is held at the edge
   where c would fall below 0. 9 numbers, one block of [b c x] per source."
  [b c x]
  (fn [_]
    (let [cc (at-least c 0) xx (at-least x 0)]
      [b cc (square-root-of b cc)
       b cc (square-root-of b cc)
       b (number-of b xx) xx])))

;; ---------------------------------------------------------------------------
;; Cardano: x^3 + p x = q

(def cardano-index
  "Where each quantity sits in the output of `cardano`."
  {:h 0 :k 1 :D 2 :sqrtD 3 :A 4 :B 5 :real? 6
   :t1-re 7 :t1-im 8 :t2-re 9 :t2-im 10 :x 11 :r 12 :phi 13 :cube-im 14 :u 15 :v 16})

(defn cardano
  "Cardano's rule for x^3 + p x = q (any real p, q), branch free.

   h = q/2, k = p/3, D = h^2 + k^3.
   D > 0: A = sqrt(D) + h, B = sqrt(D) - h are Cardano's binomium and
     apotome (A B = k^3, A - B = q), u = cbrt A, v = cbrt B, x = u - v.
   D < 0: A and -B are the complex conjugates h +- i sqrt(-D) (Bombelli);
     with r = |h + i sqrt(-D)| and phi its angle, their cube roots are
     r^(1/3) e^(+- i phi/3) and x = 2 r^(1/3) cos(phi/3).

   Output (see cardano-index): the two terms of x as complex numbers
   (t1 = cbrt A, t2 = -cbrt B, conjugate when D < 0, real when D > 0), x,
   r, phi, Im(A) = sqrt(-D) (0 when D > 0), and u = cbrt A, v = cbrt B
   (real case). The other roots and the check are `rest-roots` (a second
   kernel, so neither kernel outgrows the JVM's method size)."
  [p q]
  (fn [_]
    (let [h (e/divide q 2) k (e/divide p 3)
          D (e/+ (e/square h) (e/cube k))
          s (positive-part D)
          sd (e/sqrt (at-least D 0))
          m (e/sqrt (at-least (e/- 0 D) 0))
          A (e/+ sd h) B (e/- sd h)
          u (cbrt A) v (cbrt B)
          r (e/sqrt (e/+ (e/square h) (e/square m)))
          phi (e/atan m h)
          rho (cbrt r)
          cre (e/* rho (e/cos (e/divide phi 3)))
          cim (e/* rho (e/sin (e/divide phi 3)))
          t1re (e/+ (e/* s u) (e/* (e/- 1 s) cre))
          t1im (e/* (e/- 1 s) cim)
          t2re (e/+ (e/* s (e/- 0 v)) (e/* (e/- 1 s) cre))]
      [h k D (e/sqrt (e/abs D)) A B s t1re t1im t2re (e/- 0 t1im) (e/+ t1re t2re)
       r phi m u v])))

(defn cardano-x
  "x of Cardano's rule for (p, q) as one Emmy expression (the forward map)."
  [p q]
  (nth ((cardano p q) nil) (cardano-index :x)))

(def rest-index "Where each quantity sits in the output of `rest-roots`." {:d2 0 :re 1 :im 2 :check 3})

(defn rest-roots
  "For x a root of x^3 + p x = q: the other two roots -x/2 +- sqrt(d2),
   d2 = -3x^2/4 - p (real when d2 >= 0, else -x/2 +- i sqrt(-d2)), and the
   check x^3 + p x - q. [d2 -x/2 sqrt|d2| check]."
  [p q x]
  (fn [_]
    (let [d2 (e/- (e/* -0.75 (e/square x)) p)]
      [d2 (e/divide x -2) (e/sqrt (e/abs d2)) (e/- (e/+ (e/cube x) (e/* p x)) q)])))

(defn number-of-cube
  "q = x^3 + p x (the inverse map: the number a cube and p things make)."
  [p x]
  (e/+ (e/cube x) (e/* p x)))

(def cardano-keys [:p :q :x])

(def cardano-blocks "Where each source's state starts in `cardano-tie`." {:p 0 :q 3 :x 6})

(defn cardano-tie
  "The tie for {:p :q :x}: p or q moved (the other held), x by Cardano's
   rule (forward); x moved (p held), q = x^3 + p x (inverse). 9 numbers."
  [p q x]
  (fn [_]
    (let [xf (cardano-x p q)]
      [p q xf p q xf p (number-of-cube p x) x])))

(def readout-index
  "Where each quantity sits in the output of `cardano-readout`: cardano's
   (0..16), rest-roots' (17..20), then the other two cube roots of A,
   t1 rotated by 120 and 240 degrees (21..24)."
  (merge cardano-index
         (into {} (map (fn [[k i]] [(keyword (str "rest-" (name k))) (+ 17 i)]) rest-index))
         {:w1-re 21 :w1-im 22 :w2-re 23 :w2-im 24}))

(defn cardano-readout
  "Everything the Cardano widget shows for (p, q), one kernel: the rule,
   the other two roots, and the three cube roots of A = q/2 + i sqrt(-D)
   (D < 0; each pair of conjugates sums to one of the three real roots)."
  [p q]
  (fn [_]
    (let [c ((cardano p q) nil)
          rr ((rest-roots p q (nth c (cardano-index :x))) nil)
          rho (cbrt (nth c (cardano-index :r)))
          th (e/divide (nth c (cardano-index :phi)) 3)
          w (fn [k] [(e/* rho (e/cos (e/+ th (* k 2/3 Math/PI)))) (e/* rho (e/sin (e/+ th (* k 2/3 Math/PI))))])]
      (vec (concat c rr (w 1) (w 2))))))

(def cardano-ranges
  "Slider ranges of the Cardano widget, [min max step]. p >= -15 keeps the
   local maximum 2(-p/3)^(3/2) of x^3 + p x inside q's range, so x moved
   with p held stays between the roots of x^3 + p x = +-q-max."
  {:p [-15 20 0.1] :q [-30 30 0.1] :x [-5 5 0.01]})

(defn cardano-sources
  "The tie's policies for {:p :q :x} (alexandria.medium.tied): p or q moved,
   the other held, x by Cardano's rule (forward); x moved, p held, x held
   between the largest roots of x^3 + p x = -30 and = 30, then q = x^3 + p x
   (inverse)."
  []
  (let [[qlo qhi] (cardano-ranges :q)
        fwd (fn [p q _x] [p q (cardano-x p q)])]
    {:p fwd :q fwd
     :x (fn [p _q x]
          (let [lo (cardano-x p qlo) hi (cardano-x p qhi)
                xx (e/* 0.5 (e/- (e/+ (at-least x lo) hi) (e/abs (e/- (at-least x lo) hi))))]
            [p (number-of-cube p xx) xx]))}))

(def square-ranges "Slider ranges of the al-Khwarizmi widget." {:b [0 20 0.5] :c [0 100 0.5] :x [0 10 0.01]})

(defn square-sources
  "The tie's policies for {:b :c :x}: b or c moved, x forward; x moved, b
   held, x kept where c stays at most c-max, then c by the inverse map."
  []
  (let [cmax (second (square-ranges :c))
        fwd (fn [b c _x] [b c (square-root-of b c)])]
    {:b fwd :c fwd
     :x (fn [b _c x]
          (let [hi (square-root-of b cmax)
                xx (e/* 0.5 (e/- (e/+ (at-least x 0) hi) (e/abs (e/- (at-least x 0) hi))))]
            [b (number-of b xx) xx]))}))

;; ---------------------------------------------------------------------------
;; Viete: roots and coefficients

(defn elementary
  "[e1 e2 e3] of the roots r1 r2 r3."
  [r1 r2 r3]
  [(e/+ r1 r2 r3) (e/+ (e/* r1 r2) (e/* r1 r3) (e/* r2 r3)) (e/* r1 r2 r3)])

(defn viete
  "The kernel of the roots widget: [e1 e2 e3, then the depressed form of
   the same cubic by x = y + e1/3: p q (y^3 + p y = q), D]."
  [r1 r2 r3]
  (fn [_]
    (let [[e1 e2 e3] (elementary r1 r2 r3)
          a (e/- 0 e1) b e2 c (e/- 0 e3)
          p (e/- b (e/divide (e/square a) 3))
          q (e/- 0 (e/+ (e/divide (e/* 2 (e/cube a)) 27) (e/divide (e/* -1 a b) 3) c))]
      [e1 e2 e3 p q (e/+ (e/square (e/divide q 2)) (e/cube (e/divide p 3)))])))

(defn cubic-of-roots
  "y = (x - r1)(x - r2)(x - r3), for plotting."
  [r1 r2 r3]
  (fn [x] (e/* (e/- x r1) (e/- x r2) (e/- x r3))))

;; ---------------------------------------------------------------------------
;; Derivations, for the page (->TeX)

(defn derivations
  "Emmy's derivations as [{:label :eqs}] (each eq an expression = 0 or a
   value), symbolic in b c x, p q u v, r1 r2 r3."
  []
  (let [[e1 e2 e3] (elementary 'r_1 'r_2 'r_3)]
    {:square
     [{:label "a square and b roots equal c, then (b/2)² added to both sides:"
       :eqs [(e/+ (e/square 'x) (e/* 'b 'x)) (e/square (e/+ 'x (e/divide 'b 2)))]}
      {:label "so the side of the great square is √(c + (b/2)²), and the root (forward map):"
       :eqs [(square-root-of 'b 'c)]}
      {:label "the inverse map, c from the root:" :eqs [(number-of 'b 'x)]}
      {:label "forward then inverse, c(x(b, c)), simplified by Emmy:"
       :eqs [(e/simplify (number-of 'b (square-root-of 'b 'c)))]}]
     :cardano
     [{:label "Cardano's cube of side u = x + v, cut into x³, v³ and the solids 3x²v, 3xv²:"
       :eqs [(e/- (e/cube (e/+ 'x 'v)) (e/+ (e/cube 'x) (e/cube 'v) (e/* 3 (e/square 'x) 'v) (e/* 3 'x (e/square 'v))))]}
      {:label "with x = u − v and uv = p/3, x³ + px − (u³ − v³), simplified:"
       :eqs [(e/simplify (e/- (number-of-cube (e/* 3 'u 'v) (e/- 'u 'v)) (e/- (e/cube 'u) (e/cube 'v))))]}
      {:label "so u³ and −v³ have sum q and product −(p/3)³: the roots of t² − qt − (p/3)³ = 0,"
       :eqs [(e/+ (e/divide 'q 2) (e/sqrt (e/+ (e/square (e/divide 'q 2)) (e/cube (e/divide 'p 3)))))]}]
     :bombelli
     [{:label "(2 + i)³, real and imaginary parts (a + ib)³ = a³ − 3ab² + i(3a²b − b³) at a = 2, b = 1:"
       :eqs [(e/- (e/cube 2) (e/* 3 2 1)) (e/- (e/* 3 (e/square 2) 1) 1)]}]
     :viete
     [{:label "(x − r₁)(x − r₂)(x − r₃), expanded by Emmy:"
       :eqs [(e/simplify ((cubic-of-roots 'r_1 'r_2 'r_3) 'x))]}
      {:label "the coefficients, the symmetric functions e₁, e₂, e₃:" :eqs [e1 e2 e3]}]}))

;; ---------------------------------------------------------------------------
;; Grades

(defn- variant [kind diffs tol]
  (let [res (grade/grade kind diffs tol)]
    (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails)))

(def grid-pq
  "(p, q) on which the numeric checks run: Cardano's own (6, 20) and (6,
   ... ), the irreducible x^3 = 15 x + 4, and a spread of both cases."
  (vec (concat [[6 20] [-15 4] [3 10] [-6 4] [-3 2.5]]
               (for [p (range -12 13 4) q (range -15 16 5)] [p q]))))

(defn graded
  "[{:label :grade}] of the whole shelf. The raster kernels are built
   unsimplified (branch-free sums of abs)."
  []
  (binding [raster/*simplify?* false]
    (let [cx (cardano-index :x)
          fwd (raster/sample (fn [p q] (cardano-x p q)) grid-pq)
          roots (mapv (fn [[p q] x] (- (+ (* x x x) (* p x)) q)) grid-pq fwd)
          pxs (for [p [0.5 3 6 9] x [-3 -1 0.5 2 4]] [p x])
          inv (raster/sample (fn [p q] (cardano-x p q))
                             (mapv (fn [[p _] q] [p q]) pxs (raster/sample (fn [p x] (number-of-cube p x)) pxs)))
          inv-d (map (fn [[_ x] y] (- y x)) pxs inv)
          b4 (raster/value (fn [p q] ((cardano p q) nil)) -15 4)
          sq (raster/sample (fn [b c] (square-root-of b c)) [[10 39] [6 16] [2 3] [4 0.5]])]
      [{:label "(x + b/2)² = x² + bx + (b/2)²: the corner square completes the figure"
        :grade (variant :symbolic [(e/- (e/square (e/+ 'x (e/divide 'b 2)))
                                        (e/+ (e/square 'x) (e/* 'b 'x) (e/square (e/divide 'b 2))))]
                        grade/default-tolerance)}
       {:label "forward then inverse is the identity: c(x(b, c)) = c"
        :grade (variant :symbolic [(e/- (number-of 'b (square-root-of 'b 'c)) 'c)] grade/default-tolerance)}
       {:label "al-Khwarizmi's numbers on raster: ten roots and thirty-nine give the root three (and three more cases)"
        :grade (variant :numeric (map - sq [3 2 1 (- (Math/sqrt 4.5) 2)]) 1e-12)}
       {:label "Cardano's solids: (x + v)³ = x³ + v³ + 3x²v + 3xv²"
        :grade (variant :symbolic [(e/- (e/cube (e/+ 'x 'v))
                                        (e/+ (e/cube 'x) (e/cube 'v) (e/* 3 (e/square 'x) 'v) (e/* 3 'x (e/square 'v))))]
                        grade/default-tolerance)}
       {:label "x = u − v solves x³ + px = q when uv = p/3 and u³ − v³ = q"
        :grade (variant :symbolic [(e/- (number-of-cube (e/* 3 'u 'v) (e/- 'u 'v)) (e/- (e/cube 'u) (e/cube 'v)))]
                        grade/default-tolerance)}
       {:label "binomium times apotome: (√D + q/2)(√D − q/2) = (p/3)³"
        :grade (variant :symbolic [(let [D (e/+ (e/square (e/divide 'q 2)) (e/cube (e/divide 'p 3)))]
                                     (e/- (e/* (e/+ (e/sqrt D) (e/divide 'q 2)) (e/- (e/sqrt D) (e/divide 'q 2)))
                                          (e/cube (e/divide 'p 3))))]
                        grade/default-tolerance)}
       {:label (str "forward then inverse on raster: x(p, q)³ + p·x(p, q) = q on " (count grid-pq) " (p, q), both cases")
        :grade (variant :numeric roots 1e-9)}
       {:label "inverse then forward on raster: x(p, x³ + px) = x for p > 0 (one real root)"
        :grade (variant :numeric inv-d 1e-9)}
       {:label "Bombelli: (2 + i)³ = 2 + 11i, so x³ = 15x + 4 gives (2 + i) + (2 − i) = 4"
        :grade (variant :symbolic [(e/- (e/- (e/cube 2) (e/* 3 2 1)) 2) (e/- (e/- (e/* 3 (e/square 2) 1) 1) 11)]
                        grade/default-tolerance)}
       {:label "Bombelli on raster: the cube roots 2 ± i, their sum 4, and 4³ − 15·4 − 4 = 0"
        :grade (variant :numeric [(- (nth b4 (cardano-index :t1-re)) 2) (- (nth b4 (cardano-index :t1-im)) 1)
                                  (- (nth b4 (cardano-index :t2-im)) -1) (- (nth b4 cx) 4)
                                  (- (* 4 4 4) (* 15 (nth b4 cx)) 4)]
                        1e-9)}
       {:label "Viète: (x − r₁)(x − r₂)(x − r₃) = x³ − e₁x² + e₂x − e₃"
        :grade (variant :symbolic [(let [[e1 e2 e3] (elementary 'r1 'r2 'r3)]
                                     (e/- ((cubic-of-roots 'r1 'r2 'r3) 'x)
                                          (e/+ (e/cube 'x) (e/* -1 e1 (e/square 'x)) (e/* e2 'x) (e/- 0 e3))))]
                        grade/default-tolerance)}])))
