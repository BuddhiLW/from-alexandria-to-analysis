(ns alexandria.delian.tie
  "The ties of the doubling of the cube, derived by Emmy and graded
   (widget brief, section 5): every quantity linked by an equation is a
   control, and the forward and inverse maps below are what the widgets
   compile to raster kernels.

     menaechmus  the parabolas x^2 = a y and y^2 = 2a x. Emmy solves the
                 first for y (linear), puts it into the second, clears a^2,
                 and divides the polynomial by x (the common vertex,
                 remainder 0): x^3 - 2a^3 is left, so the cube on x is the
                 doubled cube V = 2a^3 and x = a 2^(1/3).
                 Tie {a x V}: move any one, the other two follow.
     archytas    AD fixed, AB the chord: AI^3 = AD AB^2, AK^2 = AD AI.
                 Tie {AB AI AK}.
     mesolabe    the first frame's edge a fixed, the slide s, the edge
                 reached b = a s^3, the means a s and a s^2. Tie {s b}.

   `grades` checks each derivation step and every forward-then-inverse and
   inverse-then-forward composition symbolically (:grade/proved)."
  (:require [alexandria.grade :as grade]
            [emmy.env :as e]
            [emmy.polynomial :as poly]
            [hive-dsl.result :as r]))

;; ---------------------------------------------------------------------------
;; Menaechmus: x^2 = a y, y^2 = 2a x

(defn parabola-1 "x^2 = a y, as zero = x^2 - a y." [a x y] (e/- (e/square x) (e/* a y)))
(defn parabola-2 "y^2 = 2a x, as zero = y^2 - 2a x." [a x y] (e/- (e/square y) (e/* 2 a x)))

(defn y-of-x
  "y from the first parabola, linear in y: emmy.generic/solve-linear
   (a y = x^2)."
  [a x]
  (e/solve-linear a (e/square x)))

(defn divide-by
  "Emmy's polynomial division of expr by the variable v: [quotient
   remainder] as simplified Emmy expressions."
  [expr v]
  (let [lit (fn [x] (let [s (e/simplify x)] (if (number? s) s (e/literal-number s))))]
    (poly/expression->
     (e/freeze (e/simplify expr))
     (fn [p vars]
       (let [n (count vars)
             vp (poly/make n [[(assoc (vec (repeat n 0)) (.indexOf ^java.util.List vars v) 1) 1]])
             [q rm] (poly/divide p vp)]
         [(lit (poly/->expression q vars)) (lit (poly/->expression rm vars))])))))

(def derivation
  "Every step as Emmy expressions."
  (delay
    (let [y (y-of-x 'a 'x)
          eliminated (e/simplify (e/* (e/square 'a) (parabola-2 'a 'x y)))
          [cubic remainder] (divide-by eliminated 'x)]
      {:y y :eliminated eliminated :cubic cubic :remainder remainder
       :x (e/* 'a (e/expt 2 (e// 1 3)))
       :V (e/* 2 (e/cube 'a))})))

;; forward / inverse maps (numbers or symbols; raster compiles them)

(def cbrt2 "2^(1/3) as an Emmy expression." (e/expt 2 (e// 1 3)))

(defn cbrt
  "The cube root of a positive quantity, as exp(log(q)/3): raster compiles
   it (a variable to a fractional power it does not), and Emmy simplifies
   it on positive symbols."
  [q] (e/exp (e// (e/log q) 3)))

(defn x-of-a "x = a 2^(1/3)." [a] (e/* a cbrt2))
(defn a-of-x "a = x / 2^(1/3)." [x] (e// x cbrt2))
(defn v-of-a "V = 2 a^3." [a] (e/* 2 (e/cube a)))
(defn a-of-v "a = (V/2)^(1/3)." [v] (cbrt (e// v 2)))
(defn v-of-x "V = x^3." [x] (e/cube x))
(defn x-of-v "x = V^(1/3)." [v] (cbrt v))

;; ---------------------------------------------------------------------------
;; Archytas: AD = d fixed, AB = b, AI = m, AK = k

(defn ai-of-ab "AI = (AD AB^2)^(1/3)." [d b] (cbrt (e/* d (e/square b))))
(defn ab-of-ai "AB = (AI^3 / AD)^(1/2)." [d m] (e/sqrt (e// (e/cube m) d)))
(defn ak-of-ai "AK = (AD AI)^(1/2) (the ring: AK^2 = AD AI)." [d m] (e/sqrt (e/* d m)))
(defn ai-of-ak "AI = AK^2 / AD." [d k] (e// (e/square k) d))

;; ---------------------------------------------------------------------------
;; Mesolabe: b = a s^3

(defn b-of-s "b = a s^3." [a s] (e/* a (e/cube s)))
(defn s-of-b "s = (b/a)^(1/3)." [a b] (cbrt (e// b a)))

(defn mean-1 "The first mean a s (the height BZ)." [a s] (e/* a s))
(defn mean-2 "The second mean a s^2 (the height GH)." [a s] (e/* a (e/square s)))
(defn s-of-mean-1 "s = x / a." [a x] (e// x a))
(defn s-of-mean-2 "s = (y / a)^(1/2)." [a y] (e/sqrt (e// y a)))

;; ---------------------------------------------------------------------------
;; Grades

(defn- variant [res] (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails))

(defn- proved [label diff] {:label label :grade (variant (grade/grade :symbolic [diff]))})

(defn- pos
  "A positive length: the lengths are positive, so (s^3)^(1/3) = s. Emmy
   takes roots of powers only where the base is known positive, so the
   round trips are checked on s = exp(u) for a free u."
  [u] (e/exp u))

(defn grades
  "{:menaechmus :archytas :mesolabe} -> [{:label :grade}]."
  []
  (let [d @derivation
        a (pos 'u) x (pos 'v) v (pos 'w) b (pos 'p) m (pos 'q) k (pos 'r)
        dd (pos 'c) s (pos 'z)]
    {:menaechmus
     [(proved "y from x² = a y (solve-linear) lies on the first parabola" (parabola-1 'a 'x (:y d)))
      (proved "y eliminated: a²(y² − 2ax) = x⁴ − 2a³x"
              (e/- (:eliminated d) (e/- (e/expt 'x 4) (e/* 2 (e/cube 'a) 'x))))
      (proved "divided by x (the common vertex): remainder 0" (:remainder d))
      (proved "the quotient is x³ − 2a³: x³ = 2a³ = V" (e/- (:cubic d) (e/- (e/cube 'x) (e/* 2 (e/cube 'a)))))
      (proved "x = a∛2 is its root" (e/- (e/cube (:x d)) (:V d)))
      (proved "a → x → a is the identity" (e/- (a-of-x (x-of-a a)) a))
      (proved "x → a → x is the identity" (e/- (x-of-a (a-of-x x)) x))
      (proved "a → V → a is the identity" (e/- (a-of-v (v-of-a a)) a))
      (proved "V → a → V is the identity" (e/- (v-of-a (a-of-v v)) v))
      (proved "x → V → x is the identity" (e/- (x-of-v (v-of-x x)) x))
      (proved "V → x → V is the identity" (e/- (v-of-x (x-of-v v)) v))
      (proved "the ties agree: V(a) = x(a)³" (e/- (v-of-a a) (e/cube (x-of-a a))))]
     :archytas
     [(proved "AB → AI → AB is the identity" (e/- (ab-of-ai dd (ai-of-ab dd b)) b))
      (proved "AI → AB → AI is the identity" (e/- (ai-of-ab dd (ab-of-ai dd m)) m))
      (proved "AI → AK → AI is the identity" (e/- (ai-of-ak dd (ak-of-ai dd m)) m))
      (proved "AK → AI → AK is the identity" (e/- (ak-of-ai dd (ai-of-ak dd k)) k))
      (proved "AB : AI = AI : AK" (e/- (e/* (ab-of-ai dd m) (ak-of-ai dd m)) (e/square m)))
      (proved "AI : AK = AK : AD" (e/- (e/* m dd) (e/square (ak-of-ai dd m))))]
     :mesolabe
     [(proved "s → b → s is the identity" (e/- (s-of-b dd (b-of-s dd s)) s))
      (proved "b → s → b is the identity" (e/- (b-of-s dd (s-of-b dd b)) b))
      (proved "s → as → s is the identity" (e/- (s-of-mean-1 dd (mean-1 dd s)) s))
      (proved "as → s → as is the identity" (e/- (mean-1 dd (s-of-mean-1 dd x)) x))
      (proved "s → as² → s is the identity" (e/- (s-of-mean-2 dd (mean-2 dd s)) s))
      (proved "as² → s → as² is the identity" (e/- (mean-2 dd (s-of-mean-2 dd v)) v))
      (proved "a : as = as : as² = as² : b"
              (e/+ (e/- (e/* dd (e/* dd (e/square s))) (e/square (e/* dd s)))
                   (e/- (e/* (e/* dd s) (b-of-s dd s)) (e/square (e/* dd (e/square s))))))]}))

;; ---------------------------------------------------------------------------
;; TeX for the page

(defn- eq [lhs rhs] (e/literal-number (list '= (e/freeze lhs) (e/freeze rhs))))

(defn equations
  "[{:label :eqs}]: Menaechmus' derivation as Emmy equations, each
   right-hand side as Emmy derived it."
  []
  (let [d @derivation]
    [{:label "the two parabolas (vertex O; Heath's form of Menaechmus)"
      :eqs [(eq (e/square 'x) (e/* 'a 'y)) (eq (e/square 'y) (e/* 2 'a 'x))]}
     {:label "the first is linear in y: solve-linear"
      :eqs [(eq 'y (:y d))]}
     {:label "put y into the second and clear a²: a polynomial in x"
      :eqs [(eq (list '* (list 'expt 'a 2) (list '- (list 'expt 'y 2) (list '* 2 'a 'x))) (:eliminated d))]}
     {:label "divide by x, the common vertex (polynomial division; remainder 0)"
      :eqs [(eq (list '/ (e/freeze (:eliminated d)) 'x) (:cubic d))]}
     {:label "so the cube on x is the doubled cube"
      :eqs [(eq (e/cube 'x) (:V d)) (eq 'x (:x d))]}]))
