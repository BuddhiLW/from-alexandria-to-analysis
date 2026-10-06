(ns alexandria.fermat.maxima
  "Fermat, Methodus ad disquirendam maximam et minimam (c. 1636; Oeuvres,
   ed. Tannery and Henry, vol. I, 1891, pp. 133-136): adequality.

     adequate   Fermat's rule as an operation on an Emmy function f: compare
                f(a + e) with f(a), take away what is common, divide by e,
                strike out e. The result is what Leibniz (1684) writes
                df/da; `adequality-is-derivative` proves it for any f
                given as a polynomial in a
     rectangle  his example: cut the line B at A so that A (B - A) is
                greatest; adequality gives B = 2A
     tangent    his parabola BDN: the subtangent CE is twice CD

   Reuse searched: Emmy's D (derivative) and simplify; emmy.polynomial
   carries the division by e. Nothing in emmy, desargues or raster names
   Fermat's procedure, so `adequate` composes those operations here. A
   symbolic e is struck out by substituting 0 into the quotient after
   simplification (`strike-e`), which is Fermat's step and not a limit."
  (:require [alexandria.grade :as grade]
            [alexandria.proofs :as proofs]
            [clojure.walk :as walk]
            [emmy.env :as e]
            [hive-dsl.result :as r]
            [emmy.abstract.number :as an]))

(defn- strike-e
  "Fermat's last step: in the simplified quotient, put e = 0. The frozen
   expression is read back as an Emmy literal number so it composes."
  [expr]
  (let [frozen (walk/postwalk-replace {'e 0} (e/freeze (e/simplify expr)))]
    (e/simplify (if (seq? frozen) (an/literal-number frozen) frozen))))

(defn adequate
  "Fermat's steps on f at a, as a map:
     :compared   f(a + e) - f(a)        (the two 'adequated' homogeneous terms, common terms taken away)
     :divided    (f(a + e) - f(a)) / e
     :struck     the quotient with e struck out"
  [f a]
  (let [compared (e/simplify (e/- (f (e/+ a 'e)) (f a)))
        divided (e/simplify (e// compared 'e))]
    {:compared compared :divided divided :struck (strike-e divided)}))

;; ---------------------------------------------------------------------------
;; The rectangle: Sit recta AC dividenda in E, ut rectangulum AEC sit maximum

(defn rectangle
  "The rectangle under the two parts of B cut at A: A (B - A)."
  [B]
  (fn [A] (e/* A (e/- B A))))

(defn rectangle-solution
  "Fermat's answer: the point of division a = B/2."
  [B]
  (e// B 2))

;; ---------------------------------------------------------------------------
;; The tangent to the parabola: CD = D given, CE = A sought, CI = E

(defn tangent-defect
  "Fermat's comparison for the parabola: point O on the tangent, I its foot,
   with CD = d, CE = a, CI = e. O outside the curve gives
   d (a^2 - 2 a e + e^2) > (d - e) a^2; the difference, adequated to 0, is
   d e^2 - 2 d a e + a^2 e."
  [d a]
  (fn [e] (e/- (e/* d (e/square (e/- a e))) (e/* (e/- d e) (e/square a)))))

(defn subtangent
  "Fermat's result: CE = 2 CD."
  [d]
  (e/* 2 d))

(defn parabola-point
  "Figure: the point at abscissa t of the parabola x = y^2 / p (vertex D at
   the origin, diameter along x), with p the latus rectum."
  [p]
  (fn [[t]] [(e// (e/square t) p) t]))

(defn tangent-line
  "Figure: the point s of the tangent at B = (b^2/p, b), from E (s = 0) to
   B (s = 1) and beyond."
  [p b]
  (let [bx (e// (e/square b) p)
        E [(e/- 0 bx) 0]]
    (fn [[s]] [(e/+ (first E) (e/* s (e/- bx (first E)))) (e/* s b)])))

(defn secant-point
  "Figure: the point at s of the chord of the rectangle's curve
   y = a (B - a), from a = x0 (s = 0) to a = x0 + e (s = 1), drawn on past
   both ends. Its slope is Fermat's quotient before E is struck out; as e
   shrinks the chord turns into the tangent."
  [B x0 e]
  (let [f (rectangle B)
        slope (e// (e/- (f (e/+ x0 e)) (f x0)) e)]
    (fn [[s]]
      [(e/+ x0 (e/* s e)) (e/+ (f x0) (e/* slope s e))])))

;; ---------------------------------------------------------------------------
;; Graded claims

(defn identities
  "[label difference] pairs, each zero."
  []
  (let [B 'B a 'a d 'D
        {:keys [compared divided struck]} (adequate (rectangle B) a)
        cubic (fn [x] (e/+ (e/* 'c3 (e/cube x)) (e/* 'c2 (e/square x)) (e/* 'c1 x) 'c0))
        quartic (fn [x] (e/+ (e/* 'c4 (e/expt x 4)) (cubic x)))
        tangent (tangent-defect d a)]
    [["adequated: B E - 2 A E - E^2" (e/- compared (e/- (e/* B 'e) (e/* 2 a 'e) (e/square 'e)))]
     ["divided by E: B - 2 A - E" (e/- divided (e/- B (e/* 2 a) 'e))]
     ["E struck out: B = 2 A" (e/- struck (e/- B (e/* 2 a)))]
     ["so the rectangle is greatest at A = B/2" ((fn [x] (:struck (adequate (rectangle B) x))) (rectangle-solution B))]
     ["adequality is the derivative: rectangle" (e/- struck ((e/D (rectangle B)) a))]
     ["adequality is the derivative: any cubic" (e/- (:struck (adequate cubic a)) ((e/D cubic) a))]
     ["adequality is the derivative: any quartic" (e/- (:struck (adequate quartic a)) ((e/D quartic) a))]
     ["tangent: D E^2 - 2 D A E + A^2 E, divided by E, E struck: A^2 = 2 D A"
      (e/- (strike-e (e// (tangent 'e) 'e)) (e/- (e/square a) (e/* 2 d a)))]
     ["so CE = 2 CD" (strike-e (e// ((tangent-defect d (subtangent d)) 'e) 'e))]
     ["the tangent meets the curve only at B: a double root"
      (let [p 'p b 'b t 't
            [x y] ((tangent-line p b) [(e// t b)])]
        (e/- (e/- x (e// (e/square y) p)) (e/- 0 (e// (e/square (e/- t b)) p))))]]))

(defn- grade-of [kind diffs]
  (let [res (grade/grade kind diffs)]
    (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails)))

(defn graded
  "[{:label :grade}] of the identities."
  []
  (mapv (fn [[label diff]] {:label label :grade (grade-of :symbolic [diff])}) (identities)))

;; ---------------------------------------------------------------------------
;; Adequality and Descartes' double root are one idea

(defn chord-intercept
  "Where the chord of the parabola x = y^2/p through B = (b^2/p, b) and a
   second point B' = (t^2/p, t) meets the diameter: x = -bt/p."
  [p b]
  (fn [t] (e/- 0 (e// (e/* b t) p))))

(defn chord-point
  "Figure: the point at s of the chord of x = y^2/p through B = (b^2/p, b)
   and B' = (t^2/p, t), from its meeting E with the diameter (s = 0) to B
   (s = 1); state [t s]. At t = b the chord is the tangent, CE = 2 CD."
  [p b]
  (fn [[t s]]
    (let [ex ((chord-intercept p b) t)
          bx (e// (e/square b) p)]
      [(e/+ ex (e/* s (e/- bx ex))) (e/* s b)])))

(defn descartes-discriminant
  "Descartes' rule on Fermat's parabola y^2 = p x, B = (D, b) on it: the
   line from E (CE = A, E at x = D - A) through B meets the curve where
   y^2/p - (A/b) y + (A - D) = 0. Its discriminant; a double root (the
   line touches) exactly when it vanishes."
  [p b A D]
  (e/- (e/square (e// A b)) (e/* 4 (e// 1 p) (e/- A D))))

(defn double-root-identities
  "[label difference] pairs, each zero: Fermat's adequality and Descartes'
   double root find the same tangent."
  []
  (let [B 'B a 'a x 'x p 'p b 'b t 't A 'A D 'D
        cubic (fn [y] (e/+ (e/* 'c3 (e/cube y)) (e/* 'c2 (e/square y)) (e/* 'c1 y) 'c0))
        f (rectangle B)
        on-curve (e// (e/square b) D)]
    [["rectangle: f(x) - f(a) - (x - a)(B - 2a) = -(x - a)^2, a double root at x = a"
      (e/+ (e/- (f x) (f a) (e/* (e/- x a) (:struck (adequate f a)))) (e/square (e/- x a)))]
     ["any cubic: f(x) - f(a) - (x - a) times Fermat's quotient = (x - a)^2 (c3 (x + 2a) + c2)"
      (e/- (e/- (cubic x) (cubic a) (e/* (e/- x a) (:struck (adequate cubic a))))
           (e/* (e/square (e/- x a)) (e/+ (e/* 'c3 (e/+ x (e/* 2 a))) 'c2)))]
     ["the chord BB' meets the diameter at x = -bt/p"
      (let [[ex ey] ((chord-point p b) [t 0])
            [x1 y1] ((chord-point p b) [t 1])]
        (e/+ ey (e/- (e/* (e/- x1 ex) (e/- t ey)) (e/* (e/- y1 ey) (e/- (e// (e/square t) p) ex)))))]
     ["CE - 2 CD = b(t - b)/p: E = t - b, struck out, gives CE = 2 CD"
      (e/- (e/- (e/- (e// (e/square b) p) ((chord-intercept p b) t)) (e/* 2 (e// (e/square b) p)))
           (e// (e/* b (e/- t b)) p))]
     ["Descartes: with b^2 = p D the discriminant is (A - 2D)^2 / b^2"
      (e/- (e/* (e/square b) (descartes-discriminant on-curve b A D)) (e/square (e/- A (e/* 2 D))))]
     ["so the roots are equal exactly when A = 2D, Fermat's CE = 2 CD"
      (descartes-discriminant on-curve b (e/* 2 D) D)]]))

(defn double-root-graded
  "[{:label :grade}] of the double-root identities."
  []
  (mapv (fn [[label diff]] {:label label :grade (grade-of :symbolic [diff])}) (double-root-identities)))

(defn steps-for
  "Fermat's steps for the rectangle on B = b at a, as exact numbers, for a
   figure that shows them while the rectangle grows: :area-a, :area-ae and
   the quotient for a few e."
  [b a es]
  (let [f (rectangle b)]
    (mapv (fn [e] {:e e :fa (f a) :fae (f (+ a e)) :quotient (/ (- (f (+ a e)) (f a)) e)}) es)))

(defn rectangle-curve
  "Figure: [a, k a (B - a)], the rectangle on B cut at a, drawn at height
   scale k (the curve of :fermat/rectangle)."
  [B k]
  (fn [[a]] [a (e/* k ((rectangle B) a))]))

(defn adequation
  "Figure: Fermat's numbers at the cut a with excess e, state [a e w]:
   w = 0 [f(a), f(a + e)], w = 1 [B e, 2 a e + e^2] (the terms compared
   after the common ones are taken away), w = 2 [B - 2a - e, B - 2a] (the
   quotient before and after e is struck out). Weights keep it one
   expression."
  [B]
  (fn [[a e w]]
    (let [f (rectangle B)
          w0 (e// (e/* (e/- w 1) (e/- w 2)) 2)
          w1 (e/- 0 (e/* w (e/- w 2)))
          w2 (e// (e/* w (e/- w 1)) 2)
          mix (fn [u v z] (e/+ (e/* w0 u) (e/* w1 v) (e/* w2 z)))]
      [(mix (f a) (e/* B e) (e/- B (e/* 2 a) e))
       (mix (f (e/+ a e)) (e/+ (e/* 2 a e) (e/square e)) (e/- B (e/* 2 a)))])))

(defn outside-ratios
  "Figure: [CD : DI, BC^2 : OI^2] for the point O at s on the tangent to
   x = y^2 at B = (1, 1) (E at s = 0): O = (2s - 1, s), I its foot."
  []
  (fn [[s]] (let [ix (e/- (e/* 2 s) 1)] [(e// 1 ix) (e// 1 (e/square s))])))

(def figures
  "Every moving figure, by name: {:f figure :params initial-params :state initial-state}."
  {:secant {:f secant-point :params [4 1 1] :state [0]}
   :rectangle {:f rectangle-curve :params [4 0.25] :state [0]}
   :adequation {:f adequation :params [4] :state [1 0.5 0]}
   :parabola {:f parabola-point :params [1] :state [0]}
   :tangent {:f tangent-line :params [1 1] :state [0]}
   :outside {:f outside-ratios :params [] :state [0.6]}})

(def proofs-resource "alexandria/fermat/maxima.proofs.edn")

(defn proof [] (proofs/read-proofs proofs-resource))
