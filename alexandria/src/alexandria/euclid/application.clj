(ns alexandria.euclid.application
  "The application of areas, Euclid I.44 and VI.27-29, as one tied
   relation, and the three names the Pythagoreans gave its cases.

   On the line AB = a apply a rectangle of height y whose base runs from A
   to X, AX = a + k y: falling short of B by a rectangle XB that is |k| y
   wide and y high (k < 0, VI.28; the defect similar to the given
   rectangle of sides |k| : 1), exactly on AB (k = 0, I.44), or running
   past B by such a rectangle (k > 0, VI.29). The applied area is

     S = (a + k y) y.

   Proclus (after Eudemus) on I.44: the Pythagoreans called the three
   cases parabole (application), hyperbole (exceeding) and elleipsis
   (falling short), and Apollonius moved the names to the conic sections
   (alexandria.apollonius.conics/names).

   The tie (widget brief section 5): a, S, y and k are live controls.

     y, a or k moves   the other two of (a, y, k) held, S re-solved
                       (forward); for k < 0 y is held at most a / 2|k|,
                       where the applied rectangle is greatest (VI.27)
     S moves           a and k held, y re-solved (inverse: the root
                       y = 2 S / (a + sqrt(a^2 + 4 k S)), the near one in
                       VI.28; for k < 0 S is held at most a^2 / 4|k|, the
                       diorismos of VI.27)

   Emmy turns the relation into the quadratic k y^2 + a y - S = 0, checks
   its root (written so that k = 0 is no division by zero) and grades every
   round trip :grade/proved. The rectangles are drawn by the :rect kernel
   of alexandria.apollonius.areas-steps; the numbers are `figure`'s, both
   compiled by raster."
  (:require [alexandria.grade :as grade]
            [emmy.env :as e]
            [hive-dsl.result :as r]))

(defn at-least "max(x, lo), branch-free." [x lo] (e/* 0.5 (e/+ x lo (e/abs (e/- x lo)))))
(defn at-most "min(x, hi), branch-free." [x hi] (e/* 0.5 (e/- (e/+ x hi) (e/abs (e/- x hi)))))

(defn area
  "The applied area: base a + k y, height y (forward)."
  [a k y]
  (e/* (e/+ a (e/* k y)) y))

(defn height
  "The height of the rectangle applied to a with area S (inverse):
   y = 2 S / (a + sqrt(a^2 + 4 k S)), finite at k = 0 (I.44: S / a)."
  [a k S]
  (e// (e/* 2 S) (e/+ a (e/sqrt (e/+ (e/square a) (e/* 4 k S))))))

(def ranges
  "Slider ranges [min max step]."
  {:a [2 12 0.01] :S [0.5 40 0.01] :y [0.1 8 0.01] :k [-1 1 0.01]})

(def tied [:a :S :y :k])

(def tiny "How close to 0 a held k may come in a division." 1e-6)

(defn- y-max
  "a / 2|k| for k < 0 (VI.27's greatest), far off for k >= 0."
  [a k]
  (e// a (e/* 2 (at-least (e/negate k) tiny))))

(defn- S-max
  "a^2 / 4|k| for k < 0 (VI.27: no rectangle is greater), far off else."
  [a k]
  (e// (e/square a) (e/* 4 (at-least (e/negate k) tiny))))

(defn- forward-block
  "a, y, k held (y no further than VI.27's greatest), S forward."
  [a y k]
  (let [y' (at-most y (y-max a k))]
    [a (area a k y') y' k]))

(defn- inverse-block
  "a, k held, S held to VI.27's bound, y by the inverse."
  [a S k]
  (let [S' (at-most S (S-max a k))]
    [a S' (height a k S') k]))

(def source-block
  "Where each source's solved state [a S y k] starts in the tie's output."
  {:a 0 :S 4 :y 8 :k 12})

(defn tie
  "The tie kernel: the solved state [a S y k] for each source in the order
   of source-block (16 numbers)."
  [a S y k]
  (fn [_]
    (vec (concat (forward-block a y k) (inverse-block a S k)
                 (forward-block a y k) (forward-block a y k)))))

(def outputs
  "`figure`'s numbers, in order."
  [:base :applied :left :left-w :top :y-max :S-max :a :y :k])

(defn figure
  "Every number the panel prints, for a, y, k: AX = a + k y, the applied
   area, the leftover k y^2 (negative: the defect), its width |k| y, the
   square on the half (a/2)^2, the greatest applied height a/2|k| and area
   a^2/4|k| (k < 0), and a, y, k."
  [a y k]
  (fn [_]
    (let [base (e/+ a (e/* k y))]
      [base (e/* base y) (e/* k y y) (e/* (e/abs k) y)
       (e/square (e// a 2)) (y-max a k) (S-max a k) a y k])))

(def band "|k| below this names the application exact." 0.005)

(defn kind
  "The name of the case: :parabola (applied exactly), :hyperbola
   (exceeding), :ellipse (falling short), the keys of
   alexandria.apollonius.conics/names."
  [k]
  (cond (< (Math/abs (double k)) band) :parabola
        (pos? k) :hyperbola
        :else :ellipse))

(def propositions
  "Euclid's proposition for each case, and Heath's statement (1908)."
  {:parabola {:prop "I.44" :says "To a given straight line to apply, in a given rectilineal angle, a parallelogram equal to a given triangle."}
   :ellipse {:prop "VI.28" :says "To a given straight line to apply a parallelogram equal to a given rectilineal figure and deficient by a parallelogrammic figure similar to a given one."}
   :hyperbola {:prop "VI.29" :says "To a given straight line to apply a parallelogram equal to a given rectilineal figure and exceeding by a parallelogrammic figure similar to a given one."}})

(def start "a = 10, k = -1 (the defect a square, VI.28), y = 3: S = 21." {:a 10 :S 21 :y 3 :k -1})

;; ---------------------------------------------------------------------------
;; The derivation and the grades

(defn- eq [lhs rhs] (e/literal-number (list '= (e/freeze lhs) (e/freeze rhs))))

(defn- vi-27-gap
  "a^2/4|k| - S for k < 0, and the completed square |k| (y - a/2|k|)^2."
  []
  [(e/- (e// (e/square 'a) (e/* -4 'k)) (area 'a 'k 'y))
   (e/* (e/negate 'k) (e/square (e/+ 'y (e// 'a (e/* 2 'k)))))])

(defn equations
  "[{:label :eqs}] for emmy.clerk/->TeX: the relation, the quadratic in y,
   the root, the bound. (An equation whose two sides Emmy writes the same
   collapses to a boolean, so each right-hand side is kept in the form the
   page means: S, and the completed square unsimplified.)"
  []
  (let [[gap sq] (vi-27-gap)]
    [{:label "the applied rectangle, AX = a + k y wide and y high, is S"
      :eqs [(eq 'S (e/simplify (area 'a 'k 'y)))]}
     {:label "as an equation for y (Emmy expands it): a quadratic"
      :eqs [(eq (e/simplify (area 'a 'k 'y)) 'S)]}
     {:label "its root, rationalized so that k = 0 gives I.44's S/a"
      :eqs [(eq 'y (height 'a 'k 'S))]}
     {:label "k < 0: the square completed (II.5), so S can be no greater than a²/4|k| (VI.27)"
      :eqs [(eq (list '- (list '/ (list 'expt 'a 2) (list '* 4 (list '- 'k))) 'S) sq)]}]))

(defn- variant [res] (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails))

(defn- proved [id label diff]
  {:id id :label label :grade (variant (grade/grade :symbolic [diff]))})

(defn grades
  "Each step and round trip, graded by alexandria.grade (:grade/proved)."
  []
  (let [[gap sq] (vi-27-gap)]
    [(proved :inverse "the inverse y solves (a + k y) y = S"
             (e/- (area 'a 'k (height 'a 'k 'S)) 'S))
     (proved :round-trip-y "y → S → y is y (the near root)"
             (e/- (height 'a 'k (area 'a 'k 'y)) 'y))
     (proved :round-trip-S "S → y → S is S"
             (e/- (area 'a 'k (height 'a 'k 'S)) 'S))
     (proved :I.44 "k = 0: the height is S/a (I.44)"
             (e/- (height 'a 0 'S) (e// 'S 'a)))
     (proved :VI.28 "k = −1: the applied rectangle and the square make up the strip a·y"
             (e/- (e/+ (area 'a -1 'y) (e/square 'y)) (e/* 'a 'y)))
     (proved :VI.29 "k = 1: the strip a·y and the square make up the applied rectangle"
             (e/- (area 'a 1 'y) (e/+ (e/* 'a 'y) (e/square 'y))))
     (proved :VI.27 "k < 0: a²/4|k| − S = |k| (y − a/2|k|)², so S ≤ a²/4|k|"
             (e/- gap sq))]))
