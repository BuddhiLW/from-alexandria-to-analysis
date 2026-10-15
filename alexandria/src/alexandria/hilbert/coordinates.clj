(ns alexandria.hilbert.coordinates
  "The analytic model of Hilbert's plane axioms: points are pairs of real
   numbers, the line through p and q is the zero set of a determinant.
   Each check is an Emmy expression in free symbols that must simplify to
   exactly 0, so it holds for every choice of the real coordinates."
  (:require [emmy.env :as e]))

(defn line
  "Coefficients [a b c] of a x + b y + c = 0 through [x1 y1] and [x2 y2]."
  [[x1 y1] [x2 y2]]
  [(e/- y1 y2) (e/- x2 x1) (e/- (e/* x1 y2) (e/* x2 y1))])

(defn on [[a b c] [x y]] (e/+ (e/* a x) (e/* b y) c))

(defn meet
  "The intersection of two lines by Cramer's rule."
  [[a b c] [d f g]]
  (let [det (e/- (e/* a f) (e/* b d))]
    [(e// (e/- (e/* b g) (e/* c f)) det) (e// (e/- (e/* c d) (e/* a g)) det)]))

(def p ['x1 'y1])
(def q ['x2 'y2])
(def r ['x3 'y3])
(def s ['x4 'y4])

(defn checks
  "[{:axiom label :says text :residual simplified-expression}]."
  []
  (let [l (line p q) m (line r s) x (meet l m)
        [a b c] l
        par [a b (e/- 0 (on [a b 0] r))]]
    (mapv (fn [[axiom says expr]] {:axiom axiom :says says :residual (e/simplify expr)})
          [["I 1" "The line through P and Q contains P." (on l p)]
           ["I 1" "The line through P and Q contains Q." (on l q)]
           ["I 2" "Two lines meet in their Cramer point: it lies on the first." (e/* (e/- (e/* a (second m)) (e/* b (first m))) (on l x))]
           ["I 2" "The same point lies on the second line." (e/* (e/- (e/* a (second m)) (e/* b (first m))) (on m x))]
           ["III" "The parallel to PQ through R contains R." (on par r)]
           ["III" "It has no point in common with PQ: the determinant of the two is 0." (e/- (e/* a (second par)) (e/* b (first par)))]])))

(defn proved?
  "Every residual simplified to the expression 0 (Emmy wraps it as a
   literal, so compare through e/zero?)."
  []
  (every? #(e/zero? (e/freeze (:residual %))) (checks)))
