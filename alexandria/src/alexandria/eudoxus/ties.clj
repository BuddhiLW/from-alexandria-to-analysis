(ns alexandria.eudoxus.ties
  "The equations behind the Eudoxus widgets, derived and graded by Emmy.

     Elements V, Def. 5   four magnitudes A, B, C, D with C : D = A : B,
                          tied by D = C B / A; equimultiples m, n
     Elements XII.2       the inscribed 2^k-gon, its share of the circle
                          and the leftover, tied to the bound eps

   Def. 5 is proved by an identity: when D = C B / A,
   m C - n D = (C / A)(m A - n B), and C / A > 0, so m C and n D compare
   exactly as m A and n B do, for every m and n. The widget shows the two
   comparisons named live (alike exceed, alike equal, alike fall short)."
  (:require [alexandria.grade :as grade]
            [emmy.env :as e]
            [hive-dsl.result :as r]))

(def ^:private PI Math/PI)

(defn fourth
  "D with C : D = A : B."
  [A B C]
  (e// (e/* C B) A))

(defn third
  "C with C : D = A : B."
  [A B D]
  (e// (e/* A D) B))

(defn share
  "The share of the circle covered by the inscribed n-gon:
   (n/2) sin(2 pi / n) / pi."
  [n]
  (e// (e/* (e// n 2) (e/sin (e// (* 2 PI) n))) PI))

(defn leftover [n] (e/- 1 (share n)))

(defn leftover-start "The leading term: eps ~ 2 pi^2 / (3 n^2), as n." [eps] (e/sqrt (e// (* 2 PI PI) (e/* 3 eps))))

(defn leftover-step
  "One Newton step on leftover(n) = eps (Emmy's D)."
  [u eps]
  (let [f (fn [n] (e/- (leftover n) eps))]
    (e/- u (e// (f u) ((e/D f) u)))))

(defn k-of-leftover
  "The k whose 2^k-gon leaves the fraction eps of the circle: three
   leftover-steps from leftover-start, then k = log2 n. One expression for
   the browser; on the JVM iterate the leftover-step kernel."
  [eps]
  (let [n (nth (iterate #(leftover-step % eps) (leftover-start eps)) 3)]
    (e// (e/log n) (e/log 2))))

(defn- variant [res] (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails))
(defn- proved [label diff] {:label label :grade (variant (grade/grade :symbolic [diff]))})

(defn grades
  "Def. 5 for the tied magnitudes, and the tie's exact inverses."
  []
  (let [A 'A B 'B C 'C D 'D m 'm n 'n]
    [(proved "Def. 5, tied: with D = CB/A, mC − nD = (C/A)(mA − nB), so the two compare alike for every m, n"
             (e/- (e/- (e/* m C) (e/* n (fourth A B C))) (e/* (e// C A) (e/- (e/* m A) (e/* n B)))))
     (proved "forward then inverse: C → D → C" (e/- (third A B (fourth A B C)) C))
     (proved "inverse then forward: D → C → D" (e/- (fourth A B (third A B D)) D))
     (proved "XII.2, the doubled polygon: 2n triangles (r²/2) sin(π/n) = n r² sin(π/n)"
             (e/- (e/* 2 n 1/2 (e/square 'r) (e/sin (e// 'pi n))) (e/* n (e/square 'r) (e/sin (e// 'pi n)))))]))

(defn- eq [lhs rhs] (e/literal-number (list '= (e/freeze lhs) (e/freeze rhs))))

(defn equations []
  [{:label "the fourth proportional" :eqs [(eq 'D (fourth 'A 'B 'C))]}
   {:label "the equimultiples compared" :eqs [(eq (list '- (list '* 'm 'C) (list '* 'n 'D))
                                                  (e/simplify (e/- (e/* 'm 'C) (e/* 'n (fourth 'A 'B 'C)))))]}])
