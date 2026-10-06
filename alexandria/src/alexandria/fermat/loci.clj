(ns alexandria.fermat.loci
  "Fermat, Ad locos planos et solidos isagoge (written before 1637; Oeuvres,
   ed. Tannery and Henry, vol. I, 1891, pp. 91-103): an equation in two
   unknowns is a locus. NZ = A runs along a line given in position from
   the given point N, ZI = E stands on it at a given angle (here a right
   angle), and the end I of E traces a line or a conic.

     line       D in A = B in E          I on a line through N
     hyperbola  A in E = Z pl.           I on a hyperbola, asymptotes NZ and NR
     parabola   A q. = D in E            I on a parabola, diameter NP, latus rectum D
     circle     B q. - A q. = E q.       I on the circle about N, radius B
     ellipse    (B q. - A q.) : E q.     I on an ellipse, centre N, semi-axis B
                given ratio

   Each locus is a figure: a parameter u along the curve gives I = (A, E),
   and Emmy proves for every u that I satisfies Fermat's equation. It is
   Descartes' Book II idea (a curve is an equation in two unknowns), found
   independently and with Viete's vowels for the unknowns.

   Reuse searched: emmy (simplify), alexandria.grade; the curves are
   rational parametrisations so no trigonometry is left to simplify."
  (:require [alexandria.grade :as grade]
            [alexandria.proofs :as proofs]
            [emmy.env :as e]
            [hive-dsl.result :as r]))

(defn line
  "D in A = B in E: I at A = u, E = D u / B."
  [D B]
  (fn [[u]] [u (e// (e/* D u) B)]))

(defn hyperbola
  "A in E = Z: I at A = u, E = Z / u."
  [Z]
  (fn [[u]] [u (e// Z u)]))

(defn parabola
  "A q. = D in E: I at A = u, E = u^2 / D."
  [D]
  (fn [[u]] [u (e// (e/square u) D)]))

(defn circle
  "B q. - A q. = E q.: I = B ((1 - u^2), 2u) / (1 + u^2)."
  [B]
  (fn [[u]] (let [w (e/+ 1 (e/square u))]
              [(e// (e/* B (e/- 1 (e/square u))) w) (e// (e/* B 2 u) w)])))

(defn ellipse
  "(B q. - A q.) : E q. = k : 1, i.e. E q. = (B q. - A q.) / k: the circle
   with E shrunk in the ratio 1 : sqrt k, written with m = 1/sqrt k so the
   figure stays rational: E = m times the circle's E."
  [B m]
  (fn [[u]] (let [[x y] ((circle B) [u])] [x (e/* m y)])))

(defn equations
  "Fermat's equation of each locus, as a function of [A E] that vanishes
   on it."
  [{:keys [D B Z m]}]
  {:line (fn [[A E]] (e/- (e/* D A) (e/* B E)))
   :hyperbola (fn [[A E]] (e/- (e/* A E) Z))
   :parabola (fn [[A E]] (e/- (e/square A) (e/* D E)))
   :circle (fn [[A E]] (e/- (e/- (e/square B) (e/square A)) (e/square E)))
   :ellipse (fn [[A E]] (e/- (e/* (e/square m) (e/- (e/square B) (e/square A))) (e/square E)))})

(defn identities
  "[label difference] pairs, each zero: the figure of each locus satisfies
   Fermat's equation for every u, and the general second-degree reading."
  []
  (let [u 'u D 'D B 'B Z 'Z m 'm
        eqs (equations {:D D :B B :Z Z :m m})
        on (fn [k fig] ((eqs k) (fig [u])))]
    [["D in A = B in E: the point I is on a line given in position" (on :line (line D B))]
     ["the ratio A : E is given, B : D, so the angle INZ is given" (e/- (e// (first ((line D B) [u])) (second ((line D B) [u]))) (e// B D))]
     ["A in E = Z pl.: the point I is on a hyperbola" (on :hyperbola (hyperbola Z))]
     ["A q. = D in E: the point I is on a parabola" (on :parabola (parabola D))]
     ["B q. - A q. = E q.: the point I is on a circle given in position" (on :circle (circle B))]
     ["(B q. - A q.) : E q. given: the point I is on an ellipse" (on :ellipse (ellipse B m))]
     ["Descartes' ellipse of Book II, x^2 = r y - (r/q) y^2, is Fermat's with B = q/2, m^2 = r/q, A = y - q/2"
      (let [r 'r q 'q y 'y x2 (e/- (e/* r y) (e/* (e// r q) (e/square y)))]
        (e/- x2 (e/* (e// r q) (e/- (e/square (e// q 2)) (e/square (e/- y (e// q 2)))))))]]))

(defn- grade-of [kind diffs]
  (let [res (grade/grade kind diffs)]
    (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails)))

(defn graded
  "[{:label :grade}] of the identities."
  []
  (mapv (fn [[label diff]] {:label label :grade (grade-of :symbolic [diff])}) (identities)))

(def figures
  "Every moving figure, by name: {:f figure :params initial-params :state initial-state}."
  {:line {:f line :params [1 2] :state [0]}
   :hyperbola {:f hyperbola :params [1] :state [1]}
   :parabola {:f parabola :params [1] :state [0]}
   :circle {:f circle :params [1.5] :state [0]}
   :ellipse {:f ellipse :params [1.5 0.6] :state [0]}})

(def proofs-resource "alexandria/fermat/loci.proofs.edn")

(defn proof [] (proofs/read-proofs proofs-resource))
