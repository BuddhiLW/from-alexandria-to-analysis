;; # Eudoxus: ratios without numbers, and exhaustion
;;
;; Eudoxus of Cnidus (c. 408-355 BC) studied with Plato's circle in Athens
;; and founded a school at Cyzicus. None of his books survives. Euclid's
;; *Elements* carries his two great ideas: Book V, a theory of ratio that
;; works for lengths no whole numbers measure, and the method of
;; exhaustion of Book XII, which Archimedes, a century later, made his own.
;;
;; The problem he solved came from the Pythagoreans. The side and the
;; diagonal of a square have no common measure. A ratio of whole numbers
;; cannot describe them, yet geometry needs their ratio. The quotations
;; below are from T. L. Heath's *Elements* (1908).
^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns eudoxus
  {:history/year -350
   :history/title "Eudoxus, Elements V and XII"
   :history/era "Classical Greek"
   :history/fields #{:geometry :analysis}
   :nextjournal.clerk/visibility {:code :hide :result :show}
   :nextjournal.clerk/toc true}
  (:require [alexandria.eudoxus.exhaustion-view :as view]
            [alexandria.eudoxus.proportion :as ep]
            [alexandria.archimedes.playground :as playground]
            [alexandria.eudoxus.ties :as ties]
            [alexandria.eudoxus.widgets :as widgets]
            [alexandria.medium.tied :as tie]
            [alexandria.proofs :as proofs]
            [history-of-math.page :as page]
            [history-of-math.widgets :as hom]
            [nextjournal.clerk :as clerk]))

^{::clerk/visibility {:result :hide}}
(hom/install!)

;; ## The side and the diagonal
;;
;; Write $d$ for the diagonal and $s$ for the side. By *Elements* I.47 the
;; square on the diagonal is twice the square on the side: $d^2 = 2s^2$.
;; Suppose $d : s$ were $p : q$ for whole numbers $p, q$ with no common
;; factor. The argument, which Aristotle reports, runs by parity (even and
;; odd):

(page/steps (proofs/steps ep/proofs-resource :eudoxus/side-diagonal))

;; Emmy checks the algebra each step uses, with $r$, $q$, $k$ and $s$ left
;; as symbols, so the identities hold for every number:

(page/grade-badges (take 3 (ep/graded)))

;; ## Definition 5: same ratio by equimultiples
;;
;; Eudoxus does not divide one length by another. He compares *multiples*.
;; Take whole numbers $m$ and $n$. A *multiple* $mA$ is $A$ laid off $m$
;; times.
;;
;; > *Magnitudes are said to be in the same ratio, the first to the second
;; > and the third to the fourth, when, if any equimultiples whatever be
;; > taken of the first and third, and any equimultiples whatever of the
;; > second and fourth, the former equimultiples alike exceed, are alike
;; > equal to, or alike fall short of, the latter equimultiples respectively
;; > taken in corresponding order.*
;; >
;; > — Book V, Def. 5 (Heath 1908, vol. 2, p. 114)
;;
;; So $A : B = C : D$ when, for every $m$ and $n$, $mA > nB$ exactly when
;; $mC > nD$, and likewise for $=$ and $<$. Definition 7 says $A : B$ is
;; *greater* than $C : D$ when some $m, n$ give $mA > nB$ but not
;; $mC > nD$.

(page/grade-badges (drop 5 (ep/graded)))

;; ### Definition 5, live
;;
;; Four magnitudes as bars, tied so that $C : D = A : B$. Move $A$, $B$ or
;; $C$ and $D$ follows as the fourth proportional; move $D$ and $C$
;; follows, with $A$ and $B$ held. The equimultiples $m$ and $n$ are free.
;; The page names both comparisons live: $mA$ against $nB$, and $mC$
;; against $nD$. Whatever $m$ and $n$ are, they come out *alike*, which is
;; Definition 5. Emmy proves why: with $D = CB/A$,
;; $mC - nD = \frac{C}{A}(mA - nB)$, and $\frac{C}{A} > 0$.

(widgets/def-5)

(tie/derivation (ties/equations) (take 3 (ties/grades)))

;; For the diagonal and the side, the test $n d > m s$ becomes, after
;; squaring with I.47, $2n^2 > m^2$: whole numbers only. Each fraction
;; $m/n$ falls below or above $d : s$, never on it.

(clerk/table
 {:head ["m/n" "2n² vs m²" "against d : s"]
  :rows (for [{:keys [m n side]} (ep/cut-sample 4)]
          [(str m "/" n) (str (* 2 n n) (if (= side :lower) " > " " < ") (* m m))
           (if (= side :lower) "below" "above")])})

;; The ratio $d : s$ is fixed by which fractions lie below it and which
;; above. In 1872 Richard Dedekind took exactly this split of the
;; fractions, a *cut*, as the definition of a real number, and said that
;; he followed Euclid's Book V. That is the notebook
;; [Dedekind and Cantor](../dedekind-cantor/).

;; ## Exhaustion: *Elements* XII.2
;;
;; > *Circles are to one another as the squares on the diameters.*
;;
;; Eudoxus' second idea measures curved figures. Inscribe a square in the
;; circle, then double the sides again and again. Each doubling takes more
;; than half of what was left, so the leftover falls below any given area
;; (X.1). Polygons obey the ratio of the squares on the diameters (XII.1);
;; a double contradiction, run with Definition 5, carries it to the
;; circles. The polygons below come from a raster kernel; the fraction of
;; the circle each covers is computed from its vertices.

(view/xii-2)
;; Now try it yourself: drag either diameter and double both polygons.
(playground/eudoxus)


(page/grade-badges (take 2 (drop 3 (ep/graded))))

;; ### The exhaustion, live
;;
;; X.1 is the engine: given any $\varepsilon$, enough doublings leave less
;; than $\varepsilon$ of the circle uncovered. Move $k$ and the
;; $2^k$-gon and its leftover follow. Move $\varepsilon$ and $k$ follows,
;; solved by Newton's method on Emmy's derivative.

(widgets/xii-2)

(page/grade-badges (drop 3 (ties/grades)))

;; ## Where this goes
;;
;; Archimedes, the next notebook, uses exhaustion to measure the circle and
;; the parabolic segment. The theory of Book V stays the foundation of
;; ratio until Dedekind's cuts of 1872 turn it into the real numbers.

^{::clerk/no-cache true}
(page/prev-next "notebooks/eudoxus.clj")
