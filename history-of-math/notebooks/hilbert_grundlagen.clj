;; # Hilbert: what makes a geometry possible?
;;
;; In 1899 David Hilbert replaced Euclid's tacit appeals to a diagram with
;; explicitly grouped assumptions. The *Grundlagen der Geometrie* first
;; names three sorts of objects, then specifies how they may stand in
;; relation to one another. The words "point" and "line" do not carry a
;; hidden picture into the proof. We read the German first edition
;; (Leipzig: Teubner, 1899); the English sentences below are our own.
^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns hilbert-grundlagen
  {:history/year 1899
   :history/title "Hilbert: the axioms of geometry and their models"
   :history/era "Structures, foundations and computation"
   :history/fields #{:geometry :logic-computing}
   :nextjournal.clerk/visibility {:code :hide :result :show}
   :nextjournal.clerk/toc true}
  (:require [alexandria.hilbert.coordinates :as coords]
            [alexandria.hilbert.widgets :as w]
            [history-of-math.lens :as lens]
            [history-of-math.page :as page]
            [history-of-math.widgets :as hom]
            [nextjournal.clerk :as clerk]))

^{::clerk/visibility {:code :hide :result :hide}}
(hom/install!)

;; ## Three kinds of things (1899)
;;
;; Hilbert starts with objects rather than definitions of their essence:

(page/blockquote
 "Wir denken uns drei verschiedene Systeme von Dingen: die Dinge des ersten Systems nennen wir Punkte und bezeichnen sie mit A, B, C, ..."
 "Hilbert, *Grundlagen der Geometrie*, 1st ed. (Leipzig: Teubner, 1899), §1, p. 1. Our English: We imagine three different systems of things; we call the things of the first system points and denote them by A, B, C, ...")

;; The other two systems are straight lines and planes. What matters is not
;; their names but the stipulated relations: lying on, between, congruent.
;;
;; ## Five groups of axioms
;;
;; Incidence specifies which objects lie on which. Order says when a point
;; lies between two others. Parallels constrain non-intersecting lines in
;; a plane. Congruence compares segments and angles. Continuity connects
;; the ordered geometry to the real-number line. None of these follows
;; merely from the meaning of the word "point."

(w/axiom-table)

;; ## Independence and a failed theorem
;;
;; To test an assumption, retain the others and change the interpretation.
;; Desargues' theorem says that two triangles in perspective from a point
;; O have their corresponding sides meeting in three collinear points X, Y
;; and Z. Hilbert shows that the plane axioms of incidence, order and
;; parallels do not imply it. The theorem follows once the plane sits in a
;; space where the spatial incidence axioms hold, or once congruence is
;; assumed, and he exhibits a plane geometry in which it fails. In 1902
;; Forest Ray Moulton published a simpler such plane (*Transactions of the
;; American Mathematical Society* 3, pp. 192-195). His lines are ordinary
;; lines, except that a line of one slope sign bends where it crosses a
;; fixed axis. Two points still determine exactly one line and the order
;; of points on each line is kept, but congruence fails.
;;
;; The figure uses a variant of Moulton's construction: a falling line has
;; its slope multiplied by the bend to the right of the y-axis, and rising
;; lines stay straight. The two triangles, the rays from O, and the
;; crossings X, Y, Z are recomputed from the bent line equations by an Emmy
;; function compiled to a raster kernel. With straight lines the signed
;; area of XYZ is 0. With Moulton lines at bend 0.5 the raster kernel
;; gives about -1.474, because the sides BC and B'C' bend at the axis and
;; Y moves off the line through X and Z.

(w/independence-player)

;; ## A coordinate witness for consistency
;;
;; Ordered pairs of real numbers and equations of straight lines provide a
;; relative consistency argument: any contradiction derived from these
;; geometric rules would also be a contradiction in the real arithmetic
;; used to construct the model. This is not an absolute proof of arithmetic.
;;
;; In the model a point is a pair $(x, y)$, and the line through
;; $P = (x_1, y_1)$ and $Q = (x_2, y_2)$ is the equation
;; $(y_1 - y_2)\,x + (x_2 - x_1)\,y + (x_1 y_2 - x_2 y_1) = 0$. Each row
;; below states an axiom instance, builds it from free symbols, and asks
;; Emmy to simplify the residual. A residual of exactly 0 means the
;; instance holds for every real choice of the coordinates, not only for
;; sampled numbers. The second incidence row multiplies by the determinant
;; of the two lines so that no division is hidden; the parallel rows check
;; that the line through $R$ with the same coefficients as $PQ$ contains $R$
;; and has a vanishing determinant with $PQ$, so the two never meet.

(clerk/table
 {:head ["axiom" "instance" "Emmy residual"]
  :rows (mapv (fn [{:keys [axiom says residual]}] [axiom says (pr-str residual)])
              (coords/checks))})

(clerk/md (if (coords/proved?)
            "All six residuals simplified to 0."
            "A residual did not simplify to 0; the model check failed."))

;; Hilbert makes this argument in Chapter II of the 1899 edition, whose
;; §9 treats the consistency and mutual independence of the axioms: he
;; builds a system of things out of numbers in which every axiom holds.
;; The coordinates above are that construction, restricted to the plane
;; and to the real numbers.
;;
;; Three other pages take the same step from a picture to an invariant
;; structure. Klein's [Erlangen programme](../klein-erlangen/) of 1872
;; reads a geometry off its group of transformations; Hilbert's models are
;; a different instrument, testing which statements a list of axioms can
;; prove. The [non-Euclidean notebook](../non-euclidean/) shows Beltrami's
;; 1868 model, the first relative consistency proof of this kind, which
;; Hilbert's coordinates repeat for the Euclidean case. The
;; [projective deck](/projective/) treats Desargues' theorem as the
;; incidence theorem that lets coordinates be introduced at all: a
;; projective plane in which it holds can be coordinatized by a division
;; ring, and Moulton's plane cannot.
;;
;; ## The Paris lecture (1900)
;;
;; A year later Hilbert presented a list of problems at the International
;; Congress of Mathematicians in Paris. The second asks for a proof of the
;; consistency of arithmetic. The sixth calls for axiomatizing physics.
;; His geometric model and his Paris challenge therefore pull in opposite
;; directions: the former assumes an arithmetic substrate; the latter asks
;; how secure that substrate is.

^{::clerk/no-cache true}
(page/prev-next "notebooks/hilbert_grundlagen.clj")

(lens/section "hilbert-grundlagen")