;; # Euclid, *Elements*: geometry from first principles
;;
;; About 300 BC, in Alexandria, the new Greek city on the Nile delta,
;; Euclid wrote the *Elements*. Ptolemy I had founded the Museum there, a
;; research institute with a library that tried to own every Greek book.
;; Euclid taught in it. Before him, Hippocrates of Chios, Eudoxus and
;; Theaetetus had proved many of the theorems; Euclid's work was to put them
;; in one order, each proved from what came before.
;;
;; The *Elements* has thirteen books. Book I starts from twenty-three
;; definitions, five postulates and five common notions, and ends at I.47,
;; Pythagoras' theorem. Book V is Eudoxus' theory of ratios, which lets
;; geometry compare lengths that no whole numbers measure. Book XII measures
;; circles and pyramids by exhaustion, the method Archimedes will push much
;; further a generation later. For two thousand years this book was what
;; "proof" meant.
;;
;; The text is T. L. Heath's translation, *The Thirteen Books of Euclid's
;; Elements* (1908). Statements are read from Alexandria's shelf of Euclid
;; (`alexandria/euclid/elements_1.edn`).
^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns euclid-elements
  {:history/year -300
   :history/title "Euclid, Elements"
   :history/era "Greek geometry"
   :nextjournal.clerk/visibility {:code :hide :result :show}
   :nextjournal.clerk/toc true}
  (:require [alexandria.library :as library]
            [alexandria.euclid.elements :as elements]
            [alexandria.euclid.elements-view :as elements-view]
            [alexandria.proofs :as proofs]
            [alexandria.notebooks.euclid-i1-surfaces :as surfaces]
            [hive-dsl.result :as r]
            [history-of-math.page :as page]
            [nextjournal.clerk :as clerk]))

^{::clerk/visibility {:result :hide}}
(defn statement
  "Heath's statement of proposition id, from the shelf, as a quotation."
  [id]
  (let [res (library/proposition id)]
    (clerk/md (if (r/ok? res)
                (str "> *" (get-in res [:ok :statement]) "*\n>\n> — " (get-in res [:ok :source]))
                (str "**Shelf unavailable:** `" (pr-str (:error res)) "`")))))

;; ## The foundations
;;
;; ### Definitions (a selection)
;;
;; - **Def. 1.** A *point* is that which has no part.
;; - **Def. 2.** A *line* is breadthless length.
;; - **Def. 4.** A *straight line* is a line which lies evenly with the points
;;   on itself.
;; - **Def. 15.** A *circle* is a plane figure contained by one line such that
;;   all the straight lines falling upon it from one point among those lying
;;   within the figure are equal to one another;
;; - **Def. 16.** and the point is called the *centre* of the circle.
;; - **Def. 20.** Of trilateral figures, an *equilateral triangle* is that
;;   which has its three sides equal.
;;
;; ### Postulates
;;
;; Let the following be postulated:
;;
;; 1. To draw a straight line from any point to any point.
;; 2. To produce a finite straight line continuously in a straight line.
;; 3. To describe a circle with any centre and distance.
;; 4. That all right angles are equal to one another.
;; 5. That, if a straight line falling on two straight lines make the interior
;;    angles on the same side less than two right angles, the two straight
;;    lines, if produced indefinitely, meet on that side on which are the
;;    angles less than the two right angles.
;;
;; The first three are the *ruler and compass*: they say what may be drawn.
;; The fifth, the parallel postulate, is the famous one. For two thousand
;; years geometers tried to prove it from the others. Bolyai and
;; Lobachevsky showed in the 1830s that it cannot be done, by building
;; geometries in which it fails, and Riemann's lecture of 1854, later in this
;; series, made such geometries a matter of curvature.
;;
;; ### Common notions
;;
;; 1. Things which are equal to the same thing are also equal to one another.
;; 2. If equals be added to equals, the wholes are equal.
;; 3. If equals be subtracted from equals, the remainders are equal.
;; 4. Things which coincide with one another are equal to one another.
;; 5. The whole is greater than the part.
;;
;; The postulates are about geometry. The common notions are about equality
;; and magnitude of any kind.

;; ## Proposition I.1

(statement :euclid/I.1)

;; Let $AB$ be the given finite straight line.

(page/steps
 [["With centre $A$ and distance $AB$ let the circle $BCD$ be described" "Post. 3"]
  ["With centre $B$ and distance $BA$ let the circle $ACE$ be described" "Post. 3"]
  ["From the point $C$, in which the circles cut one another, join $CA$ and $CB$" "Post. 1"]
  ["$AC = AB$, since $A$ is the centre of the circle $CDB$" "Def. 15"]
  ["$BC = BA$, since $B$ is the centre of the circle $CAE$" "Def. 15"]
  ["$CA = CB$, since each is equal to $AB$" "C.N. 1"]
  ["The triangle $ABC$ is equilateral, and it has been constructed on $AB$. *Being what it was required to do.*" "Def. 20"]])

;; Play the construction. Each circle is drawn the way Post. 3 describes it:
;; a radius swept once around its centre. The moving points are computed by
;; a raster kernel in the browser, and the given points by the board's
;; raster frame.

(elements-view/player :euclid/I.1)

;; **The gap.** Step 3 says "the point $C$, in which the circles cut one
;; another". Nothing in the postulates says two circles must cut at all;
;; that they do is taken from the figure. Hilbert's *Foundations of
;; Geometry* (1899) closed the gap with an axiom of continuity, the same
;; kind of repair Dedekind made to the numbers in 1872.

;; ### The same construction on three surfaces
;;
;; Euclid's construction uses only "circle", "meet" and "straight line".
;; Read those words on another surface and the same steps still run. On a
;; sphere the straight lines are great circles; in the Poincaré disk, a model
;; of Bolyai's and Lobachevsky's plane, they are arcs meeting the rim at
;; right angles. Below, Alexandria carries I.1 out on all three. The
;; triangle is equilateral on each. What changes is the sum of its angles:
;; more than two right angles ($\pi$) on the sphere, exactly $\pi$ in the
;; plane, less in the disk. The excess equals the integral of the curvature
;; $K$ over the triangle, $\iint K\,dA$, the Gauss–Bonnet theorem; Gauss
;; reached its first form in 1827, near the end of this series.

(clerk/table
 {:head ["surface" "sides AB, BC, CA" "angle sum" "angle sum − π" "∫∫ K dA"]
  :rows (mapv surfaces/summary-row surfaces/constructions)})

(clerk/row (mapv surfaces/figure surfaces/constructions))

(clerk/html
 (into [:div]
       (for [{:keys [title lengths excess curvature-integral]} surfaces/constructions
             :let [{:keys [AB BC CA]} lengths
                   eq? (and (surfaces/close? AB BC 1.0e-5) (surfaces/close? AB CA 1.0e-5))
                   gb? (surfaces/close? excess curvature-integral 1.0e-3)]]
         [:div (:nextjournal/value
                (page/badge (and eq? gb?)
                            (str title ": AB = BC = CA, and angle excess = ∫∫ K dA (numeric)")))])))

;; ## Proposition I.2

(statement :euclid/I.2)

;; Euclid's compass closes when it is lifted, so he cannot simply carry
;; $BC$ over to $A$. This proposition shows how to do it anyway, with I.1 as
;; its first tool.

(page/steps
 [["From the point $A$ to the point $B$ let the straight line $AB$ be joined" "Post. 1"]
  ["On it let the equilateral triangle $DAB$ be constructed" "I.1"]
  ["Let the straight lines $AE$, $BF$ be produced in a straight line with $DA$, $DB$" "Post. 2"]
  ["With centre $B$ and distance $BC$ let the circle $CGH$ be described, cutting $BF$ at $G$" "Post. 3"]
  ["With centre $D$ and distance $DG$ let the circle $GKL$ be described, cutting $AE$ at $L$" "Post. 3"]
  ["$DL = DG$, since $D$ is the centre of the circle $GKL$" "Def. 15"]
  ["$DA = DB$, so the remainder $AL$ is equal to the remainder $BG$" "C.N. 3"]
  ["$BC = BG$, since $B$ is the centre of the circle $CGH$" "Def. 15"]
  ["$AL$ and $BC$ are each equal to $BG$, so $AL = BC$. *Being what it was required to do.*" "C.N. 1"]])

;; ## Proposition I.3

(statement :euclid/I.3)

;; The first step *is* I.2, and I.2 contains I.1. That nesting is what
;; "proved from what came before" looks like.

(page/steps
 [["At the point $A$ let $AD$ be placed equal to the less straight line" "I.2"]
  ["With centre $A$ and distance $AD$ let the circle $DEF$ be described, cutting $AB$ at $E$" "Post. 3"]
  ["$AE = AD$, since $A$ is the centre of the circle $DEF$" "Def. 15"]
  ["The less is equal to $AD$, so $AE$ is equal to it: from the greater $AB$ there has been cut off $AE$ equal to the less. *Being what it was required to do.*" "C.N. 1"]])

;; ## Propositions I.4 to I.6
;;
;; Below each statement: Heath's proof as steps, then the checks Alexandria
;; runs on the construction from the shelf. A check marked *proved* is an
;; Emmy simplification to 0 with the given points left as symbols, so it
;; holds for every figure. A check marked *numeric* is measured on one
;; figure by the board's raster kernel.

^{::clerk/visibility {:result :hide}}
(defn proposition [id]
  (clerk/col
   (statement id)
   (page/steps (proofs/steps elements/proofs-resource id))
   (page/grade-badges (elements/graded id) #{:grade/proved :grade/numeric})))

;; ### Proposition I.4
;;
;; Side, angle, side: the first test for congruent triangles. Euclid proves
;; it by laying one triangle on the other. No postulate says a figure may be
;; moved without change; Hilbert made side-angle-side an axiom in 1899.

(proposition :euclid/I.4)

;; ### Proposition I.5
;;
;; The *pons asinorum*, the bridge of asses: the angles at the base of an
;; isosceles triangle are equal.

(proposition :euclid/I.5)

;; ### Proposition I.6
;;
;; The converse of I.5, and Euclid's first proof by contradiction.

(proposition :euclid/I.6)

;; ## Propositions I.47 and I.48: Pythagoras
;;
;; Book I ends here. Euclid draws squares on the three sides of a right
;; triangle, drops $AL$ from the right angle, and shows each half of the
;; big square equal to one small square. Each half is reached by a shear:
;; a triangle slid along a parallel keeps its area (I.41). Emmy proves every
;; area claim with the triangle's corner and the ratio of its legs left as
;; symbols.

(proposition :euclid/I.47)

;; The windmill, played. The square on $BA$ slides along $AC$ without
;; changing its area (I.41). It then turns a right angle about $B$, which
;; carries the triangle $FBC$ onto $ABD$ (I.4). Last, it slides along $AL$
;; into the rectangle $BL$. The square on $AC$ does the same about $C$ and
;; becomes $CL$. The two rectangles fill the square on $BC$. Every moving
;; corner is computed by a raster kernel.

(elements-view/player :euclid/I.47)

;; I.48 is the converse: if the squares add up, the angle is right.

(proposition :euclid/I.48)

;; ## Where this goes
;;
;; Book I ends at I.47, Pythagoras' theorem, and I.48, its converse. Every
;; step on the way is built like these three: a construction from the
;; postulates, a proof from the common notions and what came before.
;;
;; Euclid's successors in Alexandria used the *Elements* as their toolbox.
;; Archimedes, the next notebook, cites Book VI on angle bisectors and Book
;; XII's exhaustion to trap the circle between polygons. Apollonius builds
;; the conic sections on Books I to VI. And Book V, Eudoxus' ratios, waits
;; until 1872, when Dedekind defines the real numbers by the same test.

^{::clerk/no-cache true}
(page/prev-next "notebooks/euclid_elements.clj")
