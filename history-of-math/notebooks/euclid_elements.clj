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
   :history/fields #{:geometry :number-theory}
   :nextjournal.clerk/visibility {:code :hide :result :show}
   :nextjournal.clerk/toc true}
  (:require [alexandria.library :as library]
            [alexandria.apollonius.areas-steps-view :as areas-steps]
            [alexandria.euclid.elements :as elements]
            [alexandria.euclid.elements-view :as elements-view]
            [alexandria.euclid.playgrounds-view :as play]
            [alexandria.euclid.ties-view :as ties]
            [alexandria.proofs :as proofs]
            [alexandria.notebooks.euclid-i1-surfaces :as surfaces]
            [hive-dsl.result :as r]
            [history-of-math.page :as page]
            [history-of-math.widgets :as hom]
            [nextjournal.clerk :as clerk]))

^{::clerk/visibility {:code :hide :result :hide}}
(hom/install!)
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

;; Heath's proof, one sentence to a step: the figure beside it adds what
;; each sentence introduces and nothing more. Each circle is drawn the way
;; Post. 3 describes it, a radius swept once around its centre. Then each
;; radius is marked in its own circle, since Def. 15 is used twice, once for
;; each centre. Click a sentence to replay it. The moving points come from
;; a raster kernel in the browser, the given points from the board's raster
;; frame.

(elements-view/player :euclid/I.1)

;; Now try it yourself: drag both endpoints and watch both circles meet.
(play/i1)

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

;; Now try it yourself: all three corners are free. Drag $B$ and watch the
;; windmill. The parallel $AL$ still cuts the square on $BC$ into the
;; rectangles $BL$ and $CL$, but they match the squares on $AB$ and $AC$
;; only while the angle at $A$ is right. Open the angle and Book II takes
;; over: the perpendicular $BD$ falls outside $CA$ (II.12) or within it
;; (II.13), and the rectangle $CA \cdot AD$ is what is gained or lost.
;; Switch the layers off to see one construction at a time.

(play/i47)

;; I.48 is the converse: if the squares add up, the angle is right.

(proposition :euclid/I.48)

;; ### Move the triangle: I.47, II.12, II.13
;;
;; Book II says what happens when the angle at $A$ is not right. Drop the
;; perpendicular $BD$ from $B$ onto $CA$. Heath's II.12: "In obtuse-angled
;; triangles the square on the side subtending the obtuse angle is greater
;; than the squares on the sides containing the obtuse angle by twice the
;; rectangle contained by one of the sides about the obtuse angle, namely
;; that on which the perpendicular falls, and the straight line cut off
;; outside by the perpendicular towards the obtuse angle." II.13 says the
;; same for an acute angle, with "less" and "within".
;;
;; Write $a = BC$, $b = CA$, $c = AB$ and $d = AD$, signed: $d$ is negative
;; when $D$ falls outside, on $CA$ produced. Emmy puts $B$ at $(d, h)$ and
;; eliminates $h$. The three propositions are then one equation, read for
;; $d = 0$, $d < 0$ and $d > 0$:

(ties/derivation :pythagoras)

;; Below, every one of $a$, $b$, $c$ and the angle $A$ is a control, and $B$
;; can be dragged. They are tied by that equation, and each move names what
;; holds still. If $b$, $c$ or $A$ moves, $a$ is solved forward. If $a$
;; moves, $b$ and $A$ hold and $c$ is solved back by the inverse. If $B$ is
;; dragged, $b$ holds and $c$ and $A$ are read off $B$. The rectangle
;; $CA \cdot AD$ appears when the angle is not right. It is the excess
;; (II.12, solid) or the defect (II.13, dashed), drawn inside the square on
;; $CA$. The name above the formula changes as the triangle stops being
;; right. Every number comes from raster kernels.

(ties/pythagoras)

;; ## Applying an area: I.44 and VI.28-29
;;
;; I.44 asks for a parallelogram of given area on a given line: "To a given
;; straight line to apply, in a given rectilineal angle, a parallelogram
;; equal to a given triangle." VI.28 and VI.29 apply it so that it *falls
;; short* of the line, or *runs past* it, by a figure of given shape.
;; Proclus, after Eudemus, says the Pythagoreans named the three cases
;; *parabolē* (application), *hyperbolē* (exceeding) and *elleipsis*
;; (falling short). Apollonius, about a century after Euclid, gave these
;; names to the three conic sections.
;;
;; The figure below is the one the Apollonius notebook uses. It is stepped
;; one sentence at a time, from I.44 through VI.27-29 to the three curves,
;; and it runs on the same raster kernels.

(areas-steps/figure)

(areas-steps/identities)

;; One relation covers all three cases. On the line $AB = a$, apply a
;; rectangle of height $y$ whose base $AX$ is $a + k\,y$. The leftover $XB$
;; is $|k|\,y$ wide and $y$ high, so its shape is fixed by $k$. It is a
;; defect when $k < 0$ (VI.28; $k = -1$ makes it a square), nothing when
;; $k = 0$ (I.44), and an excess when $k > 0$ (VI.29). Emmy turns
;; $S = (a + k\,y)\,y$ into a quadratic in $y$, checks its root, and
;; completes the square for VI.27: falling short, $S$ can never pass
;; $a^2/4|k|$.

(ties/derivation :application)

;; Every one of $a$, $S$, $y$ and $k$ is a control. If $y$, $a$ or $k$
;; moves, the others hold and $S$ is solved forward. If $S$ moves, $a$ and
;; $k$ hold and $y$ is solved back, on the nearer root. Falling short, $S$
;; stops at VI.27's greatest value. Move $k$ through $0$ to watch the name
;; change from *elleipsis* to *parabolē* to *hyperbolē*.

(ties/application)

;; Now try it yourself, with nothing held. Euclid applies in a *given
;; angle*, so the parallelogram can lean: drag $\theta$. Drag $B$ to change
;; the line, $X$ to move the end of the base, $A'$ to change the height,
;; and set the given area $S_0$ that VI.28 asks for. The thin outline is
;; the parallelogram that applies exactly $S_0$. Falling short, it exists
;; only while $S_0$ is no greater than VI.27's greatest, drawn dashed:
;; Euclid's *diorismos*, the condition he states with VI.28. Ask for more
;; and the badge turns red.

(play/application)

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
