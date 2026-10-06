;; # Descartes and Fermat: the algebra of curves
;;
;; In 1637 a book appeared in Leiden with a long title: *Discours de la
;; méthode pour bien conduire sa raison, et chercher la vérité dans les
;; sciences*. Its author, René Descartes, was a Frenchman who had lived in
;; the Dutch Republic since 1628. He moved house often and wrote to the
;; world through one friend in Paris, the Minim friar Marin Mersenne. The
;; *Discours* was a preface. Three essays followed it, to show the method at
;; work: the *Dioptrique*, the *Météores*, and the *Géométrie*.
;;
;; At about the same time, in Toulouse, a magistrate of the Parlement named
;; Pierre de Fermat had reached the same idea by another road. He published
;; nothing. He sent short Latin papers to Mersenne, who copied them and
;; passed them around. One of them, *Methodus ad disquirendam maximam et
;; minimam* (a method for finding the greatest and the least), reached
;; Descartes at the start of 1638. Descartes attacked it, Fermat answered,
;; and their friends took sides. The quarrel was about tangents.
;;
;; Both men did the same new thing. They named lines by letters, wrote the
;; relation a curve imposes on them as an equation, and then worked on the
;; equation instead of the figure. The Greeks had the curves: Apollonius
;; wrote eight books on the conic sections around 200 BC, and Pappus of
;; Alexandria collected the open problems around AD 320. What the Greeks
;; lacked was the algebra. This page follows Descartes through his three
;; books, then Fermat through his method, and ends where Newton and Leibniz
;; begin.
;;
;; The French of the *Géométrie* is quoted from the edition of A. Hermann
;; (Paris, 1886; Project Gutenberg eBook 26400). Fermat's Latin is quoted from
;; the *Œuvres de Fermat*, edited by Paul Tannery and Charles Henry, volume I
;; (Paris, 1891). The English under each quotation is this series' own
;; rendering.
^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns descartes-fermat
  {:history/year 1637
   :history/title "Descartes and Fermat: the algebra of curves"
   :history/era "Scientific Revolution"
   :nextjournal.clerk/visibility {:code :hide :result :show}
   :nextjournal.clerk/toc true}
  (:require [alexandria.descartes.geometrie :as g]
            [alexandria.descartes.geometrie-view :as gv]
            [alexandria.fermat.maxima :as fm]
            [alexandria.fermat.maxima-view :as fv]
            [clojure.string :as str]
            [history-of-math.page :as page]
            [nextjournal.clerk :as clerk]))

^{::clerk/visibility {:result :hide}}
(defn badges
  "One badge per graded claim: the claim and how Emmy decided it."
  [graded]
  (clerk/html
   (into [:div {:style {:display "flex" :flex-direction "column" :gap "0.3em" :margin "0.6em 0"}}]
         (for [{:keys [label grade]} graded
               :let [ok? (#{:grade/proved :grade/closed-form :grade/numeric} grade)
                     tone (if ok? "#2f8a3e" "#c0392b")]]
           [:div {:style {:display "inline-block" :padding "0.15em 0.8em" :border-radius "1em"
                          :font-family "ui-monospace, monospace" :font-size "0.82em"
                          :border (str "1px solid " tone) :color tone :width "fit-content"}}
            (str label " (" (name grade) " by Emmy)")]))))

^{::clerk/visibility {:result :hide}}
(defn quote-block
  "A source passage, then the series' English rendering."
  [original english source]
  (clerk/md (str "> *" original "*\n\n"
                 (when english (str "*Our English:* " english "\n\n"))
                 "— " source)))

;; ## Book I: a line times a line is a line
;;
;; Greek geometry kept kinds apart. A line times a line was a rectangle, an
;; area. Three lines made a solid. Four lines made nothing at all. So the
;; Greeks could not write $x^4$, and they could not add a line to an area.
;;
;; Descartes removes the obstacle with one choice. Pick any segment and call
;; it the **unit**. Then a product of two lines can be drawn as a third
;; line, by similar triangles. That is Euclid's proposition VI.12 (to find a
;; fourth proportional). A square root is a line too, by Euclid VI.13 (to
;; find a mean proportional).

(let [{:keys [passage source]} (gv/section :descartes/unit)]
  (quote-block passage
               "Just as all of arithmetic consists of four or five operations, addition, subtraction, multiplication, division and the extraction of roots, so in geometry, to prepare the lines we seek, we need only add or take away other lines; or, having one line that I shall call the unit, to relate it better to numbers, and which can usually be taken at will, and two others, find a fourth that is to one of the two as the other is to the unit, which is the same as multiplication."
               (str "Descartes, *La Géométrie*, Book I (" source ")")))

;; In the player, $BA$ is the unit, $BD = a$ and $BC = b$. Join $A$ to $C$
;; and draw $DE$ parallel to $CA$. The triangles $BAC$ and $BDE$ are similar,
;; so $BE : BD = BC : BA$, and with $BA = 1$ that says $BE = ab$. Move the
;; sliders: the product stays a line. The last two steps take a square root
;; the same way: with the unit $FG$ laid next to $GH = a$, the half-circle
;; on $FH$ cuts the perpendicular at $G$ in $I$, and $GI = \sqrt{a}$.

(gv/unit)

;; Emmy checks the constructions for **every** $a$, $b$ and angle, not for
;; the values on the screen. "Proved" means the difference of the two sides
;; simplifies to exactly $0$.

(badges (:book-1 (g/graded)))

;; ### Book I, figures 3 and 4: every plane problem in one triangle and one circle
;;
;; Descartes next says which problems ruler and compass can solve. He calls
;; them "plane" problems. Once the equation is untangled, at most an unknown
;; square remains, equal to its root times a known line, plus or minus a
;; known square:

(let [{:keys [passage source]} (gv/section :descartes/plane)]
  (quote-block passage
               "There will remain at most an unknown square, equal to what comes from adding or subtracting its root multiplied by some known quantity, and some other quantity also known."
               (str "Descartes, *La Géométrie*, Book I (" source ")")))

;; He then draws every case with one figure. Write $a$ and $b$ for the two
;; known lines.
;;
;; 1. For $z^2 = az + b^2$, make the right triangle $NLM$ with $LM = b$ and
;;    $LN = a/2$. Extend the base $MN$ beyond $N$ to $O$ so that $NO = NL$.
;;    Then $OM = z = \tfrac12 a + \sqrt{\tfrac14 a^2 + b^2}$.
;; 2. For $y^2 = -ay + b^2$, cut $NP = NL$ off the base instead. The rest,
;;    $PM$, is $y$.
;; 3. For $x^4 = -ax^2 + b^2$, the same $PM$ is $x^2$. Its square root is a
;;    line again, by the unit at the start of the book.
;; 4. For $z^2 = az - b^2$ (his figure 4), the circle about $N$ through $M$
;;    meets the parallel $LQR$ in two points, and $LQ$ and $LR$ are both
;;    roots. If the circle misses the line there is no root, "and one may
;;    be sure that the construction of the proposed problem is impossible".
;;
;; Each length is the base of the triangle with $NL$ added or taken away.
;; By Pythagoras the base is $\sqrt{a^2/4 + b^2}$. That is the quadratic
;; formula, drawn instead of written. The sliders change $a$ and $b$. In
;; the last step $b$ grows past $a/2$ and the two roots of figure 4
;; disappear.

(gv/plane)

(badges (:plane (g/graded)))

;; **Reading the original.** The 1637 edition prints figures 3 and 4 as two
;; small woodcuts beside the text (pages 5 to 7 of Hermann's 1886 reprint).
;; Figure 3 is a right triangle with a circle about one corner and its base
;; produced through the circle. Figure 4 is a circle cut by a line parallel
;; to a radius. The player keeps Descartes' letters. Each point comes from a
;; raster kernel, and the sliders move the given lines.


;; ## Pappus' problem of four lines
;;
;; Pappus states the problem in Book VII of his *Collection*. Euclid began
;; it and Apollonius carried it further, Descartes says, but nobody
;; finished it.

(let [{:keys [steps]} (gv/section :descartes/pappus)]
  (quote-block (:french (first steps)) (:claim (first steps))
               "Descartes, *La Géométrie*, Book I (Hermann 1886, p. 9)"))

;; Take four straight lines. From a point $C$, draw a line to each of them,
;; each at its own fixed angle (the "given angles"). Call the four lengths
;; $d_1, d_2, d_3, d_4$. The problem asks for the points $C$ where
;;
;; $$d_1\, d_3 = \lambda\, d_2\, d_4$$
;;
;; for a given ratio $\lambda$. Pappus knew the answer was a conic section,
;; but he did not say which one, or how to draw it.
;;
;; Descartes names $C$ by two lengths, $x$ and $y$. Each $d_i$ is then
;; $x$ and $y$ taken once, plus known amounts: an expression of the
;; **first degree**. A product of two such expressions is of the second
;; degree, so the condition is an equation
;;
;; $$A x^2 + B xy + C y^2 + D x + E y + F = 0,$$
;;
;; and nothing higher. The player draws the four given lines (the sides of a
;; quadrilateral), then lets $C$ run along the locus with its four lines
;; drawn. The readout shows that $d_1 d_3 / d_2 d_4$ stays at $\lambda$. The
;; slider changes $\lambda$.

(gv/pappus)

;; ### Which conic? Apollonius' three symptoms are three signs
;;
;; Descartes reads the type off the sign of one term:

(let [step (some #(when (= :classify (:stage %)) %) (:steps (gv/section :descartes/pappus)))]
  (quote-block (:french step) (:claim step) "Descartes, *La Géométrie*, Book II (Hermann 1886, p. 24)"))

;; In modern terms the test is the **discriminant** $B^2 - 4AC$ of the
;; square terms: zero for a parabola, negative for an ellipse, positive for
;; a hyperbola. This is the pattern behind Apollonius. He named the three
;; curves by a single relation between an ordinate $y$ and the abscissa
;; $x$, which he called the symptom, $y^2 = p x + k x^2$:
;;
;; - $k = 0$: the square on $y$ equals the rectangle $p x$ exactly; this is
;;   *parabolē*, "application";
;; - $k < 0$: it falls short of it; *elleipsis*, "falling short";
;; - $k > 0$: it exceeds it; *hyperbolē*, "excess".
;;
;; For the symptom, $A = -k$, $B = 0$ and $C = 1$, so $B^2 - 4AC = 4k$. The
;; three names are the three signs of one number. Apollonius measured his
;; ordinates at an angle to the diameter, not at a right angle. Changing to
;; such oblique axes multiplies the discriminant by $1/\sin^2\varphi$, which
;; is positive, so the sign, and with it the name, never changes. Emmy
;; proves both facts. It also proves that four lines in **any** position
;; give a second-degree equation, using sixteen symbols for the lines, and
;; it checks Descartes' own numerical example.

(badges (:pappus (g/graded)))

;; For the four lines in the player, the discriminant is itself a quadratic
;; in $\lambda$. Its two roots are the two ratios that give a parabola:

(let [{[lo hi] :roots} (g/parabola-ratios)]
  (clerk/md (format "$\\lambda \\approx %.4f$ and $\\lambda \\approx %.4f$. Between them the locus is an ellipse; outside them it is a hyperbola. The last step of the player sweeps $\\lambda$ across both."
                    lo hi)))

;; ## Book II: the normal, by a double root
;;
;; Book II is about curves in general. Descartes says the most useful
;; problem in geometry is to draw the **normal**, the line that meets a
;; curve at right angles at a chosen point.

(let [{:keys [passage]} (gv/section :descartes/normal)]
  (quote-block passage
               "And I dare say that this is the most useful and the most general problem, not only that I know, but even that I have ever wished to know in geometry."
               "Descartes, *La Géométrie*, Book II (Hermann 1886, p. 33)"))

;; His method needs no limits. Put the centre $P$ of a circle on the axis
;; and let the circle pass through the point $C$ of the curve. In general
;; the circle cuts the curve again, at a second point $E$. Substituting the
;; circle into the curve's equation then gives an equation with two
;; different roots, one for $C$ and one for $E$. Move $P$ until $E$ runs
;; into $C$: the two roots become one **double root**, the circle touches
;; the curve, and $PC$ is the normal.
;;
;; The player uses the parabola $y^2 = x$ and the point $C = (1, 1)$. The
;; two roots are $x = 1$ and $x = 2v - 2$, where $v$ is the distance from
;; the vertex to $P$. They coincide at $v = 3/2$.

(gv/normal)

(badges (:normals (g/graded)))

;; ### Descartes' own example: the ellipse
;;
;; The parabola above is our warm-up. Descartes' first worked example is an
;; ellipse, written the way Apollonius wrote it. Here is his argument, in
;; his order and with his letters (his figure 12):
;;
;; 1. $CM = x$ is the ordinate, $MA = y$ the piece of the diameter, and he
;;    seeks the normal $CP$. He names $PC = s$ and $PA = v$, so $PM = v - y$.
;; 2. The right triangle $PMC$ gives $s^2 = x^2 + v^2 - 2vy + y^2$.
;; 3. The curve is the ellipse with latus rectum $r$ and transverse side
;;    $q$. By the thirteenth theorem of Apollonius' first book,
;;    $x^2 = ry - \tfrac{r}{q}y^2$.
;; 4. He removes $x$:
;;    $y^2 + \dfrac{qry - 2qvy + qv^2 - qs^2}{q - r} = 0$.
;; 5. A wrong $P$ makes the circle cut the ellipse again at $E$ (his figure
;;    15), and the equation has two unequal roots, $MA$ and $QA$. The right
;;    $P$ makes them one.
;; 6. An equation with a double root "has the same form" as $(y - e)^2 =
;;    y^2 - 2ey + e^2$. He compares the second terms,
;;    $\dfrac{qr - 2qv}{q - r} = -2e$.
;; 7. Therefore $v = e - \tfrac{r}{q}e + \tfrac12 r$.

(let [step (some #(when (= :compare (:stage %)) %) (:steps (gv/section :descartes/ellipse)))]
  (quote-block (:french step) (:claim step)
               "Descartes, *La Géométrie*, Book II (Hermann 1886, p. 37; Leiden 1637, p. 345)"))

;; In the player $r = 2$, $q = 4$ and $C$ sits at $MA = e = 1$. The rule
;; gives $v = 1 - \tfrac12 + 1 = \tfrac32$. Watch $QA$ run into $MA$ as $P$
;; slides to $\tfrac32$.

(gv/ellipse)

;; Emmy proves each step for every $r$, $q$, $e$: the elimination, the
;; comparison of terms, the third term (which also fixes $s$), the double
;; root, and finally that $PC$ is at right angles to the ellipse.

(badges (:ellipse (g/graded)))

;; **Reading the original.** Figure 12 of the 1637 edition draws a general
;; curve $CE$ with the axis $GA$, the point $C$, the normal $CP$ and the
;; ordinate $CM$. Figure 15 adds the second meeting point $E$ and its
;; ordinate $EQ$. Descartes leaves the curve unnamed in the woodcut,
;; because the same figure serves all his examples. The player draws his
;; ellipse and the circle from raster kernels, with his letters.


;; ## Book III: the rule of signs
;;
;; Book III is about equations. Descartes builds one from its roots,
;; multiplying $x - 2$, $x - 3$, $x - 4$ and $x + 5$:
;;
;; $$x^4 - 4x^3 - 19x^2 + 106x - 120 = 0.$$
;;
;; He calls $2$, $3$ and $4$ **true** roots. The root $-5$ is **false**: in
;; his words it is "less than nothing", the defect of a quantity $5$. Then
;; he states a rule for counting them from the signs alone.

(let [step (some #(when (= :rule (:stage %)) %) (:steps (gv/section :descartes/signs)))]
  (quote-block (:french step) (:claim step) "Descartes, *La Géométrie*, Book III (Hermann 1886, p. 57)"))

;; Read the signs of the coefficients in order, $+\,-\,-\,+\,-$. The sign
;; changes three times, so there are at most three positive roots. Two
;; neighbours have the same sign once (the two minus signs), so there is at
;; most one negative root. Here both counts are reached exactly. In general
;; the counts are upper bounds that differ from the truth by an even number.
;; Gauss proved this in 1828.

(gv/signs)

(badges (:signs (g/graded)))

;; ## Book III: every equation of the third and fourth degree, by one parabola and one circle
;;
;; Book I solved the plane problems with a circle and a line. Book III asks
;; what remains: equations of the third and fourth degree, the "solid"
;; problems. The Greeks met two of them again and again. One is to find
;; two mean proportionals between two lines, which doubles the cube (the
;; Delian problem). The other is to divide an angle into three equal parts.
;; Menaechmus, around 350 BC, solved the first with two conic sections.
;; Descartes gives one rule that solves every such equation with a single
;; parabola, drawn once, and one circle.

(let [{:keys [passage source]} (gv/section :descartes/construction)]
  (quote-block passage
               "But I shall be content here to give a general rule for finding them all by means of a parabola, because it is in some way the simplest."
               (str "Descartes, *La Géométrie*, Book III (" source ")")))

;; His steps, in his order (his figure 27):
;;
;; 1. Remove the second term, so that the equation reads
;;    $z^4 = \pm p z^2 \pm q z \pm r$ (a cubic is the case $r = 0$,
;;    multiplied by $z$). The unit is $a = 1$.
;; 2. Take the parabola $FAG$ with vertex $A$ and latus rectum $1$. Its axis
;;    carries $C$ with $AC = \tfrac12$.
;; 3. Make $CD = \tfrac12 p$ along the axis and $DE = \tfrac12 q$ at right
;;    angles. $E$ is the centre.
;; 4. The radius is $AE$ when $r = 0$. With $+r$, lay off $AR = r$ and
;;    $AS = 1$, draw the circle on $RS$, and the perpendicular at $A$ meets
;;    it at $H$, with $AH = \sqrt r$. The circle about $E$ passes through
;;    $H$.
;; 5. Wherever this circle meets the parabola at a point $G$, the
;;    perpendicular $GK$ to the axis is a root. Roots on the side of $E$ are
;;    true when $q$ has the sign $+$; the others are false.
;;
;; His proof is three lines of algebra. Name $GK = z$. Then $AK = z^2$,
;; because $GK$ is the mean proportional between $AK$ and the latus rectum
;; $1$. So $EM = DK = z^2 - \tfrac12 p - \tfrac12$ and $GM = z + \tfrac12 q$.
;; By Pythagoras, $GE^2 = EM^2 + GM^2$. But $GE$ is also the radius, and
;; its square is $AE^2 + r$. Setting the two equal gives
;; $z^4 = pz^2 - qz + r$. Emmy proves each line, for every $z$, $p$, $q$,
;; $r$.
;;
;; The player first draws figure 27 for the trisection of an angle, then
;; figure 28 for the two means.

(gv/construction)

(badges (:construction (g/graded)))

;; **Figure 30: the angle in three.** Let the arc have radius $NO = 1$ and
;; chord $NP = q$, and call the chord of a third of the arc $NQ = z$.
;; Descartes draws $QS$ parallel to $TO$, and the similar triangles give
;; $NO : NQ = NQ : QR = QR : RS$. So $QR = z^2$ and $RS = z^3$. The chord
;; $NP$ falls short of three times $NQ$ by exactly $RS$, which gives
;; $q = 3z - z^3$. The circle about $E$ (with $CD = \tfrac32$ and
;; $DE = \tfrac12 q$) meets the parabola in three points besides $A$. The
;; smaller true root $gk$ is the chord sought. The larger, $GK$, is the
;; chord of a third of the rest of the circle. The false root $FL$ equals
;; the two of them together, "as is easy to see by calculation". Emmy does
;; that calculation, with the circle parametrised by rational functions so
;; that no trigonometry is left to simplify:

(badges (:trisection (g/graded)))

;; For the arc of length $2$ in the player, raster finds the three roots by
;; Brent's method on the kernel of $z^4 - 3z^2 + qz$:

(let [{{:keys [gk third-chord GK rest-chord FL]} :trisection {:keys [root]} :mean} (g/book-3-numbers)]
  (clerk/md (format "$gk = %.10f$ against the chord of a third, $%.10f$; $GK = %.10f$ against the chord of a third of the rest, $%.10f$; $FL = %.10f = -(gk + GK)$. For figure 28, the first mean between $1$ and $2$ is $FL = %.10f$, the cube root of $2$, and $LA = FL^2$."
                    gk third-chord GK rest-chord FL root)))

;; **Reading the original.** In the 1637 edition figure 27 shows the
;; parabola with its axis vertical, the circle about $E$ meeting it at $F$
;; and $G$, and the feet $L$ and $K$ on the axis (pages 389 to 397 of the
;; Leiden volume). Figure 30 shows the circle $NQPT$ with the chords and the
;; parallel $QS$ beside the parabola. The player draws the same parabola and
;; circle from raster kernels. The feet it marks are the roots raster
;; finds, so the figure and the numbers agree.


;; ## Fermat: adequality
;;
;; Fermat's *Methodus* is a single page of rule and two examples. The rule
;; is this. Call the unknown $A$ and write the quantity to be made greatest.
;; Write it again with $A + E$ in place of $A$. Then "adequate" the two, a
;; word Fermat takes from Diophantus' Latin translators, where it means
;; "set nearly equal". Take away what is common, divide by $E$, strike out
;; every term that still contains $E$, and solve.

(let [{:keys [rule source]} (fv/section :fermat/rectangle)]
  (quote-block rule
               "Let any term of the question be A ... and, having found the maximum or minimum in terms of A, put the same term again as A + E, and find the maximum or minimum again in terms of A and E. Adequate, as Diophantus says, the two homogeneous expressions equal to the maximum or minimum."
               (str "Fermat, *Methodus ad disquirendam maximam et minimam* (" source ")")))

;; His example: divide a line $B$ at a point so that the rectangle under
;; the two parts is as large as possible. The parts are $A$ and $B - A$, and
;; the rectangle is $BA - A^2$. With $A + E$ it is
;; $BA - A^2 + BE - 2AE - E^2$. Take away what is common:
;; $BE \sim 2AE + E^2$. Divide by $E$: $B \sim 2A + E$. Strike out $E$:
;; $B = 2A$. The line is cut in half.
;;
;; In the player the curve above the line is the rectangle as a function of
;; the cut. The chord from $A$ to $A + E$ has slope $B - 2A - E$, the
;; quotient after dividing by $E$. As $E$ shrinks the chord turns into the
;; tangent, and striking out $E$ leaves its slope, $B - 2A$. At the
;; greatest rectangle that slope is $0$.

(fv/rectangle)

;; Emmy carries out Fermat's steps as written: compare, divide by $E$,
;; strike $E$. It then proves that what remains **is** the derivative, for
;; the rectangle and for any cubic or quartic. In 1684 Leibniz will write
;; that quantity as $d(BA - A^2)/dA$, and the condition for a maximum as
;; setting it to $0$. This series has that page too.

(badges (fm/graded))

;; ### The tangent to the parabola
;;
;; Fermat turns the same rule on tangents. Take the parabola with vertex
;; $D$ and diameter $DC$, a point $B$ on it, and its ordinate $BC$. The
;; tangent at $B$ meets the diameter at $E$. Any other point $O$ of the
;; tangent lies **outside** the parabola. That gives an inequality between
;; ratios, and Fermat adequates it like a maximum. Name $CD = D$ (given),
;; $CE = A$ (sought) and $CI = E$. After dividing by $E$ and striking it,
;; $A = 2D$: the subtangent $CE$ is twice $CD$.

(let [step (last (:steps (fv/section :fermat/tangent)))]
  (quote-block (:latin step) (:claim step) "Fermat, *Methodus* (Tannery and Henry 1891, vol. I, p. 136)"))

(fv/tangent)

;; ## The quarrel, and what came after
;;
;; Mersenne sent Fermat's *Methodus* to Descartes, who received it around
;; January 1638. Descartes said the method was neither general nor sound.
;; He sent back a challenge curve, the "folium" $x^3 + y^3 = 3axy$. Fermat
;; found its tangents at once. Girard Desargues and others were asked to
;; judge, and in the end Descartes conceded that the method was correct.
;; The two methods differ in a way that matters. Descartes' circle needs the
;; whole equation and a double root. Fermat's $E$ needs only the change of
;; one quantity, divided by the change of another. That is a difference
;; quotient.
;;
;; Fermat also never explained why it was legitimate first to divide by
;; $E$ and then to treat $E$ as nothing. Newton's fluxions (written in
;; 1665–1671) and Leibniz's differentials (published in 1684) gave the
;; procedure a calculus and a notation, and the next pages of this series
;; follow them. The justification waited for Cauchy's limits in 1821.

^{::clerk/no-cache true}
(page/prev-next "notebooks/descartes_fermat.clj")
