;; # Ptolemy, *Almagest*: a table of chords and the wandering stars
^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns ptolemy-almagest
  {:history/year 150
   :history/title "Ptolemy, Almagest"
   :history/era "Hellenistic"
   :history/fields #{:trigonometry :mechanics-physics :geometry}
   :nextjournal.clerk/visibility {:code :hide :result :show}
   :nextjournal.clerk/toc true}
  (:require [alexandria.ptolemy.chords-view :as chords]
            [alexandria.ptolemy.epicycle-widget :as epicycle]
            [alexandria.ptolemy.theorem-widget :as theorem]
            [history-of-math.lens :as lens]
            [history-of-math.page :as page]
            [history-of-math.widgets :as hom]
            [nextjournal.clerk :as clerk]))

^{::clerk/visibility {:code :hide :result :hide}}
(hom/install!)

;; Claudius Ptolemy worked at Alexandria under Hadrian and Antoninus Pius;
;; his dated observations run from 127 to 141 CE. His *Mathematical
;; Composition* in thirteen books, later called by the Arabic name
;; *Almagest* ("the greatest"), gathered Hipparchus' astronomy and his own
;; into one deductive system. It held the field for fourteen centuries, until
;; Copernicus rearranged the same circles about the Sun.
;;
;; We read him in N. Halma's French (*Composition mathématique de Claude
;; Ptolémée*, Paris, 1813, on fr.wikisource), with J. L. Heiberg's Greek
;; (Leipzig, 1898-1903) and K. Manitius' German (Leipzig, 1912-13). Halma
;; numbers the chapter on chords IX; in Heiberg's division it is I.10, with
;; the table as I.11.

;; ## The table of chords (I.10-11)
;;
;; Ptolemy has no sine. He measures the straight line that subtends an arc,
;; its *chord*, in parts of which the diameter has 120, and writes the
;; parts in base 60:

(page/blockquote
 "Nous emploierons en général la numération sexagésimale, pour éviter l’embarras des fractions."
 "Ptolemy, Almagest I.10, tr. Halma (1813), ch. IX, p. 26 (\"We shall use the sexagesimal numeration throughout, to avoid the nuisance of fractions\")")

;; In today's notation, with radius $R = 60$ parts, the chord of an arc
;; $2a$ is
;;
;; $$\operatorname{crd}(2a) = 2R\sin a = 120\sin a,$$
;;
;; so his table is a table of sines in disguise. He starts from the
;; regular polygons Euclid inscribes (the side of the decagon from the
;; golden section, XIII.9-10) and from three theorems. The table below sets
;; each value he states in the text beside $120\sin(a/2)$ computed by
;; raster; the last column is his error in seconds of a part. The largest
;; is under one second: one part in 400 000 of the diameter.

(chords/table)

;; Emmy proves each step he uses, with the letters left as symbols; raster
;; checks his numbers:

(page/grade-badges (chords/grades) #{:grade/proved :grade/numeric})

;; ### The chord of half an arc
;;
;; From the chord of 12° (pentagon minus hexagon) he halves to 6°, 3°,
;; 1½° and ¾°. One of his sentences per step; the arc $GB$ is yours to
;; move under the figure, and every length is computed by raster from
;; Ptolemy's construction, never from the sine.

(chords/half)

;; ### The chord of the sum of two arcs
;;
;; Given the chords of two arcs, the chord of their sum follows from the
;; lemma on the inscribed quadrilateral (next section), applied to $BGDE$.
;; Move both arcs: $AG$ is found from $GD$, the chord of the supplement.

(chords/sum)

;; With halving, sums and differences Ptolemy reaches every multiple of
;; ¾°. The chord of 1° is not among them: he squeezes it between the
;; bounds $\tfrac43 \operatorname{crd}\tfrac34°$ and
;; $\tfrac23 \operatorname{crd}1\tfrac12°$, both $1^p\,2'\,50''$ to the second,
;; and the table follows in half degrees.

;; ## Ptolemy's theorem
;;
;; The lemma that carries the whole table is the theorem that bears his
;; name:

(page/blockquote
 "Soit un quadrilatère quelconque inscrit dans le cercle ABGD ; soient menées les diagonales AG, BD il s’agit de prouver que le rectangle construit sur AG et BD, est égal aux deux rectangles des côtés opposés AB GD, et AD BG."
 "Ptolemy, Almagest I.10, tr. Halma (1813), ch. IX, p. 29 (\"Let any quadrilateral be inscribed in the circle ABGD, with diagonals AG, BD: to prove that the rectangle on AG and BD equals the two rectangles on the opposite sides AB, GD and AD, BG\")")

;; He proves it by cutting off the angle $ABE$ equal to $DBG$: two pairs of
;; similar triangles give $BG\cdot AD = BD\cdot GE$ and
;; $BA\cdot DG = BD\cdot AE$, and the two add up to $AG\cdot BD$. With our
;; letters $C$ for his $G$:
;;
;; $$AC\cdot BD = AB\cdot CD + AD\cdot BC.$$
;;
;; Below, $A$, $B$, $C$ are fixed on a circle and $D$ moves. The arc
;; $d = CD$ and the three chords that depend on it are tied: move any one
;; and the others follow, and both sides of the relation are recomputed
;; with their numbers.

(theorem/tied)

(theorem/derivation)

;; When the quadrilateral is a rectangle the diagonals are diameters, and the
;; theorem becomes $AC^2 = AB^2 + BC^2$: Pythagoras, as a special case.
;;
;; Now try it yourself: all four points are free. Drag them anywhere. With
;; the box ticked, a dragged point stays on the circle through the other
;; three; untick it and pull $D$ off the circle, and the equality breaks
;; into Ptolemy's *inequality*, $AC\cdot BD < AB\cdot CD + AD\cdot BC$,
;; which holds for any four points in the plane.

(theorem/playground)

;; ## The wandering stars: epicycle and deferent
;;
;; Mars, Jupiter and Saturn sometimes stop among the stars, run backwards
;; for weeks, stop again and resume. Following Apollonius and Hipparchus,
;; Ptolemy carries the planet on a small circle, the *epicycle*, whose centre
;; runs on a large one, the *deferent*. Book XII names the point where the
;; planet seems to stand still:

(page/blockquote
 "so bezeichnet der von der also gezogenen Geraden auf dem erdnahen Bogen des Epizykels erzeugte Punkt die Grenze zwischen Rechtläufigkeit und Rückläufigkeit, d. h. der Planet wird, in diesem Punkte angelangt, den Eindruck eines scheinbaren Stillstandes machen."
 "Ptolemy, Almagest XII.1, tr. Manitius (1913), vol. 2, p. 268 (Heiberg II, p. 451) (\"that point on the near arc of the epicycle is the limit between direct and retrograde motion: arrived there, the planet gives the impression of standing still\")")

;; Write $R$ and $r$ for the radii and $w_1$, $w_2$ for the turns per year
;; of the deferent and of the epicycle. Emmy derives the planet's apparent
;; angular speed seen from the Earth, and the retrograde condition at the
;; nearest point:

(epicycle/derivation)

;; The playground INTEGRATES the motion: one RK4 step is a raster kernel,
;; iterated in the page over the years you choose, and the longitude seen
;; from the Earth is carried along as a fifth coordinate. Where its rate
;; turns negative the path is red: a retrograde arc, named live with its
;; stations, its width in degrees and its length in days. The closed form
;; is drawn faintly beside the integration, and the largest gap between
;; them is printed. Drag the planet in the top view to change $r$; drag
;; the grey point to change $R$; drag time on the rate plot. The presets
;; load Ptolemy's own epicycles (Mars 39;30, Jupiter 11;30, Saturn 6;30
;; parts of 60) and two experiments that break the loops.

(epicycle/playground)

;; ## What came after
;;
;; Ptolemy's chords became sines in India (Aryabhata, c. 500) and in
;; Baghdad (al-Battani, c. 900), where half the chord on half the arc was
;; tabulated directly. His epicycles survived intact into
;; Copernicus (*De revolutionibus*, 1543, a notebook still to come), who moved the Earth and found that
;; each outer planet's epicycle was the Earth's own orbit seen from the
;; other side; then [Kepler](../kepler-mysterium/) replaced the circles by
;; Apollonius' ellipses. An epicycle on a deferent is two terms of a
;; Fourier series, $R e^{i w_1 t} + r e^{i w_2 t}$; add enough of them and
;; any closed curve can be traced.

^{::clerk/no-cache true}
(lens/section "ptolemy-almagest")

^{::clerk/no-cache true}
(page/prev-next "notebooks/ptolemy_almagest.clj")
