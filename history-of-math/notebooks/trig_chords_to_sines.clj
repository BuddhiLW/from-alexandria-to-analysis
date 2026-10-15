;; # From chords to sines: Ptolemy, Aryabhata, Regiomontanus
^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns trig-chords-to-sines
  {:history/year 1533
   :history/title "Regiomontanus, De triangulis omnimodis: from chords to sines"
   :history/era "Renaissance trigonometry"
   :history/fields #{:trigonometry :geometry}
   :nextjournal.clerk/visibility {:code :hide :result :show}
   :nextjournal.clerk/toc true}
  (:require [alexandria.regiomontanus.widgets :as w]
            [history-of-math.lens :as lens]
            [history-of-math.page :as page]
            [history-of-math.widgets :as hom]
            [nextjournal.clerk :as clerk]))

^{::clerk/visibility {:code :hide :result :hide}}
(hom/install!)

;; This is the first notebook of the trigonometry strand. It follows one
;; line segment across thirteen centuries. Ptolemy (Alexandria, c. 150)
;; tabulated the *chord* of an arc. Aryabhata (Kusumapura, 499) halved it
;; and tabulated the *half-chord* of the half arc, the *jya*; through the
;; Arabic *jiba* it became the Latin *sinus*. Johannes Müller of
;; Königsberg, called Regiomontanus, wrote *De triangulis omnimodis libri
;; quinque* in 1464. It was printed at Nuremberg by Petreius in 1533, and
;; it made the sine the tool for solving every triangle, plane and
;; spherical, apart from astronomy.
;;
;; Our texts are Halma's French Almagest (1813) for Ptolemy, W. E. Clark's
;; English Aryabhatiya (Chicago, 1930) for Aryabhata, and the 1533 Latin
;; edition of Regiomontanus (archive.org). The Latin and French are
;; quoted with our own English beside them, marked as translations.

;; ## From chord to half-chord
;;
;; One figure, one sentence per step: the arc, Ptolemy's chord,
;; Aryabhata's half-chord on his radius of minutes, and Regiomontanus' word
;; *sinus*.

(w/steps)

;; In today's notation the two tables measure one segment:
;;
;; $$\operatorname{crd}(2a) = 2R\sin a,\qquad \operatorname{jya}(a) = \tfrac12\operatorname{crd}(2a) = R\sin a,$$
;;
;; with Ptolemy's $R = 60$ parts and Aryabhata's $R = 3438'$, the radius
;; counted in minutes of its own circumference ($21600'/2\pi \approx 3437.75'$).
;; Drag $P$ or $Q$, or move any of the four tied controls. The others are
;; solved from the one you touch, both ways, and the value is printed in
;; Ptolemy's sexagesimal parts and in Aryabhata's minutes.

(w/half-chord)

;; Emmy proves each relation the widget uses with the letters left as
;; symbols, including forward-then-inverse ($120\sin\arcsin(k/120) = k$)
;; for both unit systems. Raster checks that every source of the tie
;; solves back to the same state:

(w/sines-derivation)

;; ## Aryabhata's table by differences
;;
;; The *Aryabhatiya* gives its sine table in one verse of the Dasagitika
;; (I, 10). The verse lists 24 differences, 225, 224, 222, 219, ... 22, 7
;; minutes, for the arcs $3^\circ 45'$, $7^\circ 30'$, ... $90^\circ$,
;; written in his alphabetic numerals. Their sum is 3438, the radius. The
;; rule that produces them is stated in the Ganitapada (II, 12), in Clark's
;; reading: each difference is the previous one
;; diminished by the quotient of the sines so far divided by the first
;; sine. Clark's page is not in our transcription of the text, so we
;; paraphrase it here and do not quote it.
;;
;; The same Ganitapada is a practical book. Its next verse tells how the
;; figures themselves are made:

(page/blockquote
 "The circle is made by turning, and the triangle and the quadrilateral by means of a karna; the horizontal is determined by water, and the perpendicular by the plumb-line."
 "Aryabhata, Aryabhatiya II.13, tr. W. E. Clark (Chicago, 1930), p. 30")

;; Build the table one sine at a time. With his divisor 225 the rule drifts
;; below the sine, while his printed numbers do not, so he corrected as he
;; went. With the exact divisor $1/(2 - 2\cos 3^\circ 45') = 233.5$ the rule
;; is the second-difference identity
;; $s_{n+1} - 2s_n + s_{n-1} = -(2-2\cos h)\,s_n$, which Emmy proved above,
;; and it lands on $3438\sin$ to the last digit.

(w/aryabhata)

;; ## Regiomontanus: the law of sines (De triangulis II.1)
;;
;; The second book opens with the theorem that sides are as the sines of
;; the opposite angles:

(page/blockquote
 "In omni triangulo rectilineo proportio lateris ad latus est, tanquam sinus recti anguli alterum eorum respicientis, ad sinum rectum anguli reliquum latus respicientis."
 "Regiomontanus, De triangulis omnimodis II.1 (Nuremberg, 1533), p. 46. Our translation: \"In every rectilinear triangle the ratio of side to side is as the right sine of the angle facing the one to the right sine of the angle facing the other.\"")

;; He proves it by making $BD$ equal to $AG$ along the side $AB$ and
;; drawing two equal circles about $B$ and $G$. The perpendiculars from
;; $A$ and $D$ are the sines of the two angles, and Euclid VI.4 gives the
;; proportion:

(page/blockquote
 "Est autem per 4. sexti Euclidis proportio a b ad b d, et ideo ad a g sicut a k ad d h, quare certum est quod asserebat propositio."
 "Regiomontanus, De triangulis II.1 (1533), p. 47. Our translation: \"By Euclid VI.4 the ratio of ab to bd, and so to ag, is as ak to dh; so what the proposition asserted is certain.\"")

;; The modern reading puts the triangle in its circumscribed circle. The
;; side $a$ subtends an inscribed angle $A$ and a central angle $2A$, so
;; $a$ is the *chord* of $2A$: Ptolemy's $\operatorname{crd} 2A = 2R\sin A$.
;; The law of sines is the chord table read three times:
;;
;; $$\frac{a}{\sin A} = \frac{b}{\sin B} = \frac{c}{\sin C} = 2R.$$
;;
;; Drag a vertex round the circle, or move any control. The ratios are
;; measured on the drawn triangle by raster, and the widget names the
;; triangle as acute, right or obtuse.

(w/law-of-sines)

(w/triangles-derivation)

;; ## Solving a triangle
;;
;; Book I solves right triangles and Book II all the others. Given any
;; three independent parts the rest follow, through the law of sines or
;; the law of cosines. One case is ambiguous: two sides and an angle
;; opposite one of them (SSA), because $\sin B = \sin(180^\circ - B)$.
;; Choose what is given and the widget names the case. In SSA you get two
;; triangles, one, or none.

(w/solver)

;; ## What came after
;;
;; Rheticus and Pitiscus (*Trigonometria*, 1595, which named the subject)
;; tabulated all six functions to ten and more places. Napier's logarithms
;; (1614) were invented to multiply sines. Euler's *Introductio* (1748)
;; made the sine a function of a number rather than a line in a circle of
;; given radius, the radius 1 that we use today. The law of sines also
;; holds on the sphere (Regiomontanus' Book IV) and in the hyperbolic plane,
;; with $\sin$ of the sides replaced by $\sin(a/R)$ and $\sinh a$: see
;; [non-Euclidean geometry](../non-euclidean/).

^{::clerk/no-cache true}
(lens/section "trig-chords-to-sines")

^{::clerk/no-cache true}
(page/prev-next "notebooks/trig_chords_to_sines.clj")
