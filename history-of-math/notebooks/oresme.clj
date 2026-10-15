;; # Oresme, *On the configurations of qualities*: the figure of a motion
^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns oresme
  {:history/year 1350
   :history/title "Oresme, De configurationibus qualitatum et motuum"
   :history/era "Medieval"
   :history/fields #{:analysis :mechanics-physics}
   :nextjournal.clerk/visibility {:code :hide :result :show}
   :nextjournal.clerk/toc true}
  (:require [alexandria.oresme.latitude-widget :as latitude]
            [history-of-math.lens :as lens]
            [history-of-math.page :as page]
            [history-of-math.widgets :as hom]
            [nextjournal.clerk :as clerk]
            [alexandria.oresme.mean-widget :as mean]
            [alexandria.oresme.series-widget :as series]))

^{::clerk/visibility {:code :hide :result :hide}}
(hom/install!)

;; Nicole Oresme (c. 1320-1382) studied and taught at the College of
;; Navarre in Paris, translated Aristotle into French for King Charles V,
;; and died bishop of Lisieux. Around 1350 he wrote the *Tractatus de
;; configurationibus qualitatum et motuum*, the treatise on the
;; configurations of qualities and motions, in three parts. Its one idea
;; reached further than anything else he wrote: draw a changing quantity.
;;
;; A shorter school version, *De latitudinibus formarum* ("On the
;; latitudes of forms"), was printed at Padua in 1486 "secundum doctrinam
;; magistri Nicolai Oresme"; we quote its Latin, expanded from the
;; printer's abbreviations, with our English. Maximilian Curtze, *Die
;; mathematischen Schriften des Nicole Oresme* (Thorn, 1870), lists the
;; manuscripts of the full treatise and quotes its opening.

;; ## The latitude of forms
;;
;; The treatise opens with the sentence on which the rest stands:

(page/blockquote
 "Omnis res mensurabilis exceptis numeris ymaginatur ad modum quantitatis continue."
 "Oresme, De configurationibus qualitatum I.1, as quoted by Curtze (1870), p. 11 (\"Every measurable thing except numbers is imagined in the manner of a continuous quantity\")")

;; A *form* or *quality* (heat in a rod, whiteness on a wall, the speed of
;; a body) has an *extension*, the parts of the subject or the stretch of
;; time it lasts, and at each point an *intensity*, its degree. Oresme lays
;; the extension along a line, the *longitude*, and raises at each point a
;; perpendicular as long as the degree there, the *latitude*. The tops of
;; the perpendiculars draw a figure, and the figure *is* the quality: two
;; qualities compare as their figures, and the quantity of a quality is the
;; area of its figure. Curtze (p. 11) saw in it "a rectangular coordinate
;; system, exactly as Descartes developed his".
;;
;; The figure sorts the qualities into kinds. The school text defines them
;; one sentence at a time; the player draws each, its numbers by raster:

(page/blockquote
 "Latitudo uniformiter difformis est illa in qua est equalis excessus graduum eque distantium."
 "De latitudinibus formarum (Padua, 1486), the definitions (\"A uniformly difform latitude is one in which the excess of degrees equally distant is equal\")")

(latitude/player)

;; Now try it yourself: the five red points are the degrees over five
;; equally spaced points of the extension $AB$, and the top of the figure
;; is the curve through them. Drag them, and the figure is named live as
;; Oresme names it. The area is measured by gold strips, each degree from
;; the raster kernel of the curve, beside Boole's rule (exact for this
;; curve); the dashed line is the *mean degree*, the uniform quality of the
;; same quantity. The badge checks the next section's theorem on your
;; figure.

(latitude/playground)

;; Emmy checks the kernels behind the playground: the area rule is exact
;; for the curves it draws, and equal excesses make a straight top.

(page/grade-badges (latitude/grades) #{:grade/proved :grade/numeric})

;; ## The mean degree theorem
;;
;; At Merton College, Oxford, in the 1330s, William Heytesbury and Richard
;; Swineshead stated a rule for motion whose speed grows by equal amounts
;; in equal times: it covers the distance of a uniform motion at its middle
;; speed. They proved it by arithmetic. Oresme proved it with the figure.
;; The school text states it for every uniformly difform quality:

(page/blockquote
 "Omnis latitudo uniformiter difformis tam quo ad eius esse successivum quam quo ad eius esse permanens correspondet suo gradui medio."
 "De latitudinibus formarum (Padua, 1486), art. 3, conclusion 2 (\"Every uniformly difform latitude, as to its successive being as well as its permanent being, corresponds to its middle degree\")")

;; The *successive* being is a motion in time; the *permanent* being is a
;; quality spread over a body. The proof rests on two suppositions, the
;; midline of a triangle and the parallelogram on the middle degree:

(page/blockquote
 "Quodlibet parallelogrammum ex linea medii gradus et linea extensionis constitutum est equale triangulo per quem nobis representatur latitudo uniformiter difformis."
 "De latitudinibus formarum (Padua, 1486), art. 2, supposition 5 (\"Any parallelogram made of the line of the middle degree and the line of extension equals the triangle by which the uniformly difform latitude is represented to us\")")

;; The player follows the proof one step at a time. Move the first and
;; the last degree under it: the triangle becomes a trapezium and the two
;; small triangles stay equal.

(mean/player)

;; Today we write the speed $v(s) = v_0 + (v_1 - v_0)\,s/t$ and the distance
;; as the area under it. Emmy integrates and solves the relation for each
;; quantity:

(mean/derivation)

;; Now try it yourself: the five quantities $v_0$, $v_1$, $t$, $v_m$ and
;; $d$ are tied, so move any one and the others are solved again, and
;; drag the points on the figure: the first degree at $A$, the corner $C$
;; (the last degree and the time), and the purple handle over the middle
;; instant, which bends the top. The distance is the sum of 64 strips by
;; raster; the blue rectangle is the uniform motion at $v_m$. Bend the
;; top and the motion is no longer uniformly difform: the badge turns red,
;; and the strips leave the rectangle by $2bt/3$.
;;
;; From rest ($v_0 = 0$), cut the time in four: the distances in the four
;; quarters are as $1 : 3 : 5 : 7$. Three centuries later this is
;; [Galileo's odd-number law](../galileo-two-new-sciences/), his Third Day,
;; Theorem I and Corollary I, drawn with Oresme's figure.

(mean/tied)

(page/grade-badges (mean/grades) #{:grade/proved :grade/numeric})

;; ## The harmonic series diverges
;;
;; In his *Quaestiones super geometriam Euclidis* (question 2) Oresme asks
;; whether a sum of infinitely many ever smaller parts must be finite. It
;; need not: add $1 + \tfrac12 + \tfrac13 + \tfrac14 + \cdots$. His
;; argument groups the terms so that each group is at least a half. The
;; *Quaestiones* are not in our corpus, so the steps below paraphrase him
;; (the text is in H. L. L. Busard's edition, Leiden, 1961); the numbers
;; on each step are the sums by raster.

(series/player)

;; The bound is $H(2^k) \ge 1 + k/2$: to pass 11 you need $2^{20}$ terms,
;; but you will pass it. Euler (1734) found how slowly: the partial sums
;; follow $\ln N + \gamma$, with $\gamma = 0.5772\ldots$ his constant.
;;
;; Oresme also knew that some infinite sums are finite. In the
;; *De configurationibus* (III.8) a quality has degree 1 in the first half
;; of an hour, 2 in the next quarter, 3 in the next eighth, and so on
;; without end; its figure has infinitely many steps, ever higher, and yet
;; its area is $\sum n/2^n = 2$, twice the first half.
;;
;; Now try it yourself: set the number of terms $N$ (or drag the black
;; marker along the axis of doublings), change the exponent in $1/n^p$ (or
;; drag the top of the second bar, which is $1/2^p$), or switch to
;; Oresme's convergent series. Every partial sum is added term by term on
;; raster kernels. With $p > 1$ the groups fall below one half, the badge
;; turns red, and the sum stays bounded: the grouping proof needs $p \le 1$.

(series/playground)

(page/grade-badges (series/grades) #{:grade/proved :grade/numeric})

;; ## After Oresme
;;
;; Oresme's figures were printed in Padua and Venice around 1500 and read
;; in the universities. Galileo's figure of uniformly accelerated motion
;; in the *Two New Sciences* (1638) is Oresme's triangle; Descartes' axes
;; (1637) turn longitude and latitude into abscissa and ordinate. The area
;; under the figure became Newton's and Leibniz's integral, and the
;; grouping of the harmonic series became Cauchy's condensation test
;; (1821): $\sum a_n$ and $\sum 2^k a_{2^k}$ converge together.

^{::clerk/no-cache true}
(lens/section "oresme")

^{::clerk/no-cache true}
(page/prev-next "notebooks/oresme.clj")
