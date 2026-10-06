;; # Archimedes: the circle, the parabola and *The Method*
;;
;; Archimedes lived in Syracuse, on Sicily, from about 287 to 212 BC. He
;; wrote to the mathematicians of Alexandria (Conon, Dositheus, and
;; Eratosthenes, the librarian) and sent them his results as letters. He
;; read Euclid's *Elements* as his toolbox and went past it: where Euclid
;; compares figures, Archimedes measures curved ones. He was killed when
;; Rome took Syracuse.
;;
;; His method of proof is *exhaustion*, which Eudoxus had invented and
;; Euclid's Book XII uses: trap the curved figure between polygons, make the
;; trap as tight as you please, and rule out every other value by
;; contradiction. This notebook follows three works in T. L. Heath's
;; translation, *The Works of Archimedes* (1897, with the 1912 supplement
;; for *The Method*): *Measurement of a Circle*, the rings argument told
;; later about the same circle, and *The Method*, the letter where
;; Archimedes shows how he *found* the area of the parabolic segment before
;; he proved it.
;;
;; Press ▶ to watch a proof, or click any step to see its figure. The
;; figures, Archimedes' numbers and the checks come from Alexandria's shelf
;; of Archimedes (`alexandria.archimedes.circle`, `alexandria.archimedes.method`).
;; Each moving figure is an Emmy function compiled to WebAssembly by
;; emmy-viewers' raster backend.
^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns archimedes
  {:history/year -250
   :history/title "Archimedes, Measurement of a Circle and The Method"
   :history/era "Greek geometry"
   :nextjournal.clerk/visibility {:code :hide :result :show}
   :nextjournal.clerk/toc true}
  (:require [alexandria.archimedes.circle :as circle]
            [alexandria.archimedes.circle-view :as view]
            [alexandria.archimedes.method :as method]
            [alexandria.archimedes.method-view :as method-view]
            [alexandria.archimedes.quadrature :as quadrature]
            [alexandria.archimedes.quadrature-view :as quadrature-view]
            [clojure.string :as str]
            [history-of-math.page :as page]
            [nextjournal.clerk :as clerk]))

;; ## *Measurement of a Circle*
;;
;; The treatise is very short. It gives three propositions:
;;
;; 1. A circle is equal to a right-angled triangle whose legs are the radius
;;    and the circumference.
;; 2. The circle is to the square on the diameter as 11 is to 14.
;; 3. The circumference of any circle is less than three times the diameter
;;    by one-seventh, but greater than three times the diameter by ten
;;    seventy-firsts.
;;
;; Proposition 2 follows from Proposition 3 by using 22/7 as the estimate
;; for the circumference; the text as transmitted puts it second.

;; ### Proposition 1
;;
;; > *The area of any circle is equal to a right-angled triangle in which one
;; > of the sides about the right angle is equal to the radius, and the other
;; > to the circumference, of the circle.*
;;
;; Call that triangle $K$, $r$ the radius and $C$ the circumference. In
;; modern notation the proposition says the area is $A = \frac12 r C$; with
;; $C = 2\pi r$ that is $\pi r^2$. Archimedes proves it by two
;; contradictions. Suppose the circle exceeds $K$ by some amount $\varepsilon$.
;; Double the sides of an inscribed polygon until what lies between polygon
;; and circle is less than $\varepsilon$; then the polygon exceeds $K$, but
;; cut at the centre and stood in a row it is a triangle lower and shorter
;; than $K$. Suppose the circle falls short of $K$, and a circumscribed
;; polygon gives the same contradiction from above. The two small windows
;; magnify the apex and the end of the base, where polygon and $K$ part.

(view/proposition-1)

(page/grade-badges [(nth (circle/graded-identities) 0)])

;; Archimedes takes one principle from outside the *Elements*: a convex
;; curve is longer than any inscribed broken line and shorter than any
;; circumscribed one. He states assumptions of this kind in *On the Sphere
;; and Cylinder*. Arc length as a limit is defined only in the analysis of
;; the nineteenth century, at the end of this series.

;; ### Why a triangle? Unrolling the rings
;;
;; Cut a circle along a radius and unroll it, and you get a triangle. Peel
;; the circle like an onion: the outer ring, of width $e$, has an outer edge
;; of length $2\pi r$ and an inner edge of length $2\pi(r-e)$. Straightened,
;; it is an isosceles trapezoid, the next ring in a shorter one, down to the
;; centre, whose ring has length nothing. The trapezoids stack into a
;; triangle with base $C$ and height $r$: Archimedes' $K$.
;;
;; This argument is not in Archimedes. Abraham bar Hiyya tells it in
;; twelfth-century Barcelona. It follows Archimedes' two habits:
;;
;; - **Seeing**: the circle made of its circles, $K$ made of its lines, each
;;   circle of radius $\rho$ exactly as long as the line at height $r-\rho$.
;;   This is the way of *The Method*, below.
;; - **Proving**: each ring lies between two rectangles of width $e$, one as
;;   long as its inner edge and one as long as its outer edge, and so does
;;   each slab of $K$. Both figures sit between the same two sums, which
;;   differ by $C\,e$. Take more rings and the difference is as small as
;;   you please: exhaustion.
;;
;; Move the slider to change the number of rings $n$, and so $e = r/n$.

(view/rings)

(page/grade-badges (circle/graded-ring-identities))

;; ### Proposition 2
;;
;; > *The area of a circle is to the square on its diameter as 11 to 14.*
;;
;; The square on the diameter has area $4r^2$, so the modern ratio is
;; $\pi/4$. Archimedes' $11/14$ is what $22/7$ gives for $\pi$: the
;; proposition is the bound of Proposition 3 read as an area.

(view/proposition-2)

(page/grade-badges [(nth (circle/graded-identities) 2)])

;; ### Proposition 3
;;
;; > *The circumference of any circle exceeds three times the diameter by a
;; > part which is less than one-seventh of the diameter but more than ten
;; > seventy-firsts.*
;;
;; In modern notation, $3\frac{10}{71} < \pi < 3\frac17$. Archimedes starts
;; from the hexagon and doubles the sides four times: 6, 12, 24, 48, 96. Every
;; ratio he writes is a pair of whole numbers, or whole numbers with a simple
;; fraction, rounded always in the direction that keeps the bound safe. Each
;; doubling rests on *Elements* VI.3: the bisector of an angle of a triangle
;; cuts the opposite side in the ratio of the sides about the angle. The
;; player shows that cut, the polygon it belongs to, his numbers, and the
;; bound each one gives on the number line.

(view/proposition-3)

(page/grade-badges [(nth (circle/graded-identities) 1)])

(let [checks (circle/table-checks)]
  (page/badge (every? :holds? checks)
              (str "all " (count checks) " of Archimedes' inequalities, decided exactly in rationals")))

;; ## *The Method*: how he found it
;;
;; Archimedes published the area of the parabolic segment in *Quadrature of
;; the Parabola*, proved by exhaustion. How he *found* it was unknown for two
;; thousand years. In 1229 a scribe in Constantinople scraped a book of
;; Archimedes off its parchment and wrote a prayer book over it. In 1906
;; Johan Ludvig Heiberg read the faint undertext and found a work thought
;; lost: *The Method*, a letter to Eratosthenes. Imaging at the Walters Art
;; Museum (1999–2008) recovered more of it. Kepler, Newton and Leibniz never
;; saw it; they reinvented its way of thinking.

(let [{:keys [letter]} (method-view/proposition)]
  (clerk/md
   (str (str/join "\n>\n" (map #(str "> *" % "*") (:passages letter)))
        "\n\n— " (:from letter) " to " (:to letter) ", *The Method*, tr. T. L. Heath ("
        (:source letter) ")")))

;; His first example is the parabolic segment. He imagines it made of lines,
;; every line parallel to the axis, each with a weight. He hangs them on a
;; balance against a triangle and lets the balance tell him the area. In
;; Heath's letters, $A$ and $C$ are the ends of the chord and $B$ is the
;; vertex of the segment.
;;
;; > *Any segment of a section of a right-angled cone (i.e. a parabola) is
;; > four-thirds of the triangle which has the same base and equal height.*

(method-view/proposition-1)

(page/grade-badges (method/graded) #{:grade/proved :grade/numeric})

;; Then the sentence that makes *The Method* precious, because it shows a
;; mathematician separating seeing from proving:
;;
;; > *Now the fact here stated is not actually demonstrated by the argument
;; > used; but that argument has given a sort of indication that the
;; > conclusion is true. Seeing then that the theorem is not demonstrated,
;; > but at the same time suspecting that the conclusion is true, we shall
;; > have recourse to the geometrical demonstration which I myself
;; > discovered and have already published.*
;;
;; The balance adds up infinitely many lines, which Greek geometry could not
;; justify. The published demonstration uses finitely many triangles: each
;; generation of inscribed triangles has a quarter of the area of the one
;; before, $1 + \frac14 + \frac1{16} + \cdots$, and a double contradiction
;; shows the segment is $\frac43$ of the first triangle.

;; ## *Quadrature of the Parabola*: the published proof
;;
;; Here the segment is cut from $y = 1 - x^2$ by the chord from $(-1, 0)$ to
;; $(1, 0)$. Its first triangle has area 1. *Stage* $n$ adds $2^n$ triangles,
;; one in each segment still uncovered. The areas shown are exact fractions.

^{::clerk/visibility {:result :hide}}
(defn quad-statement [id]
  (let [{:keys [statement source]} (quadrature-view/proposition id)]
    (clerk/md (str "> *" statement "*\n>\n> — " source))))

;; ### Proposition 21

(quad-statement :archimedes/parabola-21)

(quadrature-view/player :archimedes/parabola-21)

;; ### Proposition 22
;;
;; Move the slider to add stages. The polygon grows toward $\frac43$ and
;; never reaches it.

(quad-statement :archimedes/parabola-22)

(quadrature-view/player :archimedes/parabola-22)

;; ### Proposition 23
;;
;; Archimedes sums finitely many areas, each a quarter of the one before,
;; and adds one third of the last. The result is exactly $\frac43$ of the
;; first. No infinite sum appears.

(quad-statement :archimedes/parabola-23)

(quadrature-view/player :archimedes/parabola-23)

;; ### Proposition 24

(quad-statement :archimedes/parabola-24)

(quadrature-view/player :archimedes/parabola-24)

(page/grade-badges (quadrature/graded))


;; ## Where this goes
;;
;; Archimedes' two habits run through the rest of the series. Seeing a
;; figure as made of its lines returns with Cavalieri's indivisibles and with
;; Kepler, who sliced wine barrels to find their volume. Proving by trapping
;; between polygons returns as the limit: Cauchy's *Cours d'analyse* (1821)
;; makes the infinite sum Archimedes avoided an object of mathematics, and
;; Riemann's integral (1854) is his squeeze between lower and upper sums.
;; Before that, Apollonius, the next Greek in the series, studies the
;; parabola, ellipse and hyperbola as sections of one cone.

^{::clerk/no-cache true}
(page/prev-next "notebooks/archimedes.clj")
