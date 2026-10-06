;; # Kepler, *Astronomia Nova* and *Harmonices Mundi*: the three laws
;;
;; In 1600 Kepler joined Tycho Brahe in Prague. Tycho had observed the sky
;; for thirty years, from his island observatory at Hven and then in Bohemia,
;; with instruments finer than any before him. When Tycho died in 1601,
;; Kepler inherited the observations. He set himself one planet, Mars: its
;; orbit is the most eccentric of the outer planets, and no theory had ever
;; fitted it. He called the work his war with Mars. It took him eight years,
;; and he published it at Prague in 1609 as *Astronomia Nova*, the *new
;; astronomy*.
;;
;; Before Kepler every astronomer, Ptolemy and Copernicus included, built
;; planetary motion from circles moving uniformly. Kepler ended with an
;; ellipse and a law of areas, and ten years later, in *Harmonices Mundi*
;; (Linz, 1619), with a law that ties all the planets together.
;;
;; The ellipse was not new to geometry. Apollonius of Perga had written its
;; theory in the *Conics* around 200 BC, including the property this page
;; uses (*Conics* III.52): the distances from any point of an ellipse to the
;; two foci add up to the major axis. Kepler found that a planet uses it.
;;
;; Neither book has a public-domain English translation. The quotations come
;; from J. L. E. Dreyer, *History of the Planetary Systems from Thales to
;; Kepler* (Cambridge, 1906, ch. XV), and from Newton's *Principia* in
;; Motte's translation. The figures and checks come from Alexandria's shelf
;; (`alexandria.kepler.orbit`).
^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns kepler-laws
  {:history/year 1609
   :history/title "Kepler, Astronomia Nova and Harmonices Mundi"
   :history/era "Scientific Revolution"
   :nextjournal.clerk/visibility {:code :hide :result :show}
   :nextjournal.clerk/toc true}
  (:require [alexandria.kepler.orbit :as orbit]
            [alexandria.kepler.orbit-view :as view]
            [history-of-math.page :as page]
            [nextjournal.clerk :as clerk]))

^{::clerk/visibility {:result :hide}}
(defn quote-md [{:keys [text source]}]
  (clerk/md (str "> " text "\n>\n> — " source)))

^{::clerk/visibility {:result :hide}}
(def mars (view/proposition :kepler/war-with-mars))

^{::clerk/visibility {:result :hide}}
(def harmonice (view/proposition :kepler/harmonice))

^{::clerk/visibility {:result :hide}}
(def graded (orbit/graded))

;; ## The eight minutes
;;
;; Kepler first gave Mars what everyone used: a circle, with the Sun off
;; centre and an *equant*, a point from which the planet appears to move
;; uniformly. He fitted it to Tycho's oppositions with enormous labour, and it
;; matched them. Then he tested it at other points of the orbit. Forty-five
;; degrees from the line of apsides it was wrong by 8 minutes of arc, about a
;; quarter of the Moon's width.

(quote-md (first (:passages mars)))

;; Eight minutes was too small for any earlier observer to notice. Tycho's
;; observations were finer than that, so Kepler threw the circle away.

;; ## The first law: an ellipse with the Sun at a focus
;;
;; Notation. The orbit has semi-major axis $a$ (the mean distance) and
;; eccentricity $e$, a number between 0 (a circle) and 1. Its semi-minor axis
;; is $b = a\sqrt{1-e^2}$. With the centre at the origin, the planet is at
;;
;; $$x = a\cos E, \qquad y = b \sin E,$$
;;
;; where $E$, the *eccentric anomaly*, is the angle at the centre on the
;; circle of radius $a$ drawn around the ellipse. The Sun sits at the focus
;; $(ae, 0)$. Emmy proves that the Sun–planet distance is $a(1 - e\cos E)$,
;; and that the distance to the other focus is $a(1 + e\cos E)$. The two add
;; to $2a$: that is Apollonius III.52.
;;
;; For Mars, Kepler found $e = 0.09264$ and $a = 1.5235$ in units of the
;; Earth's mean distance.

;; ## The second law: equal areas in equal times
;;
;; The line from the Sun to the planet sweeps out area. Kepler's law says it
;; sweeps equal areas in equal times. So the planet runs fast near the Sun
;; and slow far from it.
;;
;; Write the area swept since perihelion (the point nearest the Sun) through
;; $E$:
;;
;; $$A(E) = \frac{ab}{2}\,(E - e\sin E).$$
;;
;; Emmy proves this. The $E$-derivative of the right side equals the rate at
;; which the Sun–planet line sweeps, and the area is 0 at $E = 0$. Now name
;; the bracket. The *mean anomaly*
;;
;; $$M = E - e\sin E$$
;;
;; is *Kepler's equation*. With it $A = \frac{ab}{2}M$, so the area grows
;; linearly in $M$. The second law says area grows linearly in time, so $M$
;; is time measured as an angle: $M = 2\pi t/T$ over a period $T$. One
;; revolution, $M = 2\pi$, sweeps $\pi ab$, the whole ellipse.

(quote-md (second (:passages mars)))

;; Press ▶ to watch Mars. The circle fails, the ellipse fits, and twelve
;; sectors swept in twelve equal times fill one after another. The planet is
;; an Emmy function compiled to WebAssembly. Each frame it solves Kepler's
;; equation for the moving time.

(view/war-with-mars)

;; ## Solving Kepler's equation
;;
;; Given the time you know $M$ and need $E$. The equation $E - e\sin E = M$
;; has no solution in closed form. Kepler solved it by trial. Alexandria
;; solves it two ways and checks that they agree. First, Emmy writes the
;; equation and raster's Brent method finds its root on $[M - e,\ M + e]$.
;; The root must lie there because $|E - M| = |e\sin E| \le e$. Second, the
;; steps $E \leftarrow M + e\sin E$ shrink the error by a factor $e$ each
;; time. The same steps run inside the browser's raster kernel.
;;
;; The badges: the identities of the first two laws, *proved* by Emmy. Then
;; the *numeric* ones: the root of Kepler's equation, and the equal areas
;; computed two ways, by Kepler's formula and by raster's quadrature of the
;; rate at which the line from the Sun sweeps area. Both of those numbers
;; come from raster. Last, the third law, checked on Newton's table.

(page/grade-badges graded #{:grade/proved :grade/numeric})

;; ## The third law (*Harmonices Mundi*, 1619)

(quote-md (first (:passages harmonice)))

;; With $T$ the period in years and $a$ the mean distance in Earth
;; distances, the law says $T^2/a^3$ is the same number, 1, for every
;; planet. Newton prints the periods and Kepler's mean distances side by
;; side in the *Principia* (Book III, Phenomenon IV). Here is that table and
;; the ratio:

(clerk/table
 {:head ["planet" "T (days)" "T (years)" "a (Kepler)" "T²/a³"]
  :rows (for [{:keys [planet T a k]} (orbit/third-law)]
          [(name planet) (orbit/periods planet) (format "%.4f" T) (format "%.5f" a) (format "%.4f" k)])})

(let [{:keys [min max]} (orbit/spread)]
  (clerk/md (format "Over the six planets the ratio runs from **%.4f** to **%.4f**: within 0.8%% of 1, on distances measured with the instruments of 1600." min max)))

;; On logarithmic axes, $\log T^2$ against $\log a^3$, the law is a straight
;; line of slope 1:

(view/harmonice)

;; ## Toward Newton

(quote-md (second (:passages harmonice)))

;; Newton turned the laws into mechanics. In the *Principia* (1687), Book I:

(quote-md (nth (:passages harmonice) 2))

;; Proposition 1 proves that equal areas follow from *any* force directed to
;; a fixed centre. Read backwards, Kepler's second law says that the force on
;; a planet points at the Sun.

(quote-md (nth (:passages harmonice) 3))

;; Proposition 11 finds that force for an ellipse with the centre of force at
;; a focus: it falls as the inverse square of the distance. The third law
;; then fixes one constant for all the planets, the Sun's gravity. Kepler's
;; three laws, found in Tycho's numbers, became three consequences of one
;; law.

;; ## The argument, step by step
;;
;; ### *Astronomia Nova*

(page/steps (:steps mars))

;; ### *Harmonices Mundi*

(page/steps (:steps harmonice))

^{::clerk/no-cache true}
(page/prev-next "notebooks/kepler_laws.clj")
