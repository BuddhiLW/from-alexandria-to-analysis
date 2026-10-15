;; # Copernicus, *De revolutionibus*: the Earth among the planets
^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns copernicus
  {:history/year 1543
   :history/title "Copernicus, De revolutionibus"
   :history/era "Renaissance astronomy"
   :history/fields #{:mechanics-physics :trigonometry}
   :nextjournal.clerk/visibility {:code :hide :result :show}
   :nextjournal.clerk/toc true}
  (:require [alexandria.copernicus.orbits-widget :as orbits]
            [alexandria.copernicus.periods-widget :as periods]
            [history-of-math.lens :as lens]
            [history-of-math.page :as page]
            [history-of-math.widgets :as hom]
            [nextjournal.clerk :as clerk]))

^{::clerk/visibility {:code :hide :result :hide}}
(hom/install!)

;; Nicolaus Copernicus, canon of Frauenburg (Frombork) in Warmia, printed
;; his *De revolutionibus orbium coelestium* at Nuremberg in 1543, the year
;; he died. He kept Ptolemy's tools, uniform circles on circles, and moved
;; one thing: the Earth leaves the centre and becomes the third planet,
;; carried once a year about the Sun.
;;
;; We read him in the Latin of the 1543 edition (la.wikisource) and in
;; C. L. Menzzer's German (*Über die Kreisbewegungen der Weltkörper*,
;; Thorn, 1879, on de.wikisource); book and chapter as Copernicus numbers
;; them.

;; ## The order of the orbs (I.10)
;;
;; Book I, chapter 10 sets out the new order, from the fixed stars inward:

(page/blockquote
 "Sequitur errantium primus Saturnus, qui XXX. anno suum complet circuitum. Post hunc Iupiter duodecennali revolutione mobilis. Deinde Mars, qui biennio circuit."
 "Copernicus, De revolutionibus I.10, 1543, fol. 9a (\"Of the wanderers Saturn comes first, completing its circuit in the thirtieth year; after it Jupiter, moving in a twelve-year revolution; then Mars, which goes round in two years\")")

(page/blockquote
 "Quinto loco Venus nono mense reducitur. Sextum denique locum Mercurius tenet, octuaginta dierum spacio circumcurrens. In medio vero omnium residet Sol."
 "Copernicus, De revolutionibus I.10, fol. 9b (\"In the fifth place Venus returns in the ninth month. Mercury holds the sixth place, running round in eighty days. In the middle of all sits the Sun\")")

;; And in the same chapter he says what the order buys: the stations and
;; backward runs of the planets are the Earth's own motion, seen in them.

(page/blockquote
 "errantium quoque stationes, retrogradationes atque progressus non illorum, sed telluris esse motus videbitur, quem illa suis mutuant apparentiis."
 "Copernicus, De revolutionibus I.9, fol. 7b (\"the stations, retrogradations and progressions of the wanderers will be seen to be not theirs but the Earth's motion, which they borrow for their appearances\")")

;; ## Retrograde loops from a moving Earth (I.10, V.1)
;;
;; Put the Earth $E$ on a circle of radius 1 turning once a year, and the
;; planet $P$ on a circle of radius $a$ with period $P$. The planet is seen
;; along $G = P - E$; its longitude $\lambda$ is the angle of $G$. Emmy
;; derives the rate $\dot\lambda$, shows that it is Ptolemy's rate for an
;; epicycle on a deferent once the roles are read right, and that at the
;; nearest point the planet runs backwards exactly when the inner body is the
;; faster:

(orbits/derivation)

;; Book V, chapter 1 names the borrowed motion the *motus commutationis*,
;; the motion of parallax:

(page/blockquote
 "weil sie es ist, welche bei allen Planeten die Stillstände und die rechtläufigen und rückläufigen Bewegungen in der Erscheinung hervorbringt, nicht weil der Planet selbst dieselben an sich hat, denn derselbe ist in seiner eigenen Bewegung immer rechtläufig"
 "Copernicus, De revolutionibus V.1, tr. Menzzer (1879), p. 255 (\"because it is this that produces in every planet the appearance of stations, direct and retrograde motion; not that the planet has them itself, for in its own motion it is always direct\")")

;; Now try it yourself: the playground INTEGRATES both orbits, one RK4 step
;; being a raster kernel iterated in the page, and carries the longitude
;; seen from the Earth along as a fifth coordinate. Where its rate turns
;; negative the path is red, and the arc being crossed is named with its
;; stations, width and days. The frame slider moves the same motion from
;; the Sun's frame ($f = 1$, Copernicus) to the Earth's ($f = 0$, Ptolemy),
;; where the loops appear. Drag the planet to change its distance; drag
;; time on the rate plot. One preset makes an outer planet faster than the
;; Earth, against Copernicus' order, and the loops vanish.

(orbits/playground)

;; ## Synodic and sidereal periods (V.1)
;;
;; What an observer counts is the return of the planet to the same place
;; relative to the Sun, the *synodic* period $S$ (for Copernicus the
;; revolution of parallax). What the planet does on its own circle is the
;; *sidereal* period $P$. The Earth laps an outer planet once each $S$, so
;; $1/S = 1/E - 1/P$; an inner planet laps the Earth, $1/S = 1/P - 1/E$.
;; Copernicus gives each planet a cycle: so many years in which the Earth
;; laps it so many times. The table turns his cycles into $S$ and then into
;; $P$, by raster, and sets the modern periods beside them:

(periods/table)

;; The three periods are tied: move $E$, $P$ or $S$ and the others are
;; solved. The successive conjunctions are drawn on the planet's orbit,
;; each $360\,S/P$ degrees on from the last.

(periods/synodic)

;; ## Ptolemy's model and his, side by side
;;
;; For an outer planet Ptolemy's deferent is the planet's own orbit and his
;; epicycle is the Earth's orbit carried along; for Venus and Mercury the
;; deferent is the Earth's orbit (the mean Sun) and the epicycle the
;; planet's. Both models are integrated below on one time slider. Ptolemy's
;; scale $k$ is free (drag it: the sky does not change); his epicycle, as a
;; fraction $m$ of what the Earth's orbit fixes, is not. With $m = 1$ the
;; two lines of sight agree at every instant; change $m$ and they part.

(periods/side-by-side)

;; ## The order that only the moving Earth fixes
;;
;; In Ptolemy every planet has its own scale, so nothing fixes how far
;; Mars is compared with Jupiter. Once each outer epicycle *is* the Earth's
;; orbit, its ratio $r/R$ gives the planet's distance $a = R/r$ in units of
;; the Earth's; for Venus and Mercury $a = r/R$. Copernicus' claim in I.10
;; is that the distances so fixed and the periods fall into one order, the
;; farther the slower. Drag any orbit in or out, or change a period, and
;; see whether the two orders still agree.

(periods/distances)

;; ## What came after
;;
;; Copernicus kept circles and needed small epicycles still to match the
;; planets' uneven speeds. [Kepler](../kepler-mysterium/) took his
;; distances seriously enough to ask why they were what they were, and then
;; replaced the circles by Apollonius' ellipses.

^{::clerk/no-cache true}
(lens/section "copernicus")

^{::clerk/no-cache true}
(page/prev-next "notebooks/copernicus.clj")
