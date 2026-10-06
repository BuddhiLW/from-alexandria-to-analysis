;; # Newton, *Principia* (1687)
;;
;; In August 1684 Edmond Halley rode to Cambridge to ask Isaac Newton a
;; question. Robert Hooke, Christopher Wren and Halley suspected that the Sun
;; pulls the planets with a force that weakens as the square of the distance.
;; None of them could prove what orbit such a force produces. Newton said: an
;; ellipse. He had calculated it. Three months later he sent Halley a short
;; tract, *De motu corporum in gyrum*. By 1687 it had grown into the three
;; books of *Philosophiae Naturalis Principia Mathematica*, printed in London
;; at Halley's expense.
;;
;; Kepler (1609, 1619) had found three laws in Tycho Brahe's observations:
;; planets move on ellipses with the Sun at a focus, a planet's line to the
;; Sun sweeps equal areas in equal times, and the square of the period goes as
;; the cube of the size. Newton derived them from his laws of motion and one
;; force. We read three pieces of Book I in Andrew Motte's English translation
;; of 1729.
;;
;; Newton knew the calculus: he had invented his method of fluxions in 1665.
;; But he wrote *Principia* in the geometry of Euclid and Apollonius, with
;; quantities that become "ultimately equal". The previous notebook,
;; Leibniz's *Nova Methodus*, printed the calculus three years before.
^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns newton-principia
  {:history/year 1687
   :history/title "Newton, Principia Mathematica"
   :history/era "Calculus"
   :nextjournal.clerk/toc true
   :nextjournal.clerk/visibility {:code :hide :result :show}}
  (:require [alexandria.newton.principia-view :as view]
            [clojure.string :as str]
            [history-of-math.page :as page]
            [nextjournal.clerk :as clerk]))

^{::clerk/visibility {:result :hide}}
(defn badges [rows]
  (clerk/html
   (into [:div {:style {:display "flex" :flex-direction "column" :gap "0.3em"}}]
         (for [{:keys [label grade]} rows
               :let [tone (case grade :grade/proved "#2f8a3e" :grade/numeric "#b7791f" "#c0392b")]]
           [:div {:style {:display "inline-block" :padding "0.15em 0.8em" :border-radius "1em"
                          :font-family "ui-monospace, monospace" :font-size "0.85em"
                          :border (str "1px solid " tone) :color tone :width "fit-content"}}
            (str (name grade) " by Emmy: " label)]))))

^{::clerk/visibility {:result :hide}}
(defn statement [id]
  (let [{:keys [statement source]} (view/proposition id)]
    (clerk/md (str "> *" statement "*\n\n— Newton, *Principia*, tr. A. Motte (1729), " source))))

^{::clerk/visibility {:result :hide}}
(defn graded-where [pred] (filter (comp pred :label) (view/graded)))

;; ## Lemma I: first and last ratios
;;
;; Book I opens with eleven lemmas on what Newton calls "the method of first
;; and last ratios". They replace Leibniz's infinitely small $dx$ with a
;; process: let a quantity shrink, and look at what a ratio tends to.

(statement :newton/lemma-1)

(clerk/md (str "*" (:proof (view/proposition :newton/lemma-1)) "*"))

;; This is the limit, stated as an argument by contradiction. A difference
;; that stays above some fixed $D$ cannot also fall below every given
;; difference. Lemma II applies it to areas. Inscribe rectangles under a curve
;; and circumscribe others. The two figures differ by one rectangle, $1/n$
;; wide, so as $n$ grows they become "ultimately equal" to the curved area.
;; Archimedes squeezed areas between two polygons in the same way, by
;; exhaustion; Newton turns the squeeze into a general method. Cauchy (1821)
;; will define the limit in these terms, with an $\varepsilon$ for Newton's
;; "any given difference".

(view/lemma-1)

(badges (graded-where #(str/includes? % "Lemma")))

;; ## Proposition I: areas proportional to the times
;;
;; This is Kepler's second law, derived for *any* force that points to a fixed
;; centre $S$.

(statement :newton/prop-1)

;; Newton replaces the continuous pull by a series of blows. Time is cut into
;; equal moments. In each moment the body moves in a straight line (Law I).
;; At the end of each moment one impulse toward $S$ changes its course.
;; Euclid's *Elements* I.37 and I.38 then do the work. Triangles with equal
;; bases and the same vertex are equal. So are triangles on the same base
;; between the same parallels. Each step below is an exact construction.
;; Emmy proves the areas equal for arbitrary $A$, $B$ and any size of
;; impulse.

(view/proposition-1)

(badges (graded-where #(or (str/includes? % "SAB") (str/includes? % "SBc") (str/includes? % "polygon"))))

;; The three Euclid steps are proved symbolically. The polygon itself is
;; computed numerically: 200 impulses under an inverse-square pull, whose
;; triangles $SAB, SBC, \ldots$ agree to $10^{-12}$. In the last step the
;; moments shrink, the polygon tends to a curve, and the equal triangles
;; become Kepler's equal areas.

;; ## Proposition XI: the ellipse and the inverse square
;;
;; Now the question Halley brought to Cambridge.

(statement :newton/prop-11)

;; Newton's argument uses the ellipse's focal property from Apollonius'
;; *Conics*: $SP + PH$ equals the whole axis. His measure of force, from
;; Proposition VI, is the limit of $QR/(QT^2 \cdot SP^2)$, where $QR$ is how
;; far the body falls from the tangent in a short time. On the ellipse this
;; ratio is the constant $1/L$, with $L$ the latus rectum. So the force goes
;; as $1/SP^2$.
;;
;; Emmy checks the same conclusion in the calculus that followed. Write the
;; ellipse about its focus as $r = p/(1 + e\cos\theta)$. Let the body sweep
;; equal areas, $r^2\,\dot\theta = h$ (Proposition I). Differentiate the
;; position twice. The acceleration points at $S$ and has size $h^2/(p\,r^2)$.
;; Equivalently, $u = 1/r$ satisfies $u'' + u = 1/p$. This is the orbit
;; equation, which Jacques Binet wrote in 1818.

(view/proposition-11)

(badges (graded-where #(or (str/includes? % "u =") (str/includes? % "acceleration")
                           (str/includes? % "a_x") (str/includes? % "a_y"))))

;; In the figure the body moves at the speed Kepler's equation gives. It is
;; fast near $S$ and slow far away. The arrow shows the pull toward $S$,
;; drawn with length proportional to $1/SP^2$.
;;
;; Propositions XII and XIII do the hyperbola and the parabola. Corollary 1
;; of XIII states the converse: an inverse-square force always produces a
;; conic about the focus. Book III applies all this to the Moon, the planets,
;; the tides and the comets. A century later Lagrange and Laplace recast
;; Newton's geometry in Leibniz's notation, the language in which mechanics
;; is still written.

^{::clerk/no-cache true}
(page/prev-next "notebooks/newton_principia.clj")
