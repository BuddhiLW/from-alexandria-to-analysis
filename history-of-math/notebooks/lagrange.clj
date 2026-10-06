;; # Lagrange: the calculus of variations and analytical mechanics
;;
;; *1696 to 1797: from Johann Bernoulli's challenge to the Théorie des
;; fonctions analytiques.*
;;
;; Newton and Leibniz gave the eighteenth century the calculus: how to find
;; the slope of a curve and the area under it. Newton's *Principia* (1687)
;; used it to explain Kepler's laws of the planets. The next question was
;; harder. Not "what is the slope of this curve?" but "which curve, of all
;; curves, makes some quantity as small as possible?" This notebook follows
;; that question from a challenge printed in a Leipzig journal in 1696 to
;; Joseph-Louis Lagrange's *Mécanique analytique* of 1788, which turned all
;; of mechanics into one such problem, and to his *Théorie des fonctions
;; analytiques* of 1797.
;;
;; Lagrange was born in Turin in 1736. At nineteen he wrote to Euler with a
;; new method. In 1766 he succeeded Euler at the Berlin Academy, and in 1787
;; he moved to Paris, where the *Mécanique analytique* was printed the next
;; year. Its preface makes a proud promise:
;;
;; > *On ne trouvera point de Figures dans cet Ouvrage.*
;; >
;; > One will find no figures in this work. (English rendering: this series.)
;;
;; This notebook answers that promise by drawing the figures.
;;
;; **Notation.** $t$ is time. A *path* $q(t)$ gives a position at each time;
;; $q'$ is its velocity, $q''$ its acceleration. A *Lagrangian*
;; $L(t, q, q')$ is a number computed from a time, a position and a velocity.
;; $\partial L / \partial q$ is the rate of change of $L$ when only $q$
;; moves. The badges below are checks run by
;; [Emmy](https://github.com/mentat-collective/emmy): **proved** means Emmy
;; simplified a difference to exactly $0$; **numeric** means a numerical
;; computation agreed to within a stated tolerance.
^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns lagrange
  {:history/year 1788
   :history/title "Lagrange: the calculus of variations and analytical mechanics"
   :history/era "Enlightenment"
   :nextjournal.clerk/visibility {:code :hide :result :show}
   :nextjournal.clerk/toc true}
  (:require [alexandria.lagrange.lagrange-view :as view]
            [history-of-math.page :as page]
            [nextjournal.clerk :as clerk]))

^{::clerk/visibility {:result :hide}}
(defn badges
  "The graded checks of one section, as badges."
  [id]
  (clerk/html
   (into [:div {:style {:display "flex" :flex-wrap "wrap" :gap "0.4em" :margin "0.6em 0"}}]
         (for [{:keys [label grade]} (get (view/graded) id)
               :let [ok? (not= grade :grade/fails)]]
           [:div {:style {:padding "0.15em 0.8em" :border-radius "1em" :font-size "0.85em"
                          :border (str "1px solid " (if ok? "#2f8a3e" "#c0392b"))}}
            [:strong {:style {:color (if ok? "#2f8a3e" "#c0392b")}} (name grade)] " " label]))))

^{::clerk/visibility {:result :hide}}
(defn quote-block
  "An original passage and this series' English rendering."
  [original english source]
  (clerk/html
   [:blockquote
    [:p [:em original]]
    [:p english " " [:span {:style {:opacity 0.6}} "(English rendering: this series.)"]]
    [:p {:style {:font-size "0.85em" :opacity 0.7}} source]]))

;; ## 1696: the brachistochrone
;;
;; In June 1696 Johann Bernoulli, professor at Groningen, printed a challenge
;; in the *Acta Eruditorum* of Leipzig, the journal Leibniz had helped found.
;; A bead slides without friction from a point $A$ to a lower point $B$,
;; pulled only by its weight. Which track gets it there soonest? The Greek
;; name is *brachistochrone*, "shortest time".

(let [c (:challenge (view/brachistochrone-text))]
  (quote-block (:original c) (:english c) "Acta Eruditorum, June 1696, p. 269"))

;; The straight line is shortest, but it is not quickest: a track that drops
;; steeply first lets the bead pick up speed early. Galileo knew in 1638 that
;; a circle arc beats the chord. Answers came from Johann, his brother Jakob,
;; Leibniz, l'Hôpital and, anonymously, Newton. All found the same curve: the
;; **cycloid**, traced by a point on the rim of a wheel rolling along a line.
;;
;; In the language Lagrange would later give it, the bead's speed after a drop
;; $y$ is $\sqrt{2 g y}$ ($g$ the acceleration of gravity), and the time is
;; the integral of $ds / \sqrt{2 g y}$ along the track, $ds$ the element of
;; length. The cycloid is the track for which that integral is least. Emmy
;; checks that the cycloid $x = a(\theta - \sin\theta)$,
;; $y = a(1 - \cos\theta)$ satisfies the Euler–Lagrange equation of this
;; integral (section 2 explains that equation), and races three beads.

(view/brachistochrone)

(badges :brachistochrone)

;; **The tautochrone is the same curve.** Christiaan Huygens had found in
;; 1673, in his *Horologium oscillatorium*, that a bead in a bowl shaped like
;; an upside-down cycloid reaches the bottom in the same time from any
;; height. We do not derive this again. We recognise a pattern: measured by
;; arc length $s$ from the bottom, the bowl's height is $s^2/8a$. That is the
;; potential of a spring. So the bowl's Lagrangian is Emmy's harmonic
;; oscillator with stiffness $k = mg/4a$, whose period does not depend on the
;; amplitude. The badges show the two identities and the match.

(badges :tautochrone)

;; ## 1744 to 1755: from Euler's polygon to Lagrange's $\delta$
;;
;; Leonhard Euler, then in Berlin, gathered such problems in the *Methodus
;; inveniendi lineas curvas* (1744). His creed, from its first appendix:

(let [{:keys [original english source]} (:euler (view/delta-text))]
  (quote-block original english source))

;; Euler replaced a curve by a polygon, moved one vertex, and required the
;; integral not to change to first order. That gave his rule, but the method
;; leaned on the picture. On 12 August 1755 Lagrange, nineteen years old and
;; teaching at the artillery school of Turin, wrote to Euler with a new
;; symbol:

(let [{:keys [original english]} (:lagrange (view/delta-text))]
  (quote-block original english "Lagrange to Euler, Turin, 12 August 1755; Oeuvres de Lagrange t. XIV, p. 140"))

;; Instead of one vertex, the whole curve moves: $y$ becomes $y + \delta y$,
;; where $\delta y$ (Emmy's $\eta$ below) is any function that is zero at the
;; ends. Euler replied within a month:

(let [{:keys [original english source]} (:euler-reply (view/delta-text))]
  (quote-block original english source))

;; We derive the equation once, in Emmy, from a varied path. The variation of
;; $\int L\,dt$ along $\eta$ is the derivative in $\varepsilon$ of
;; $\int L(t, q + \varepsilon\eta, q' + \varepsilon\eta')\,dt$ at
;; $\varepsilon = 0$. Emmy checks, for every $L$, $q$ and $\eta$, that its
;; integrand equals
;; $\frac{d}{dt}\left(\eta\,\frac{\partial L}{\partial q'}\right) - \eta \left(\frac{d}{dt}\frac{\partial L}{\partial q'} - \frac{\partial L}{\partial q}\right)$.
;; The first term integrates to its values at the ends, where $\eta = 0$. So
;; the variation vanishes for every $\eta$ exactly when
;;
;; $$\frac{d}{dt}\frac{\partial L}{\partial q'} - \frac{\partial L}{\partial q} = 0,$$
;;
;; the **Euler–Lagrange equation**. Euler named the method the *calculus of
;; variations*.

(view/delta)

(badges :delta)

;; ## 1788: the *Mécanique analytique*
;;
;; The full preface states the plan:

(let [p (view/preface)]
  (clerk/html
   [:div
    [:blockquote [:p [:em (:plan-original p)]] [:p (:plan-english p) " " [:span {:style {:opacity 0.6}} "(English rendering: this series.)"]]]
    [:blockquote [:p [:em (:original p)]] [:p (:english p) " " [:span {:style {:opacity 0.6}} "(English rendering: this series.)"]]
     [:p {:style {:font-size "0.85em" :opacity 0.7}} (:source p)]]]))

;; ### The principle of virtual velocities
;;
;; Lagrange starts from statics. Johann Bernoulli had written to Varignon in
;; 1717 that in equilibrium the forces do no work on any small motion the
;; system allows. Jean d'Alembert (1743) turned motion into equilibrium by
;; adding to each force the reversed product of mass and acceleration.
;; Lagrange writes every position through independent *coordinates*, such as
;; the two angles of a double pendulum, and the principle becomes, coordinate
;; by coordinate, the Euler–Lagrange equation for $L = T - V$, kinetic minus
;; potential energy. Emmy checks the identity for the double pendulum.

(view/virtual-velocities)

(badges :virtual-velocities)

;; ### The pendulum and the double pendulum
;;
;; For the pendulum, Emmy's `Lagrange-equations` give
;; $\theta'' + (g/l)\sin\theta = 0$, and its energy is constant along every
;; motion. So the motions are the level curves of the energy in the plane of
;; angle $\theta$ and angular velocity $\theta'$: the *phase portrait*. Emmy
;; integrates the equations and the curves are drawn from its numbers. The
;; double pendulum gets the same treatment: two angles, two equations,
;; integrated numerically.

(view/equations-of-motion)

(badges :pendulum)
(badges :double-pendulum)

;; ### The Kepler problem
;;
;; Kepler found in 1609 that a planet's radius sweeps equal areas in equal
;; times, and that the orbit is an ellipse. Newton derived both from the
;; inverse-square law in Book I of the *Principia* (1687), with geometric
;; figures. In Lagrange's method there is no figure to draw: the Lagrangian of
;; a body of mass $m$ about a centre of strength $GM$, in polar coordinates
;; $r, \varphi$, is
;; $L = \tfrac12 m (r'^2 + r^2\varphi'^2) + GMm/r$. The angle $\varphi$ does
;; not appear in it, so its equation says $m r^2 \varphi'$ never changes:
;; Kepler's second law. And Emmy checks that every conic
;; $r = p/(1 + e\cos\varphi)$ with $p = h^2/GM$ solves the equations: circle,
;; ellipse, parabola and hyperbola alike.

(view/kepler)

(badges :kepler)

;; ## 1797: the *Théorie des fonctions analytiques*
;;
;; In the 1790s Lagrange taught at the new École polytechnique. He wanted the
;; calculus without infinitely small quantities and without limits, built on
;; power series alone. In article 52 of the *Théorie des fonctions
;; analytiques* he states what we now call the **mean value theorem** and the
;; **Lagrange form of the Taylor remainder**:

(let [t (view/mean-value-text)]
  (quote-block (:original t) (:english t) (:source t)))

;; The first line, $f(x) = f(0) + x f'(u)$, says that between two points of a
;; curve there is a point where the tangent is parallel to the chord. The
;; later lines bound the error of a truncated Taylor series. Lagrange drew no
;; picture; here is the secant and its parallel tangent.

(view/mean-value)

(badges :mean-value)
(badges :remainder)

;; ## After Lagrange
;;
;; Lagrange died in Paris in 1813. Augustin-Louis Cauchy, his younger
;; colleague at the École polytechnique, kept the theorem and changed its
;; foundation: in his *Résumé des leçons* of 1823 he proved it from limits and
;; continuity, which is the proof taught today. That is the next notebook in
;; this series. In mechanics, William Rowan Hamilton (1834) rewrote
;; Lagrange's equations in positions and momenta, the form quantum mechanics
;; would inherit.

^{::clerk/no-cache true}
(page/prev-next "notebooks/lagrange.clj")
