;; # Noether, Invariante Variationsprobleme: every symmetry has its conserved quantity
;;
;; On 26 July 1918 Felix Klein presented to the Royal Society of Sciences at
;; Göttingen a paper by Emmy Noether, then thirty-six and, as a woman, not
;; allowed to hold a chair. The final version went to print at the end of
;; September, in the *Nachrichten* of the mathematical-physical class, pages
;; 235 to 257. Its question came from Hilbert and Klein's work on Einstein's
;; general relativity of 1915: why does the energy of that theory behave so
;; differently from the energy of classical mechanics? Noether's answer is
;; two theorems about any problem of the calculus of variations that a group
;; of transformations leaves unchanged. The first is the one physics now
;; calls *Noether's theorem*: each continuous symmetry of the action gives a
;; conserved quantity.
;;
;; We quote her German from the transcription on German Wikisource (from
;; the Commons scan of the *Nachrichten*), with our own English beside it.
;;
;; **Notation.** $t$ is time, $q(t)$ a path of positions, $\dot q$ its
;; velocity. A *Lagrangian* $L(t, q, \dot q)$ is a number computed from a
;; time, a position and a velocity, and the *action* of a path is
;; $I = \int L\,dt$. The *Lagrange expressions* $\psi_i$ are the left sides
;; of Lagrange's equations,
;; $\psi_i = \partial L/\partial q_i - \frac{d}{dt}\,\partial L/\partial \dot q_i$,
;; and the motions are the paths with $\psi = 0$. **proved** on a badge
;; means [Emmy](https://github.com/mentat-collective/emmy) simplified a
;; difference to exactly $0$; **numeric** means a number from raster was
;; within the stated bound.
^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns noether-1918
  {:history/year 1918
   :history/title "Noether, Invariante Variationsprobleme: symmetry and conservation"
   :history/era "Structures, foundations and computation"
   :history/fields #{:mechanics-physics :analysis :abstract-algebra}
   :nextjournal.clerk/visibility {:code :hide :result :show}
   :nextjournal.clerk/toc true}
  (:require [alexandria.noether.conservation :as noether]
            [alexandria.noether.orbit :as orbit]
            [alexandria.noether.widgets :as w]
            [emmy.env :as e]
            [history-of-math.lens :as lens]
            [history-of-math.page :as page]
            [history-of-math.widgets :as hom]
            [nextjournal.clerk :as clerk]))

^{::clerk/visibility {:code :hide :result :hide}}
(hom/install!)

;; ## The programme, in her words (§ 0, p. 235)
;;
;; The paper opens by naming the two methods it joins: the formal calculus
;; of variations of Euler and Lagrange, and Sophus Lie's theory of
;; continuous groups, the same groups Klein's
;; [Erlangen programme](../klein-erlangen/) of 1872 put at the centre of
;; geometry.

(page/blockquote
 "Es handelt sich um Variationsprobleme, die eine kontinuierliche Gruppe (im Lieschen Sinne) gestatten; die daraus sich ergebenden Folgerungen für die zugehörigen Differentialgleichungen finden ihren allgemeinsten Ausdruck in den in § 1 formulierten, in den folgenden Paragraphen bewiesenen Sätzen."
 "Noether, *Invariante Variationsprobleme*, Nachr. Ges. Wiss. Göttingen, Math.-phys. Kl. (1918), p. 235. Ours: \"We are concerned with variational problems that admit a continuous group (in Lie's sense); the consequences for the associated differential equations find their most general expression in the theorems formulated in § 1 and proved in the following sections.\"")

(page/blockquote
 "Das folgende beruht also auf einer Verbindung der Methoden der formalen Variationsrechnung mit denen der Lieschen Gruppentheorie."
 "Noether (1918), p. 235. Ours: \"What follows therefore rests on a combination of the methods of the formal calculus of variations with those of Lie's group theory.\"")

;; ## What an invariant integral is (§ 1, pp. 236-237)
;;
;; A group $\mathfrak G_\varrho$ is *finite continuous* when its
;; transformations depend on $\varrho$ essential parameters $\varepsilon$:
;; the translations of time form a $\mathfrak G_1$, the rotations of the
;; plane another. The integral $I$ is *invariant* when the transformed
;; variables $y$, $v$ give the same value over every region:

(page/blockquote
 "Insbesondere wird also ein Integral I eine Invariante der Gruppe wenn eine Relation besteht: I = ∫…∫ f(x, u, ∂u/∂x, …) dx = ∫…∫ f(y, v, ∂v/∂y, …) dy, integriert über ein beliebiges reelles x-Gebiet und das entsprechende y-Gebiet."
 "Noether (1918), p. 237, formula (1). Ours: \"In particular an integral I is an invariant of the group when there is a relation I = ∫ f(x, u, ∂u/∂x, …) dx = ∫ f(y, v, ∂v/∂y, …) dy, integrated over an arbitrary real x-region and the corresponding y-region.\"")

;; ## The two theorems (§ 1, pp. 238-239)

(page/blockquote
 "I. Ist das Integral I invariant gegenüber einer 𝔊_ϱ, so werden ϱ linear-unabhängige Verbindungen der Lagrangeschen Ausdrücke zu Divergenzen — umgekehrt folgt daraus die Invarianz von I gegenüber einer 𝔊_ϱ."
 "Noether (1918), pp. 238-239. Ours: \"I. If the integral I is invariant under a 𝔊_ϱ, then ϱ linearly independent combinations of the Lagrange expressions become divergences, and conversely this implies the invariance of I under a 𝔊_ϱ.\"")

(page/blockquote
 "II. Ist das Integral I invariant gegenüber einer 𝔊_∞ϱ, in der die willkürlichen Funktionen bis zur σten Ableitung auftreten, so bestehen ϱ identische Relationen zwischen den Lagrangeschen Ausdrücken und ihren Ableitungen bis zur σten Ordnung; auch hier gilt die Umkehrung."
 "Noether (1918), p. 239. Ours: \"II. If the integral I is invariant under a 𝔊_∞ϱ in which the arbitrary functions occur up to their σ-th derivative, then there are ϱ identities between the Lagrange expressions and their derivatives up to order σ; here too the converse holds.\"")

;; With one independent variable, time, a divergence is a total derivative
;; $\frac{d}{dt}B$. On a motion $\psi = 0$, so $\frac{d}{dt}B = 0$ and $B$ is
;; constant. Noether says so herself:

(page/blockquote
 "so sagt Satz I im eindimensionalen Fall — wo die Divergenz in ein totales Differential übergeht — die Existenz von ϱ ersten Integralen aus, … im mehrdimensionalen Fall erhält man die neuerdings oft als „Erhaltungssätze“ bezeichneten Divergenzgleichungen"
 "Noether (1918), p. 239. Ours: \"in the one-dimensional case, where the divergence becomes a total differential, Theorem I asserts the existence of ϱ first integrals; in the many-dimensional case one obtains the divergence equations recently often called 'conservation laws'.\"")

;; ## Time gives energy, rotation gives angular momentum
;;
;; Take a point of mass $m$ in the plane, at $q = (x, y)$, pulled towards
;; the origin by a force that depends only on the distance
;; $r = \sqrt{x^2 + y^2}$. Its Lagrangian is
;;
;; $$L = \tfrac12 m(\dot x^2 + \dot y^2) - V(r),$$
;;
;; with $V$ any function of one variable (for Newton's gravity
;; $V = -GMm/r$). The momentum is $p = \partial L/\partial \dot q = m\dot q$.
;; Two one-parameter groups leave the action unchanged.
;;
;; **Time translation** $t \mapsto t + \varepsilon$: $L$ does not contain
;; $t$. Noether's charge is the energy $E = p\cdot\dot q - L$, which here is
;; $\tfrac12 m|\dot q|^2 + V(r)$, and for *every* path, solution or not,
;;
;; $$\frac{dE}{dt} = -\,\dot q\cdot\psi .$$
;;
;; **Rotation** by an angle $\varepsilon$ about the origin: $r$ and
;; $|\dot q|$ do not change. The rotation moves $q$ in the direction
;; $\delta q = (-y, x)$, the charge is $p\cdot\delta q$, the angular momentum
;; $J = x p_y - y p_x$, and for every path
;;
;; $$\frac{dJ}{dt} = -\,(x\psi_y - y\psi_x).$$
;;
;; These are Noether's "linear combinations of the Lagrange expressions that
;; become divergences": $\dot q\cdot\psi$ and $x\psi_y - y\psi_x$ are total
;; derivatives. On a motion $\psi = 0$, so $E$ and $J$ stay constant: this
;; is the first integral of her one-dimensional case. Emmy derives both
;; identities from the Lagrangian, with $x$, $y$, $V$ left as unknown
;; functions (`alexandria.noether.conservation`, Emmy's
;; `Lagrange-equations` and `Lagrangian->energy`):

(page/grade-badges (noether/graded) #{:grade/proved})

;; Break one symmetry and the matching identity acquires a remainder, while
;; the other is untouched (the last two badges above). Let the strength of
;; gravity change in time, $L = \tfrac12 m|\dot q|^2 + k(t)/r$: the time
;; translation is lost, rotation is kept. Add to the central potential a
;; term $W(x)$ in $x$ alone: rotation is lost, time translation is kept.
;; Emmy computes what each leaks:

(clerk/tex (str "\\frac{dE}{dt} + \\dot q\\cdot\\psi = "
                (e/->TeX (noether/energy-leak))
                ",\\qquad \\frac{dJ}{dt} + (x\\psi_y - y\\psi_x) = "
                (e/->TeX (noether/torque-leak))))

;; The energy changes at the rate $-\dot k/r$: the work done by changing the
;; law itself. The angular momentum changes by $y\,W'(x)$, the torque of the
;; force $-W'(x)$ along the $x$-axis. Neither depends on the path being a
;; motion: they are identities, as Noether's theorem says they must be.

;; ## An orbit that keeps, and an orbit that loses
;;
;; The same three Lagrangians, now solved. Unit mass, strength $1$, so
;; $V = -1/r$; the planet starts at perihelion of an ellipse of
;; eccentricity $0.5$, where $E = -3/8$ and $J = 1$. Emmy turns each
;; Lagrangian into its equations of motion (`Lagrangian->state-derivative`),
;; raster integrates them with RK4 over 24 time units (about four
;; revolutions), and raster kernels evaluate $E$ and $J$ at every sample
;; (`alexandria.noether.orbit`). The dashed line is the starting value, and
;; *spread* is the largest minus the smallest value along the run.
;;
;; Choose a case. With both symmetries both curves lie on their dashed
;; lines. Let gravity grow in time, $k(t) = 1 + \varepsilon t$: the orbit
;; shrinks, the energy falls, and the angular momentum does not move, since
;; the law is still the same in every direction. Add the term
;; $\varepsilon x^2/2$ instead, a pull towards the $y$-axis that the
;; rotations do not respect: the ellipse turns and changes shape, $J$ swings,
;; and $E$ stays on its line, since the law is still the same at every time.
;; The selector offers $\varepsilon$ from $0.005$ to $0.02$. Much beyond that
;; the $x^2$ term drives $J$ through zero, the planet falls into the centre,
;; where $1/r$ is infinite, and no fixed-step integrator keeps anything.

(w/orbit-widget)

;; What the widget shows, as graded claims (raster RK4 at a step of
;; $0.0075$; the bound $10^{-6}$ is the integrator's error, the threshold
;; $0.01$ a change no integrator error explains):

(page/grade-badges (orbit/graded) #{:grade/numeric})

;; ## Energy in general relativity (§ 6, p. 255)
;;
;; Theorem II is the reason energy in Einstein's theory is not like energy
;; in Lagrange's mechanics. The group of *all* coordinate changes is
;; infinite, and when the translations sit inside such a group the energy
;; relations become *improper* (uneigentlich): identities that hold on every
;; path, not only on the motions.

(page/blockquote
 "Gestattet I die Verschiebungsgruppe, so werden die Energierelationen dann und nur dann uneigentliche, wenn I invariant ist gegenüber einer unendlichen Gruppe, die die Verschiebungsgruppe als Untergruppe enthält."
 "Noether (1918), § 6, p. 255. Ours: \"If I admits the group of translations, then the energy relations become improper if and only if I is invariant under an infinite group that contains the group of translations as a subgroup.\" Her footnote: the energy laws of classical mechanics are proper, since no infinite groups occur there.")


;; ## After Noether
;;
;; [Lagrange's](../lagrange/) energy integral and his cyclic angle in the
;; Kepler problem were the two examples Noether's theorem explains, and
;; Kepler's equal areas are the angular momentum of the
;; [orbits of 1609 and 1619](../kepler-laws/). In physics the theorem runs
;; the other way: a theory is now specified by its symmetry group, and its
;; conservation laws follow. Charge is the Noether charge of a phase
;; symmetry, and Theorem II became the identities of gauge theory. Klein's
;; [Erlangen programme](../klein-erlangen/) read geometry the same way. A
;; *Mathematics of physics* series, planned, will follow the thread from
;; Lagrange through Hamilton (1834) to the gauge fields.

^{::clerk/no-cache true}
(page/prev-next "notebooks/noether_1918.clj")

(lens/section "noether-1918")
