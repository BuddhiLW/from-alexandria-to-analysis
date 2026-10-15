;; # Riemann: the hypotheses on which geometry is founded
;;
;; Bernhard Riemann, born in 1826 in Breselenz near Hanover, the son of a
;; Lutheran pastor, studied at Goettingen and Berlin and took his doctorate
;; under Gauss in 1851. To lecture at Goettingen he had to give a trial
;; lecture, the *Habilitationsvortrag*, before the faculty. He offered three
;; topics; Gauss, against custom, chose the third, the foundations of
;; geometry. Riemann gave it on 10 June 1854, *Ueber die Hypothesen, welche
;; der Geometrie zu Grunde liegen*, with almost no formulas, for an audience
;; of philosophers and historians as much as mathematicians. Gauss, then
;; seventy-seven, is said to have walked home moved. The text was printed
;; only in 1868, two years after Riemann's death, by Dedekind (*Abhandlungen
;; der Koeniglichen Gesellschaft der Wissenschaften zu Goettingen* 13). W. K.
;; Clifford put it into English in *Nature* in 1873.
;;
;; The lecture opens a new era of this series: mathematics turns from
;; computing objects to studying the *structures* they live in. Here the
;; structure is space itself. Euclid took it as given; Riemann asks what
;; must be given for there to be a geometry at all, and finds that the
;; answer is a rule for measuring the length of an infinitely small step.
;;
;; We quote Clifford's English (*Nature* 8, 1873, pp. 14-17 and 36-37) and
;; Riemann's German of 1868 by part and section. The figures and checks
;; come from Alexandria's shelf `alexandria.riemann-geometry`: *proved*
;; means Emmy simplified a difference to exactly 0, *numeric* that a
;; number from raster was within the stated bound.
^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns riemann-hypothesen
  {:history/year 1854
   :history/title "Riemann: the hypotheses on which geometry is founded"
   :history/era "Structures, foundations and computation"
   :history/fields #{:geometry :differential-geometry :mechanics-physics}
   :nextjournal.clerk/visibility {:code :hide :result :show}
   :nextjournal.clerk/toc true}
  (:require [alexandria.riemann-geometry.geodesic :as geodesic]
            [alexandria.riemann-geometry.geodesic-widget :as gw]
            [alexandria.riemann-geometry.metric :as metric]
            [alexandria.riemann-geometry.metric-widget :as mw]
            [history-of-math.lens :as lens]
            [history-of-math.page :as page]
            [history-of-math.widgets :as hom]
            [nextjournal.clerk :as clerk]))

^{::clerk/visibility {:code :hide :result :hide}}
(hom/install!)

;; ## Geometry rests on hypotheses (Plan of the investigation)
;;
;; Riemann begins by saying what is wrong with the foundations. Geometry
;; presupposes space and the first principles of construction, and the
;; relation between them stays in the dark:

(page/blockquote
 "It is known that geometry assumes, as things given, both the notion of space and the first principles of constructions in space. She gives definitions of them which are merely nominal, while the true determinations appear in the form of axioms."
 "Riemann, *On the Hypotheses which lie at the Bases of Geometry*, tr. W. K. Clifford, *Nature* 8 (1873), Plan of the Investigation.")

;; The axioms, he goes on, are not necessary truths about every possible
;; space: they single out ours among many that are thinkable, and so

(page/blockquote
 "they are not necessary, but only of empirical certainty; they are hypotheses."
 "Clifford's translation, Plan of the Investigation. Riemann: *\"nicht nothwendig, sondern nur von empirischer Gewissheit, sie sind Hypothesen\"* (*Abh. Ges. Wiss. Göttingen* 13, 1868, Plan der Untersuchung).")

;; ## The line element ds² = Σ g_ij dx_i dx_j (Part II, §1)
;;
;; Part I builds the notion of an *n-fold extended manifoldness*: a set
;; whose points are fixed by $n$ continuous numbers $x_1, \ldots, x_n$, the
;; surface for $n = 2$, our space for $n = 3$, but also the colours or any
;; family of quantities. Nothing yet says how far apart two points are.
;; Part II asks for the simplest way to measure. A line is measured by
;; adding the lengths of its infinitely small pieces, so what is needed is
;; the length $ds$ of a step $dx_1, \ldots, dx_n$ from the point $x$. If
;; the length does not depend on the direction being reversed, and grows in
;; proportion when all the $dx$ grow in proportion, Riemann argues that the
;; simplest case is

(page/blockquote
 "ds is the square root of an always positive integral homogeneous function of the second order of the quantities dx, in which the coefficients are continuous functions of the quantities x. For Space, when the position of points is expressed by rectilinear co-ordinates, ds = √Σ(dx)²; Space is therefore included in this simplest case."
 "Clifford's translation, II §1. Riemann: *\"ds = der Quadratwurzel aus einer immer positiven ganzen homogenen Function zweiten Grades der Grössen dx, in welcher die Coefficienten stetige Functionen der Grössen x sind\"* (1868, II §1).")

;; In today's letters, with the coefficients $g_{ij}(x) = g_{ji}(x)$,
;;
;; $$ds^2 = \sum_{i,j=1}^{n} g_{ij}(x)\,dx_i\,dx_j,$$
;;
;; which for $n = 2$ is Gauss' $E\,du^2 + 2F\,du\,dv + G\,dv^2$, with one
;; change of view: Gauss computed $E, F, G$ from a surface lying in space,
;; Riemann starts from the $g_{ij}$ and needs no surrounding space at all.
;; "Always positive" is the hypothesis $\det g > 0$ (with $g_{11} > 0$):
;; every step has a positive length.
;;
;; Different coordinates give different $g_{ij}$ for the same geometry, so
;; Riemann asks what does not depend on the coordinates. Of the
;; $n(n+1)/2$ functions $g_{ij}$, $n$ can be changed at will by a change of
;; coordinates; what remains is the curvature, Gauss' measure carried to
;; every surface direction through a point (II §2). He then writes the
;; manifolds of constant curvature $\alpha$ in a normal form (II §4):
;;
;; $$ds = \frac{\sqrt{\sum dx^2}}{1 + \frac{\alpha}{4}\sum x^2}.$$
;;
;; Emmy checks it with Gauss' formula (the same `brioschi` the Gauss
;; notebook uses for the *Theorema egregium*), in Riemann's coordinates and
;; in skew ones:

(page/grade-badges (metric/graded) #{:grade/proved :grade/numeric})

;; Now try it yourself: the metric on the page is Riemann's normal form in
;; skew coordinates, $g = \lambda(x)\,A$ with a constant form
;; $A = \begin{pmatrix} g_{11} & g_{12}\\ g_{12} & g_{22}\end{pmatrix}$ and
;; $\lambda = (1 + h e^{-Q})/(1 + \frac{\alpha}{4}Q)^2$,
;; $Q = A(x, x)$. Edit $g_{11}, g_{12}, g_{22}$, or the determinant $\det A$
;; (tied: moving it re-solves $g_{22}$), Riemann's $\alpha$, and a hill $h$
;; that is not in Riemann's normal form. The colour is $K$ at every cell,
;; from the $g_{ij}$ alone; each grey curve is the set of points one small
;; step $ds$ away, so you see the metric as Riemann defines it. Drag $P$.
;; At $h = 0$ the badge stays green whatever $A$ and $P$: constant
;; curvature $\alpha$. Raise the hill and $K$ varies from point to point.
;; Push $\det A$ to $0$ or below, and there is no line element left.

(mw/playground)

;; ## Shortest lines and the spreading of geodesics (Part II, §2-§3)
;;
;; With a line element, the shortest line between two nearby points is a
;; matter of the calculus of variations. Riemann uses these *geodesics*
;; from a point $O$ as coordinates and describes the curvature by them: take
;; the geodesics from $O$ in the directions of one surface element; they
;; sweep out a surface, and its Gauss curvature at $O$ is the curvature of
;; the manifold in that surface direction:

(page/blockquote
 "It is obviously zero when the manifold represented is flat, i.e. when the squared line-element is reducible to Σdx², and may therefore be regarded as the measure of the deviation of the manifoldness from flatness at the given point in the given surface-direction. Multiplied by −¾ it becomes equal to the quantity which Privy-councillor Gauss has called the total curvature of a surface."
 "Clifford's translation, II §2. (The factor −¾ comes from the way Riemann normalises the quadric; in today's terms the quantity is the sectional curvature.)")

;; Today the geodesic equation is written with the Christoffel symbols
;; (Christoffel, 1869, two years after the lecture was printed):
;;
;; $$\ddot x^k + \Gamma^k_{ij}\,\dot x^i \dot x^j = 0,\qquad
;; \Gamma^k_{ij} = \tfrac12 g^{kl}\left(\partial_i g_{jl} + \partial_j g_{il} - \partial_l g_{ij}\right).$$
;;
;; Emmy derives the $\Gamma$ from the metric alone. For the torus of the
;; playground below:

(gw/derivation)

(page/grade-badges (geodesic/graded) #{:grade/proved :grade/numeric})

;; Positive curvature pulls geodesics from one point together again, as
;; the meridians meet at the pole; negative curvature drives them apart
;; faster than straight lines. That is how a being inside the surface
;; could measure $K$ without leaving it.
;;
;; Now try it yourself: pick a surface, drag the start $P$ and the violet
;; handle in the parameter chart (or use the sliders for the heading
;; $\theta$, the half opening $\delta$ and the length $L$). Each of the two
;; geodesics is integrated in your browser by RK4 over Emmy's Christoffel
;; symbols. The page compares the gap between their ends with $2L\sin\delta$,
;; the gap straight lines would have, and names the case. On the torus the
;; same opening converges outside and diverges in the hole.

(gw/playground)

;; ## n dimensions, and whether space is curved (Part III)
;;
;; Nothing in Part II depended on $n = 2$. In $n$ dimensions the curvature
;; is given by its values in $n(n-1)/2$ surface directions at each point,
;; and Riemann says that a manifold whose curvature is everywhere zero is
;; flat, Euclid's. The third part applies all this to the space we live in.
;; Its unboundedness, he says, is more certain than any experience, but
;; infinite extent does not follow: with a constant positive curvature it
;; would be finite. Which geometry holds is for measurement to decide:

(page/blockquote
 "If we suppose that bodies exist independently of position, the curvature is everywhere constant, and it then results from astronomical measurements that it cannot be different from zero; or at any rate its reciprocal must be an area in comparison with which the range of our telescopes may be neglected."
 "Clifford's translation, III §2, *Nature* 8 (1873).")

;; And in the small, where solid bodies and light rays may lose their
;; meaning, the metric might have another ground:

(page/blockquote
 "Either therefore the reality which underlies space must form a discrete manifoldness, or we must seek the ground of its metric relations outside it, in binding forces which act upon it."
 "Clifford's translation, III §3. Riemann: *\"oder es muss der Grund der Massverhältnisse ausserhalb, in darauf wirkenden bindenden Kräften, gesucht werden\"* (1868, III §3).")

;; ## Where this goes
;;
;; Christoffel (1869) and Lipschitz gave the calculus of the $g_{ij}$; Ricci
;; and Levi-Civita (1900) made it the tensor calculus. Beltrami (1868) had
;; already shown that Lobachevsky's geometry is a surface of constant
;; negative curvature, Riemann's $\alpha < 0$; Klein's Erlangen programme
;; (1872) took the other road, geometry by groups. In 1915 Einstein used
;; Riemann's geometry, with a metric of signature $(-,+,+,+)$ on spacetime,
;; for gravity: the $g_{ij}$ are determined by matter, Riemann's "binding
;; forces", and the curvature of space became the question Riemann said it
;; was, an empirical one, answered by the bending of starlight in 1919.

^{::clerk/no-cache true}
(page/prev-next "notebooks/riemann_hypothesen.clj")

(lens/section "riemann-hypothesen")
