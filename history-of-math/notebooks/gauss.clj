;; # Gauss: the 17-gon and the curvature a surface cannot lose
;;
;; Carl Friedrich Gauss was born in Brunswick in 1777, the son of a
;; bricklayer. The Duke of Brunswick paid for his schooling and for his
;; years at Goettingen. On 30 March 1796, aged eighteen, he found that the
;; circle can be divided into seventeen equal parts with ruler and compass,
;; the first new regular polygon since Euclid. It is the first entry of the
;; diary he kept from that day on, and he asked for a 17-gon on his
;; tombstone. Five years later the result closed his *Disquisitiones
;; arithmeticae* (Leipzig, 1801), the book that founded modern number
;; theory.
;;
;; From 1807 he directed the Goettingen observatory. In the 1820s he
;; surveyed the Kingdom of Hanover by triangulation, measuring angles
;; between church towers and hilltops, and from that practice came the
;; *Disquisitiones generales circa superficies curvas* (Goettingen, 1828,
;; presented to the Society in October 1827). There he measured the
;; curvature of a surface from inside it, and found that it does not change
;; when the surface is bent: the *Theorema egregium*. He died in Goettingen
;; in 1855.
;;
;; We quote his Latin from the first printings (art. 365 of the
;; *Disquisitiones arithmeticae*, art. 12 of the *Disquisitiones
;; generales*) and Poullet-Delisle's French of 1807 (*Recherches
;; arithmetiques*, Wikisource), and give our English beside them, marked as
;; ours. The figures, numbers and checks come from Alexandria's Gauss shelf
;; (`alexandria.gauss.heptadecagon`, `alexandria.gauss.egregium`): *proved*
;; means Emmy simplified a difference to exactly 0, *numeric* that a
;; floating-point value from raster was within a stated bound.
^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns gauss
  {:history/year 1801
   :history/title "Gauss: the regular 17-gon and the Theorema egregium"
   :history/era "Analysis made rigorous"
   :history/fields #{:number-theory :algebra :differential-geometry :geometry}
   :nextjournal.clerk/visibility {:code :hide :result :show}
   :nextjournal.clerk/toc true}
  (:require [alexandria.gauss.egregium :as egregium]
            [alexandria.gauss.egregium-widget :as egw]
            [alexandria.gauss.heptadecagon :as h]
            [alexandria.gauss.widgets :as widgets]
            [history-of-math.lens :as lens]
            [history-of-math.page :as page]
            [history-of-math.widgets :as hom]
            [nextjournal.clerk :as clerk]))

^{::clerk/visibility {:code :hide :result :hide}}
(hom/install!)

;; ## The regular 17-gon (1796; *Disquisitiones arithmeticae* VII, 1801)
;;
;; Euclid constructs the triangle, the square, the pentagon (IV.11) and the
;; 15-gon (IV.16), and bisection doubles any of them. For two thousand
;; years nobody added another. Gauss turned the question into algebra: the
;; vertices of the regular $n$-gon on the unit circle are the roots of
;; $x^n = 1$, and a length can be drawn with ruler and compass exactly when
;; it is reached from the rationals by $+, -, \times, \div$ and square roots.
;; So the 17-gon is constructible if $\cos(2\pi/17)$ is a nest of square
;; roots. Section VII of the *Disquisitiones* shows that it is, and how.
;;
;; ### The periods
;;
;; Let $r = \cos\frac{2\pi}{17} + i\sin\frac{2\pi}{17}$. The sixteen roots
;; $r, r^2, \ldots, r^{16}$ are permuted by $r \mapsto r^3$, because 3 is a
;; primitive root of 17: its powers run through every residue. Gauss writes
;; the roots in that order and sums every second, every fourth, every eighth
;; of them. These sums are his *periods* (art. 343): two of eight terms, four
;; of four, eight of two. The two periods inside a larger one have a known
;; sum (the larger period) and a product that the multiplication table of
;; the roots expresses in periods already found, so each split is one
;; quadratic equation. Four splits, four square roots, and the last period of
;; two terms is $r + r^{-1} = 2\cos\frac{2\pi}{17}$.
;;
;; Step through the tower. Each step's numbers are the roots of its
;; quadratic, computed by raster; the circle colours the roots by period.

(widgets/periods-player)

;; Gauss prints the result in art. 365 (we copy the French of 1807, which
;; sets the radicals legibly):
;;
;; > *Par exemple, pour n = 17, on tire facilement des nos 354, 361*
;; > *cos P/17 = −1/16 + 1/16 √17 + 1/16 √(34 − 2√17)*
;; > *+ 1/8 √(17 + 3√17 − √(34 − 2√17) − 2√(34 + 2√17))*
;;
;; — Gauss, *Recherches arithmétiques*, tr. Poullet-Delisle (Paris, 1807),
;; art. 365, p. 487 (Wikisource). $P$ is the whole circumference.
;;
;; Emmy builds the same number from the tower, $w_1/2$ with
;; $w_1 = \frac{z_1 + \sqrt{z_1^2 - 4 z_3}}{2}$ and so on down, and the two
;; are graded against $\cos(2\pi/17)$ from raster:

(widgets/derivation)

(page/grade-badges (h/graded) #{:grade/proved :grade/numeric})

;; ### Which polygons? Gauss' answer
;;
;; Gauss saw the general rule. The tower of a prime $p$ has one step for
;; every prime factor of $p - 1$, and a step whose factor is $q$ is an
;; equation of degree $q$. Only when $p - 1$ is a power of 2 is every step
;; a quadratic, and then $p = 2^{2^\nu} + 1$, a Fermat prime:

(page/blockquote
 "Omnes itaque valores ipsius n, pro quibus ad meras aequationes quadraticas deferimur, sub forma 2^(2^ν) + 1 continentur; ita quinque numeri 3, 5, 17, 257, 65537 prodeunt statuendo ν = 0, 1, 2, 3, 4."
 "Gauss, *Disquisitiones arithmeticae* (Leipzig, 1801), art. 365, p. 663. Ours: \"All values of n for which we are led to nothing but quadratic equations are therefore of the form 2^(2^ν) + 1; so the five numbers 3, 5, 17, 257, 65537 come out by putting ν = 0, 1, 2, 3, 4.\"")

;; He adds that Fermat, misled by induction, had held every such number
;; prime, and that Euler found $2^{32} + 1 = 4294967297$ divisible by 641.
;; Then he warns the reader off:

(page/blockquote
 "ne quis adhuc alias sectiones praeter eas quas theoria nostra suggerit, e. g. sectiones in 7, 11, 13, 19 etc. partes, ad constructiones geometricas perducere speret, tempusque inutiliter terat."
 "Gauss, *Disquisitiones arithmeticae* (1801), art. 365, p. 664. Ours: \"so that nobody should hope to bring other divisions than those our theory suggests, e.g. into 7, 11, 13, 19 etc. parts, to geometric constructions, and waste his time.\"")

;; Gauss asserted the necessity but did not print its proof. Pierre Wantzel
;; did, in 1837 (*Journal de mathématiques pures et appliquées* 2,
;; pp. 366-372), together with the impossibility of doubling the cube and
;; trisecting the angle: a constructible number has degree a power of 2
;; over the rationals. With art. 366 for prime powers, the rule is: the
;; regular $n$-gon is constructible exactly when $n = 2^a p_1 \cdots p_m$
;; with distinct Fermat primes $p_i$.
;;
;; Now try it yourself: pick any $n$ up to 100 (or drag $P_1$ round the
;; circle). The page factors $n$, names the case and the obstruction, and
;; shows the tower of the prime that decides it, each step's degree marked,
;; red where it is not 2. For a prime $n$ the vertices take the colours of
;; the periods at the level you choose.

(widgets/n-gon)

;; ## The Theorema egregium (1827; *Disquisitiones generales* 1828)
;;
;; Gauss gives a surface by two parameters $p, q$: each point is
;; $(x, y, z)$ as a function of them. The length of a small step on the
;; surface is then (art. 11, in today's letters $u, v$)
;;
;; $$ds^2 = E\,du^2 + 2F\,du\,dv + G\,dv^2,\qquad E = r_u\cdot r_u,\; F = r_u\cdot r_v,\; G = r_v\cdot r_v.$$
;;
;; $E, F, G$ are what a surveyor on the surface can measure: lengths and
;; angles along it, with no view of the space around it. The curvature is a
;; different matter. Gauss defines it (art. 6) by the Gauss map, the
;; normal at each point carried to the unit sphere, as the ratio of the
;; sphere's area to the surface's area; that looks outside. In art. 11 he
;; computes it anyway, and the result contains only $E, F, G$ and their
;; first and second derivatives. Art. 12 draws the consequence:

(page/blockquote
 "Si superficies curva in quamcunque aliam superficiem explicatur, mensura curvaturae in singulis punctis invariata manet."
 "Gauss, *Disquisitiones generales circa superficies curvas* (Göttingen, 1828), art. 12, p. 24. Ours: \"If a curved surface is developed onto any other surface whatever, the measure of curvature at each point stays unchanged.\"")

;; *Explicare*, to unfold, is his word for bending without stretching: a
;; map that keeps $E, F, G$. He called it *egregium*, outstanding, and
;; added (same article) that the flat-unfoldable surfaces of the
;; geometers before him are exactly those with curvature 0 everywhere.
;;
;; Today Gauss' formula is usually written in Brioschi's arrangement
;; (1852). With subscripts for derivatives,
;;
;; $$K = \frac{\begin{vmatrix} -\tfrac12 E_{vv} + F_{uv} - \tfrac12 G_{uu} & \tfrac12 E_u & F_u - \tfrac12 E_v\\ F_v - \tfrac12 G_u & E & F\\ \tfrac12 G_v & F & G\end{vmatrix} - \begin{vmatrix} 0 & \tfrac12 E_v & \tfrac12 G_u\\ \tfrac12 E_v & E & F\\ \tfrac12 G_u & F & G\end{vmatrix}}{(EG - F^2)^2}.$$
;;
;; Alexandria computes $K$ both ways: this formula, fed nothing but $E, F,
;; G$, and the outside way, $(LN - M^2)/(EG - F^2)^2$ with the second
;; derivatives of the embedding against the normal. The two agree.
;;
;; ### A bending you can watch
;;
;; The helicoid (a spiral staircase) bends into the catenoid (the soap film
;; between two rings) through a family of surfaces with the same $E, F, G$:
;;
;; $$r_t(u, v) = \cos t\,(\sinh v \sin u,\, -\sinh v \cos u,\, u) + \sin t\,(\cosh v \cos u,\, \cosh v \sin u,\, v).$$
;;
;; Emmy differentiates it. $E = G = \cosh^2 v$ and $F = 0$ come out for
;; every $t$, so Brioschi's formula gives the same $K = -1/\cosh^4 v$ for
;; every $t$. Stretch the axis by $s$ instead (multiply the third coordinate
;; by $s$) and $E, F, G$ change, and $K$ with them:

(egw/derivation)

(page/grade-badges (egregium/graded) #{:grade/proved :grade/numeric})

;; Now try it yourself. Bend the helicoid into the catenoid, or roll a flat
;; sheet into a half cylinder. Move the point $P$ in the parameter square
;; (Gauss' $p, q$) or with the sliders. Beside the 3D view (orbitable) the
;; page shows $E, F, G$ at $P$, the curvature from them alone, the same from
;; the embedding, and the curvature the unbent surface had at the same
;; point. The badge stays green through any bending. The stretch $s$ is not
;; a bending, and the badge turns red as soon as you use it. The violet
;; curve in the square is the first fundamental form at $P$, the dashed one
;; that of the unbent surface.

(egw/playground)

;; ## Where this goes
;;
;; The 17-gon opened Galois' theory: the periods are the steps of a chain
;; of subgroups of the cyclic group of order 16, and Galois (1832) turned
;; "solvable by radicals" into a property of groups. Wantzel's 1837 proof
;; settled the three classical problems, the duplication of the cube among
;; them.
;;
;; The *Theorema egregium* made the surface a world of its own, measured
;; from inside. Gauss' student Riemann, in his Habilitation lecture of 1854,
;; took the first fundamental form to any number of dimensions and asked
;; which geometry the physical space has. Einstein's general relativity
;; (1915) is that question answered with a metric of spacetime. And every
;; flat map of the Earth distorts: the sphere has $K = 1/R^2$, the plane 0,
;; and no bending turns one into the other.

^{::clerk/no-cache true}
(page/prev-next "notebooks/gauss.clj")

(lens/section "gauss")
