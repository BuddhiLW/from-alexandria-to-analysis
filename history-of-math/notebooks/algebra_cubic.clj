;; # From the square to the cube: al-Khwarizmi, Cardano's *Ars magna* and Bombelli
;;
;; Around 820 Muhammad ibn Musa al-Khwarizmi, a scholar of the House of
;; Wisdom in Baghdad, wrote a short book on calculation by *al-jabr*
;; (restoring) and *al-muqabala* (balancing). It sorts every equation of
;; the second degree into six types, solves each by a rule, and proves the
;; rule by a figure. From its title the subject took its name; from his,
;; the word algorithm.
;;
;; Seven centuries later the cubic fell. Scipione del Ferro of Bologna
;; solved the cube and things equal to a number around 1515 and told only
;; his pupil; Niccolò Tartaglia found it again in 1535 to win a public
;; contest; Girolamo Cardano of Milan, physician, gambler and
;; mathematician, obtained the rule from him under a promise of secrecy,
;; found a proof, and printed it in his *Ars magna* (Nuremberg, 1545), with
;; credit to both. The rule hid a scandal: for some cubics with three real
;; roots it asks for the square root of a negative number. Rafael Bombelli
;; (*L'Algebra*, Bologna, 1572) learned to calculate with such roots and
;; showed that they cancel, leaving the real root.
;;
;; We quote al-Khwarizmi in Frederic Rosen's English (*The Algebra of
;; Mohammed ben Musa*, London, 1831) and Cardano in his own Latin, read
;; against the scan of the 1545 printing, with our English beside it,
;; marked as ours. Bombelli's book is not in our corpus yet: we state his
;; equation and quote nothing. The figures, numbers and checks come from
;; Alexandria's algebra shelf (`alexandria.algebra.cubic`): *proved* means
;; Emmy simplified a difference to exactly 0, *numeric* that numbers from
;; raster agreed within a stated bound.
^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns algebra-cubic
  {:history/year 1545
   :history/title "From the square to the cube: al-Khwarizmi, Cardano's Ars magna and Bombelli"
   :history/era "From Oresme to Copernicus"
   :history/fields #{:algebra :geometry}
   :nextjournal.clerk/visibility {:code :hide :result :show}
   :nextjournal.clerk/toc true}
  (:require [alexandria.algebra.cubic :as cubic]
            [alexandria.algebra.cubic-widgets :as w]
            [history-of-math.lens :as lens]
            [history-of-math.page :as page]
            [history-of-math.widgets :as hom]
            [nextjournal.clerk :as clerk]))

^{::clerk/visibility {:code :hide :result :hide}}
(hom/install!)

;; ## A square and ten roots (al-Khwarizmi, c. 820)
;;
;; Al-Khwarizmi writes no symbols. His unknown is the *root* (*jidhr*), its
;; square the *square* (*mal*), and a plain number is counted in
;; *dirhems*. His fourth type, "squares and roots equal to numbers", is our
;; $x^2 + bx = c$, and his example is the most copied equation of the
;; Middle Ages:

(page/blockquote
 "One square, and ten roots of the same, amount to thirty-nine dirhems; that is to say, what must be the square which, when increased by ten of its own roots, amounts to thirty-nine?"
 "al-Khwarizmi, *The Algebra of Mohammed ben Musa*, ed. and tr. F. Rosen (London, 1831), p. 8")

;; The rule follows at once: halve the roots, square the half, add it to
;; the number, take the root, subtract the half. Then he proves it twice,
;; by two figures. Step through the second, one of his sentences at a
;; time. The square $AB$ is the unknown square; on two of its sides stand
;; the rectangles $G$ and $D$, each five long; the corner they leave is
;; five by five; the completed figure is the great square $SH$.
;;
;; The sliders are tied. Move the roots $b$ or the number $c$ and the root
;; $x$ follows (forward: $x = \sqrt{c + (b/2)^2} - b/2$); move $x$ and the
;; number follows ($c = x^2 + bx$), the roots held. The figure, the
;; formula with its numbers and the check are read from one raster kernel.

(w/square-widget)

;; Emmy completes the square, solves it, and composes the forward map with
;; the inverse:

(w/derivation :square #"corner square|forward then inverse is|al-Khwarizmi")

;; Al-Khwarizmi admits only positive numbers and only the positive root:
;; the square cannot be negative, and the figure has no side of negative
;; length. Of $x^2 + 21 = 10x$ (his next case) he says that it has two
;; positive roots, 3 and 7, and that if the half of the roots squared is
;; less than the number, "the case is impossible". That impossibility is
;; where this notebook ends up, one degree higher.

;; ## The cube and things equal to a number (Cardano, *Ars magna* XI, 1545)
;;
;; Cardano writes the cube $x^3$, the *things* (*res*, our $x$) and the
;; number. His chapter XI treats *de cubo & rebus aequalibus numero*,
;; $x^3 + px = q$, and opens with its history:

(page/blockquote
 "Scipio Ferreus Bononiensis iam annis ab hinc triginta ferme capitulum hoc invenit, tradidit vero Anthonio Mariae Florido Veneto, qui cum in certamen cum Nicolao Tartalea Brixellense aliquando venisset, occasionem dedit, ut Nicolaus invenerit et ipse."
 "Cardano, *Artis magnae sive de regulis algebraicis liber unus* (Nuremberg, 1545), ch. XI, fol. 29v (scan p. 62). Ours: \"Scipione del Ferro of Bologna found this chapter some thirty years ago and handed it to Antonio Maria Fiore of Venice, who, when he once came into a contest with Niccolò Tartaglia of Brescia, gave Niccolò the occasion to find it himself.\"")

;; His proof is a figure in space. Take a cube on the line $AC$ and cut
;; the line at $B$ so that $AB$ is the unknown $x$ and $BC = v$; call
;; $AC = u$, so $x = u - v$. Planes through $B$ cut the big cube into the
;; cube of $AB$, the cube of $BC$, and six solids: three slabs $x \cdot x
;; \cdot v$ and three rods $x \cdot v \cdot v$. Choose the two cubes so
;; that their difference is the number and the rectangle $AC \cdot BC$ is
;; a third of the number of things:

(page/blockquote
 "Sit igitur exempli causa cubus GH et sexcuplum lateris GH aequale 20, et ponam duos cubos AE et CL, quorum differentia sit 20, ita quod productum AC lateris in CK latus sit 2, tertia scilicet numeri rerum pars."
 "Cardano, *Ars magna* (1545), ch. XI, fol. 29v. Ours: \"Let then, for example, the cube of GH and six times the side GH be equal to 20; and I take two cubes AE and CL whose difference is 20, such that the product of the side AC by the side CK is 2, namely the third part of the number of things.\"")

;; Then the solids add up exactly to what the equation needs, and $AB = AC
;; - CB$ is the root. The rule he draws from it, with his own example:

(page/blockquote
 "Deducito tertiam partem numeri rerum ad cubum, cui addes quadratum dimidii numeri aequationis, et totius accipe radicem, scilicet quadratam, quam seminabis, unique dimidium numeri quod iam in se duxeras adiicies, ab altera dimidium idem minues, habebisque binomium cum sua apotome."
 "Cardano, *Ars magna* (1545), ch. XI, fol. 30r (scan p. 62). Ours: \"Raise the third part of the number of things to the cube, add to it the square of half the number of the equation, and take the root of the whole, the square root; this you will double, and to one copy add the half of the number you have just multiplied by itself, from the other subtract the same half, and you will have a binomium with its apotome.\"")

;; Subtract the cube root of the apotome from the cube root of the
;; binomium, he concludes, and the remainder is the value of the thing. For
;; "the cube and six things equal twenty" the binomium is $\sqrt{108} + 10$,
;; the apotome $\sqrt{108} - 10$, and
;; $x = \sqrt[3]{\sqrt{108} + 10} - \sqrt[3]{\sqrt{108} - 10} = 2$.
;;
;; In the widget the cube is Cardano's, drawn from $u$ and $v$ of the
;; current $p$ and $q$ (MathBox; drag to turn it). $p$, $q$ and $x$ are
;; tied: $p$ or $q$ moves and the rule gives $x$; $x$ moves and the number
;; follows, $q = x^3 + px$. Beside it the rule with the current numbers
;; substituted, the case of the equation, and the root put back into it.

(w/cardano-widget :cube)

;; Emmy checks the dissection, that $x = u - v$ solves the equation when
;; $uv = p/3$ and $u^3 - v^3 = q$, and that binomium times apotome is the
;; cube of a third of the things. The rule and its inverse, run by raster
;; over a grid of equations, compose to the identity.

(w/derivation :cardano #"solids|u − v solves|binomium|on raster: x")

;; ## The irreducible case (Bombelli, *L'Algebra*, 1572)
;;
;; Now let the things stand on the other side: $x^3 = px + q$, our
;; $x^3 + px = q$ with $p$ negative. The square root in the rule is of
;; $D = (q/2)^2 + (p/3)^3$, and when $p$ is negative enough $D$ is negative.
;; Cardano met it in $x^3 = 15x + 4$: $D = 4 - 125 = -121$, and the rule
;; reads
;;
;; $$x = \sqrt[3]{2 + \sqrt{-121}} + \sqrt[3]{2 - \sqrt{-121}}.$$
;;
;; Yet $x = 4$ is plainly a root ($64 = 60 + 4$), and so are $-2 \pm
;; \sqrt 3$. Cardano left this case aside; the later tradition named it the
;; *casus irreducibilis*. Bombelli took the expression at its word. He
;; wrote $\sqrt{-121} = 11i$ (his *più di meno*), found that
;; $(2 + i)^3 = 2 + 11i$ and $(2 - i)^3 = 2 - 11i$, and added:
;; $(2 + i) + (2 - i) = 4$. The square roots of negatives come in
;; conjugate pairs, and the pair cancels.
;;
;; Slide $p$ down through the point where $D = 0$ and watch the case
;; change. On the complex plane, the two cube roots of the rule are the
;; violet and green points; while $D > 0$ they lie on the real axis, at
;; $u$ and $-v$. Past $D = 0$ they leave it as a conjugate pair, and their
;; sum, by the parallelogram, stays on the real axis at the root $x$. The
;; faint points are the two other cube roots of $q/2 + i\sqrt{-D}$: each
;; pair with its conjugate gives one of the other two real roots.

(w/cardano-widget :plane)

(w/derivation :bombelli #"Bombelli")

;; ## Roots and coefficients (Viète, 1591)
;;
;; The way out of the irreducible case was not a better formula but a
;; change of view: look at all the roots at once. François Viète, writing
;; letters for the known quantities as well as the unknown (*In artem
;; analyticem isagoge*, 1591), saw that the coefficients of an equation are
;; built from its roots: for three roots $r_1, r_2, r_3$,
;;
;; $$(x - r_1)(x - r_2)(x - r_3) = x^3 - e_1 x^2 + e_2 x - e_3,$$
;;
;; with $e_1, e_2, e_3$ the sum, the sum of products in pairs, and the
;; product. Drag the roots; the coefficients follow. Every symmetric
;; function of the roots is a function of the coefficients; Cardano's rule
;; is the way back, from the coefficients to the roots, and it must break
;; the symmetry: it picks one cube root out of three.

(w/viete-widget)

(w/derivation :viete #"Viète")

;; ## Where this goes
;;
;; Ferrari, Cardano's student, solved the quartic in the same book (*Ars
;; magna*, ch. XXXIX). The quintic resisted for 250 years, until Lagrange
;; (1771) asked why the cubic and the quartic can be solved at all and
;; found the answer in the permutations of the roots; Abel (1824) proved
;; the general quintic cannot be solved by radicals; Galois (1832) said
;; exactly which equations can, by the group of their roots. The square
;; roots of negative numbers that Bombelli forced into arithmetic became
;; the complex numbers; Gauss drew them in the plane and proved (1799) that
;; every polynomial has a root there. Descartes (1637) turned the cubic
;; into a curve and read its roots off the intersections with a line.

(page/grade-badges (cubic/graded) #{:grade/proved :grade/numeric})

^{::clerk/no-cache true}
(page/prev-next "notebooks/algebra_cubic.clj")

(lens/section "algebra-cubic")
