;; # Galois, the group of an equation (1830-1832, printed 1846)
;;
;; The quadratic was solved in Babylon, the cubic and the quartic by
;; del Ferro, Tartaglia, Cardano and Ferrari in the 1540s (see
;; [the cubic notebook](../algebra-cubic/)). For the quintic, nobody found a
;; formula in radicals in the next 270 years. Lagrange (1770-1771) showed why
;; the old formulas work: each one is built from functions of the roots that
;; take few values when the roots are permuted. Abel (1824, 1826) proved that
;; the general equation of degree five has no such formula.
;;
;; Évariste Galois, born 25 October 1811, answered the sharper question:
;; which equations are solvable by radicals? He sent his memoir to the
;; Académie in January 1831; Poisson reported that he could not follow it.
;; On 29 May 1832, the night before the duel that killed him, Galois wrote
;; to his friend Auguste Chevalier what the memoir contained. Joseph
;; Liouville printed the memoir, the letter and a fragment in his *Journal
;; de mathématiques pures et appliquées* 11 (1846), pp. 381-444. We quote
;; that printing, page by page, and translate it ourselves.
;;
;; Notation used below. A *permutation* of the roots $a, b, c, \dots$ is a
;; rearrangement of them; Galois calls the passage from one arrangement to
;; another a *substitution*. $S_n$ is the group of all $n!$ permutations of
;; $n$ roots. A subgroup $H$ of $G$ is *normal* (written $H \trianglelefteq
;; G$) when $g H g^{-1} = H$ for every $g$ in $G$, which is Galois'
;; *décomposition propre*. $G/H$ is the quotient group of the cosets $gH$.
;;
;; The figures and checks come from Alexandria's shelf `alexandria.galois`:
;; *proved* means Emmy simplified a difference to exactly 0, *numeric* that
;; a number from raster was within the bound.
^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns galois
  {:history/year 1832
   :history/title "Galois, the group of an equation: solvability by radicals"
   :history/era "Structures, foundations and computation"
   :history/fields #{:abstract-algebra :algebra :number-theory}
   :nextjournal.clerk/visibility {:code :hide :result :show}
   :nextjournal.clerk/toc true}
  (:require [history-of-math.lens :as lens]
            [history-of-math.page :as page]
            [history-of-math.widgets :as hom]
            [nextjournal.clerk :as clerk]
            [alexandria.galois.widgets :as w]
            [alexandria.galois.solvable :as solvable]))

^{::clerk/visibility {:code :hide :result :hide}}
(hom/install!)

;; ## The memoir, in Galois' words (1831)
;;
;; The preface, dated 16 January 1831, was crossed out in the manuscript;
;; Chevalier printed it anyway. It states the whole programme:

(page/blockquote
 "The attached memoir is extracted from a work that I had the honour of presenting to the Academy a year ago. That work not having been understood, the propositions it contains having been doubted, I have had to content myself with giving, in synthetic form, the general principles and a single application of my theory. I beg my judges at least to read these few pages with attention."
 "Galois, *Mémoire sur les conditions de résolubilité des équations par radicaux*, J. math. pures appl. 11 (1846), p. 417; our translation. *Le Mémoire ci-joint est extrait d'un ouvrage que j'ai eu l'honneur de présenter à l'Académie il y a un an. [...] Je supplie mes juges de lire du moins avec attention ce peu de pages.*")

(page/blockquote
 "For an equation of prime degree, which has no commensurable divisors, to be solvable by radicals, it is necessary and sufficient that all the roots be rational functions of any two of them."
 "Galois, Mémoire, p. 417; our translation. *Pour qu'une équation de degré premier, qui n'a pas de diviseurs commensurables, soit soluble par radicaux, il faut et il suffit que toutes les racines soient des fonctions rationnelles de deux quelconques d'entre elles.*")

;; The first proposition attaches a group to every equation. *Rationally
;; known* means: expressible by the four operations from the coefficients
;; (and from any quantities adjoined so far).

(page/blockquote
 "Let an equation be given, of which a, b, c, ... are the m roots. There will always be a group of permutations of the letters a, b, c, ... which will have the following property: 1. that every function of the roots, invariant under the substitutions of this group, be rationally known; 2. conversely, that every function of the roots that can be determined rationally be invariant under the substitutions."
 "Galois, Mémoire, Proposition I, p. 421; our translation. *Il y aura toujours un groupe de permutations des lettres a, b, c, ... qui jouira de la propriété suivante : 1°. Que toute fonction des racines, invariable par les substitutions de ce groupe, soit rationnellement connue ; 2°. Réciproquement, que toute fonction des racines, déterminable rationnellement, soit invariable par les substitutions.*")

;; Solving the equation means shrinking that group, one adjoined radical at
;; a time, down to the identity:

(page/blockquote
 "I shall observe first that, to solve an equation, one must successively lower its group until it contains no more than a single permutation."
 "Galois, Mémoire, Proposition V, p. 425; our translation. *J'observerai d'abord que, pour résoudre une équation, il faut successivement abaisser son groupe jusqu'à ne contenir plus qu'une seule permutation.*")

;; ## The group of x⁴ − 2 acting on its roots
;;
;; Take $x^4 - 2 = 0$. Its roots are $r_k = \sqrt[4]{2}\, i^k$ for
;; $k = 0, 1, 2, 3$, where $i^2 = -1$: the corners of a square of radius
;; $\sqrt[4]{2} \approx 1.1892$ (raster). Over the rational numbers they
;; satisfy relations such as $r_0 + r_2 = 0$ and $r_1 + r_3 = 0$. By
;; Proposition I, the group of the equation consists of the permutations
;; that keep every rational relation true. Of the $4! = 24$ permutations
;; of $S_4$, only 8 keep both relations above, and they are exactly the
;; closure of two moves: $\sigma$, multiplication by $i$ (the 4-cycle
;; $r_0 \to r_1 \to r_2 \to r_3 \to r_0$), and $\tau$, complex conjugation
;; (exchanging $r_1$ and $r_3$). This is the dihedral group $D_4$ of the
;; square. The group is computed with `alexandria.structure.group/closure`
;; on `alexandria.structure.groups/symmetric`, the same $S_n$ the Klein and
;; Kepler notebooks use.
;;
;; Press $\sigma$ and $\tau$ in any order, or pick any permutation from
;; the list. The verdict is read from the two dashed pairs: in the group,
;; both still join opposite corners.

(w/roots-widget)

(w/group-table)


;; ## Solvable and not: S₄ against S₅
;;
;; In the letter to Chevalier, Galois says which subgroups allow the
;; lowering. A group $G$ containing $H$ splits into the cosets $HS$ and also
;; into the cosets $TH$; when the two splittings agree, $H$ is normal:

(page/blockquote
 "These two kinds of decomposition do not ordinarily coincide. When they coincide, the decomposition is called proper. [...] If these groups each have a prime number of permutations, the equation will be solvable by radicals; if not, not. The smallest number of permutations that an indecomposable group can have, when this number is not prime, is 5.4.3."
 "Galois, letter to Auguste Chevalier, 29 May 1832, J. math. pures appl. 11 (1846), p. 409; our translation. *Quand ils coïncident, la décomposition est dite propre. [...] Si ces groupes ont chacun un nombre premier de permutations, l'équation sera soluble par radicaux ; sinon, non. Le plus petit nombre de permutations que puisse avoir un groupe indécomposable, quand ce nombre n'est pas premier, est 5.4.3.*")

;; In today's words: an equation is solvable by radicals exactly when its
;; group $G$ has a chain $G = G_0 \trianglerighteq G_1 \trianglerighteq
;; \dots \trianglerighteq G_m = 1$ in which every $G_{j+1}$ is normal in
;; $G_j$ and every quotient $G_j / G_{j+1}$ is cyclic of prime order.
;; Each step is one adjoined radical: a $p$-th root lowers the group by a
;; factor $p$. Such a group is called *solvable*, after this theorem.
;;
;; The general quartic has the whole of $S_4$ as its group, and $S_4$ has
;; the chain $S_4 \trianglerighteq A_4 \trianglerighteq V_4
;; \trianglerighteq Z_2 \trianglerighteq 1$ with indices $2, 3, 2, 2$.
;; Here $A_n$ is the subgroup of even permutations, $V_4$ the four
;; permutations $e, (0\,1)(2\,3), (0\,2)(1\,3), (0\,3)(1\,2)$, and $Z_2$ one
;; of its two-element subgroups. The steps are Ferrari's: a square root
;; (the discriminant), a cube root (the resolvent cubic), then two square
;; roots. The general quintic has $S_5$, whose only proper normal subgroup
;; is $A_5$ of order $60 = 5 \cdot 4 \cdot 3$, and $A_5$ has no normal
;; subgroup between itself and $1$: its conjugacy classes have sizes
;; $1, 12, 12, 15, 20$, and no sum of them that includes the $1$ divides
;; $60$ except $1$ and $60$. Galois' "5.4.3" is this group. The table is
;; computed on the permutations themselves, every pair checked for
;; normality and for a commutative quotient:

(w/chain-table)

;; Emmy grades the counts as exact differences and raster the roots:

(page/grade-badges (solvable/graded) #{:grade/proved :grade/numeric})

;; ## Where the group went next
;;
;; Liouville announced in 1843 that he had found the method exact, and
;; printed it in 1846. Camille Jordan's *Traité des substitutions et des
;; équations algébriques* (1870) set the theory out as a theory of
;; permutation groups, with composition series and their factors, and
;; Felix Klein read it with Sophus Lie in Paris that same year. Two years
;; later Klein turned the idea from equations to space: a geometry is the
;; study of what a group of transformations leaves unchanged. That is
;; [the Erlangen notebook](../klein-erlangen/), where the same
;; `alexandria.structure.group` protocol carries the motions, similarities,
;; affine and projective maps that carry the roots here. The live
;; [Erlangen deck](/erlangen-program/) follows the programme group by
;; group, and a group-theory journey (subgroups, normal subgroups,
;; quotients, actions, from the definitions to $A_5$) is planned as its
;; own series.

^{::clerk/no-cache true}
(page/prev-next "notebooks/galois.clj")

(lens/section "galois")
