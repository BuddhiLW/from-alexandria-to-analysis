;; # Gauss' Disquisitiones arithmeticae: congruences, powers, residues
^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns number-theory-gauss
  {:history/year 1801
   :history/title "Gauss, Disquisitiones arithmeticae: congruence, Fermat's theorem, quadratic reciprocity"
   :history/era "Analysis made rigorous"
   :history/fields #{:number-theory :abstract-algebra}
   :nextjournal.clerk/visibility {:code :hide :result :show}
   :nextjournal.clerk/toc true}
  (:require [alexandria.gauss.congruence :as c]
            [alexandria.gauss.congruence-widgets :as w]
            [history-of-math.lens :as lens]
            [history-of-math.page :as page]
            [history-of-math.widgets :as hom]
            [nextjournal.clerk :as clerk]))

^{::clerk/visibility {:code :hide :result :hide}}
(hom/install!)

;; This is the first notebook of the number theory strand. Gauss wrote the
;; *Disquisitiones arithmeticae* between 1796 and 1798, aged nineteen to
;; twenty-one, and it was printed at Leipzig in 1801. It gathered what
;; Fermat, Euler, Lagrange and Legendre had found about whole numbers and
;; rebuilt it on one idea, the congruence, with a sign of its own. Its last
;; section, the regular 17-gon, has its own page:
;; [Gauss: the 17-gon and the Theorema egregium](../gauss/). Here we read
;; the first four sections.
;;
;; We quote the Latin of the *Werke* (vol. I, 1863, transcribed on Latin
;; Wikisource) and Fermat's French from the *Varia opera* (Toulouse, 1679),
;; with our English beside them, marked as ours. Read today, Gauss' objects
;; are standard algebra: the residues modulo $n$ form the ring
;; $\mathbb Z/n$, its addition is the cyclic group acting on the $n$-gon,
;; its units form a finite abelian group, and the Legendre symbol is a
;; character of that group. Alexandria holds those structures once
;; (`alexandria.structure.group`, `groups`, `ring`, `tie`). This notebook
;; uses special cases of them, and every number on it comes from a raster
;; kernel or from those instances.

;; ## 1. Congruence (art. 1-4)
;;
;; The book opens with a definition and, in the next article, a sign.
;; Step through it on a clock of five hours:

(w/steps)

;; Art. 2 adds that all the residues of $a$ modulo $m$ are contained in
;; one formula:

(page/blockquote
 "Omnia numeri dati a residua secundum modulum m sub formula a+km comprehenduntur, designante k numerum integrum indeterminatum."
 "Gauss, Disquisitiones arithmeticae (1801), art. 2. Ours: \"All the residues of a given number a modulo m are contained in the formula a + km, k standing for an indeterminate integer.\"")

;; So $b \equiv a \pmod n$ exactly when $b = a + kn$ for an integer $k$.
;; The widget ties the four quantities $n, a, b, k$. Move $b$ and $k$ is
;; re-solved as $(b - a)/n$; move $k$ and $b$ follows. The two numbers are
;; congruent when $k$ is a whole number, and the page names the case live
;; in Gauss' words, *congrui* or *incongrui*. Drag the hands round the clock.
;; Adding $a$ turns the clock: hour $k$ goes to hour $a + k$. That is the
;; cyclic group $\mathbb Z/n$ acting on the vertices of the regular $n$-gon,
;; drawn by the amber arrows. Art. 4 calls the number on the dial
;; ($0 \le r < n$) the *residuum minimum*.

(w/clock)

;; ## 2. Fermat's little theorem and Euler's generalisation (art. 45-57)
;;
;; Fermat stated the theorem in a letter to Frénicle of 18 October 1640,
;; without proof:

(page/blockquote
 "Tout nombre premier mesure infailliblement une des puissances − 1 de quelque progression que ce soit, & l'exposant de ladite puissance est sous-multiple du nombre premier donné − 1."
 "Fermat to Frénicle, 18 October 1640, in Varia opera mathematica (Toulouse, 1679), p. 163. Ours: \"Every prime number infallibly measures one of the powers − 1 of any progression whatever, and the exponent of that power is a submultiple of the given prime − 1.\"")

;; His example is the progression $3, 9, 27, \ldots$ and the prime 13: it
;; measures $3^3 - 1 = 26$, and 3 divides 12. Gauss proves it as art. 49:
;; if $a^t$ is the lowest power of $a$ congruent to 1, then $t$ is $p - 1$
;; or a divisor of it. In art. 50 he names it:

(page/blockquote
 "Theorema hoc, quod tum propter elegantiam tum propter eximiam utilitatem omni attentione dignum, ab inventore theorema Fermatianum appellari solet."
 "Gauss, Disquisitiones arithmeticae, art. 50. Ours: \"This theorem, which deserves every attention both for its elegance and for its outstanding usefulness, is usually called Fermat's theorem after its discoverer.\"")

;; He then recalls Euler's first proof (1736). It expands $(a+1)^p$ by the
;; binomial theorem: every middle coefficient $\binom{p}{j}$ is divisible
;; by $p$, so $(a+1)^p - a^p - 1$ is, and induction on $a$ gives
;; $a^p \equiv a$. Emmy checks the divisibility for $p = 3, 5, \ldots, 13$
;; among the grades below. Gauss's own proof is the one we can watch: the
;; powers $1, a, a^2, \ldots$ walk round the clock of $p$ hours and must come
;; back to 1. The number of steps, the *order* of $a$, divides $p - 1$,
;; because the walk and its translates split the $p - 1$ units into equal
;; cycles. Those cycles are the cosets of the subgroup generated by $a$ in
;; $(\mathbb Z/p)^\times$. For a composite modulus $m$ only the units walk
;; back. There are $\varphi(m)$ of them, and Euler (1763) put $\varphi(m)$
;; in place of $p - 1$. When the walk visits every unit, $a$ is a primitive
;; root:

(page/blockquote
 "Numeros ad exponentem p−1 pertinentes radices primitivas cum ill. Eulero vocabimus."
 "Gauss, Disquisitiones arithmeticae, art. 57. Ours: \"Numbers belonging to the exponent p − 1 we shall call primitive roots, with the illustrious Euler.\"")

;; Choose $m$ and $a$ and press *× a*. Each step is one call of a raster
;; kernel: $a$ acting on the clock in the group of units modulo $m$. The page
;; names the case live: a primitive root, an order that divides
;; $\varphi(m)$, or not a unit at all. The primitive root 3 of 17 is the
;; one Gauss uses to order the roots of the 17-gon (art. 353), on the
;; [Gauss page](../gauss/).

(w/powers)

;; ## 3. The multiplication table as permutations
;;
;; Write out the multiplication table modulo $n$. Row $a$ lists
;; $a \cdot 0, a \cdot 1, \ldots, a \cdot (n-1)$. It is a permutation of
;; $0, \ldots, n-1$ exactly when $\gcd(a, n) = 1$: then $ax \equiv ay$
;; forces $x \equiv y$ (art. 22). Otherwise the row repeats values. The
;; rows that are permutations belong to the units, and the units are closed
;; under multiplication: they form a group. Gauss shows in art. 52-57 that
;; for a prime modulus it is cyclic: a primitive root exists. In art. 82-92
;; he finds which composite moduli have one: $2, 4, p^k$ and $2p^k$. For
;; every other modulus the group of units is a product of smaller cyclic
;; groups. The page names that product live from the orders of the
;; elements, by `alexandria.structure.ring/structure`.

(w/table)

;; ## 4. Quadratic residues and the fundamental theorem (art. 95-131)
;;
;; Section IV sorts the numbers by whether they are squares modulo the
;; modulus:

(page/blockquote
 "Illos appellabimus residua quadratica numeri istius quem pro modulo accepimus, hos vero ipsius non-residua quadratica"
 "Gauss, Disquisitiones arithmeticae, art. 95. Ours: \"The former [numbers congruent to a square] we shall call quadratic residues of the number taken as modulus, the latter its quadratic non-residues.\"")

;; Today one writes Legendre's symbol $(a/p) = +1$ for a residue and $-1$
;; for a non-residue. Euler's criterion computes it as
;; $a^{(p-1)/2} \bmod p$: the walk of section 2 stopped half way. Since
;; $(ab/p) = (a/p)(b/p)$, the symbol is a *character* of the group of units,
;; a homomorphism onto $\{+1, -1\}$. Alexandria's ring contract checks
;; exactly that. Gauss found by induction which primes $q$ have a given prime
;; $p$ as a residue, then stated the law that ties the two questions
;; together and called it the fundamental theorem:

(page/blockquote
 "Si p est numerus primus formae 4n+1, erit +p, si vero p formae 4n+3, erit −p residuum vel non-residuum cuiusvis numeri primi, qui positive acceptus ipsius p est residuum vel non-residuum."
 "Gauss, Disquisitiones arithmeticae, art. 131. Ours: \"If p is a prime of the form 4n + 1, then +p, and if p is of the form 4n + 3, then −p, will be a residue or a non-residue of every prime which, taken positively, is a residue or a non-residue of p.\"")

;; In Legendre's notation:
;; $(p/q)(q/p) = (-1)^{\frac{p-1}{2}\cdot\frac{q-1}{2}}$. The two symbols
;; agree unless both primes are $\equiv 3 \pmod 4$, and then they are
;; opposite. The grid below has every pair of odd primes below 60, coloured
;; by the two symbols, with the exceptional cells lit. Pick a cell, or move
;; $p$ and $q$: the page names the case and checks the law in that cell.
;; Gauss gave the first complete proof (art. 135-144, by induction), and
;; eight proofs over his lifetime.

(w/reciprocity)

;; Emmy and raster check everything above. Emmy proves the tie and Euler's
;; binomial step symbolically. Raster's kernels run the clock against the
;; action of $\mathbb Z/n$ (`structure.groups/cyclic`), the walk against
;; the order in `structure.groups/units-mod`, and Euler's criterion against
;; the Legendre character of `structure.ring`:

(w/derivation)

(page/grade-badges (c/graded) #{:grade/proved :grade/numeric})

;; ## 5. Bridge to analysis: Dirichlet 1837, Riemann 1859
;;
;; How many primes are there below $x$? As a boy Gauss counted them in
;; blocks of a thousand and guessed that their density near $x$ is
;; $1/\log x$, so that $\pi(x)$ is close to
;; $\operatorname{Li}(x) = \int_2^x dt/\log t$. Dirichlet (1837) used
;; analysis, his $L$-series built from characters like the Legendre symbol,
;; to prove a statement about progressions:

(page/blockquote
 "dass jede unbegrenzte arithmetische Reihe, deren erstes Glied und Differenz keinen gemeinschaftlichen Factor haben, unendlich viele Primzahlen enthält."
 "Dirichlet, Beweis des Satzes, dass jede unbegrenzte arithmetische Progression ... (Abh. Akad. Berlin 1837), Werke I, p. 315. Ours: \"that every unbounded arithmetic progression whose first term and difference have no common factor contains infinitely many primes.\"")

;; Riemann (1859) wrote $\pi(x)$ as a sum over the zeros of
;; $\zeta(s)$. He noted that the count of Gauss and Goldschmidt up to three
;; million had always found fewer primes than $\operatorname{Li}(x)$:

(page/blockquote
 "von Li(x) mit der Anzahl der Primzahlen unter x diese Anzahl schon vom ersten Hunderttausend an stets kleiner als Li(x) ergeben"
 "Riemann, Ueber die Anzahl der Primzahlen unter einer gegebenen Grösse (1859), Gesammelte Werke (1876), p. 143. Ours: \"[the comparison] of Li(x) with the number of primes below x has, from the first hundred thousand on, always given this number smaller than Li(x)\".")

;; Move $x$. The sieve gives $\pi(x)$, and raster's adaptive quadrature
;; gives $\operatorname{Li}(x)$. The page names the closer of the two
;; approximations, Gauss' $\operatorname{Li}(x)$ or Legendre's
;; $x/\log x$. (Littlewood showed in 1914 that $\pi(x)$ overtakes
;; $\operatorname{Li}(x)$ infinitely often, far beyond any table.) Riemann's
;; analysis continues on the [Riemann and Weierstrass](../riemann-weierstrass/)
;; page.

(w/primes)

^{::clerk/no-cache true}
(lens/section "number-theory-gauss")

^{::clerk/no-cache true}
(page/prev-next "notebooks/number_theory_gauss.clj")
