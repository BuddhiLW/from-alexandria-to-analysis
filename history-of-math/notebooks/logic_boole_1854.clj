;; # Boole, the laws of thought as an algebra (1854)
;;
;; In 1847 Boole had published a short *Mathematical Analysis of Logic*. In
;; 1854 he came back to it at length, in *An Investigation of the Laws of
;; Thought, on which are founded the Mathematical Theories of Logic and
;; Probabilities* (London: Walton and Maberly). We quote the text of
;; Project Gutenberg 15114, checked against the first edition in the
;; Internet Archive scan
;; [bub_gb_SWgLVT0otY8C](https://archive.org/details/bub_gb_SWgLVT0otY8C);
;; page numbers are those printed in the 1854 edition, each linked to its
;; scan leaf. The chapters we use, from the printed contents: II *Signs and
;; their Laws*, III *Derivation of the Laws*, VII *Of Elimination*, XV *Of
;; the Aristotelian Logic*.
;;
;; The idea is short. Let a letter stand for a *class* of things: $x$ for
;; "men". Writing two letters side by side, $xy$, means the things to which
;; both names apply: the product is the conjunction. Then one law holds for
;; every letter that does not hold for the numbers of ordinary algebra:
;; $xx = x$. Everything else (a syllogism, an elimination, a table of
;; truth values) is done by the rules of ordinary algebra on top of it. The
;; figures and checks below come from `alexandria.structure.boolring`, the
;; powerset ring under symmetric difference, which is a special case of
;; `alexandria.structure.ring`, and from `alexandria.boole.logic`.
^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns logic-boole-1854
  {:history/year 1854
   :history/title "Boole, the Laws of Thought: x² = x and the syllogism as elimination"
   :history/era "Structures, foundations and computation"
   :history/fields #{:logic-computing :abstract-algebra}
   :nextjournal.clerk/visibility {:code :hide :result :show}
   :nextjournal.clerk/toc true}
  (:require [history-of-math.lens :as lens]
            [history-of-math.page :as page]
            [history-of-math.widgets :as hom]
            [nextjournal.clerk :as clerk]
            [alexandria.boole.widgets :as w]))

^{::clerk/visibility {:code :hide :result :hide}}
(hom/install!)

;; ## Letters are classes, the product is a conjunction
;;
;; Boole's rule of interpretation for juxtaposition (Chapter II, §6-7) is
;; that $xy$ is the class of objects to which the names of $x$ and of $y$
;; are together applicable; his example is "estuaries" and "navigable
;; rivers". From it he derives the law that sets his algebra apart:

(page/blockquote
 "As the combination of two literal symbols in the form xy expresses the whole of that class of objects to which the names or qualities represented by x and y are together applicable, it follows that if the two symbols have exactly the same signification, their combination expresses no more than either of the symbols taken alone would do. In such case we should therefore have xy = x. As y is, however, supposed to have the same meaning as x, we may replace it in the above equation by x, and we thus get xx = x. [...] In accordance with this notation, then, the above equation assumes the form x² = x, and is, in fact, the expression of a second general law of those symbols by which names, qualities, or descriptions, are symbolically represented."
 "Boole, *Laws of Thought* (1854), ch. II, §9, p. 31-32 ([scan, leaf 40](https://archive.org/details/bub_gb_SWgLVT0otY8C/page/n40), [leaf 41](https://archive.org/details/bub_gb_SWgLVT0otY8C/page/n41)).")

;; Squaring a number changes it, except for $0$ and $1$. Boole saw that
;; this makes the algebra of classes a different algebra, and in
;; Chapter II (§15) he drew the consequence: the only numbers obeying
;; $x^2 = x$ are $0$ and $1$, so "an Algebra in which the symbols admit
;; indifferently of the values $0$ and $1$, and of these values alone" has
;; the laws of the algebra of logic. Here is the same fact on a ring. In
;; $\mathbb{Z}/12$ the idempotents (the solutions of $x^2 = x$) are only
;; $0, 1, 4, 9$; in the ring of classes of a universe, with intersection as
;; product and symmetric difference as sum, *every* element is idempotent,
;; and the contract suite of the library checks it on generated classes.

(def idempotents-mod-12
  (filterv #(zero? (mod (- (* % %) %) 12)) (range 12)))

^{::clerk/visibility {:code :hide :result :show}}
(clerk/md (str "Solutions of $x^2 = x$ in $\\mathbb{Z}/12$: **" (pr-str idempotents-mod-12)
               "**; in the ring of the 16 classes of a four-object universe: **all 16**."))

;; ## The principle of contradiction is $x(1-x)=0$
;;
;; Write $x^2 = x$ as $x - x^2 = 0$, so $x(1-x) = 0$. If $x$ is "men", then
;; $1 - x$ is "not-men" and the product is the class of things that are at
;; once men and not men. The equation says it is empty:

(page/blockquote
 "Hence x (1 − x) will represent the class whose members are at once “men,” and “not men,” and the equation (1) thus express the principle, that a class whose members are at the same time men and not men does not exist. [...] But this is identically that “principle of contradiction” which Aristotle has described as the fundamental axiom of all philosophy."
 "Boole, *Laws of Thought* (1854), ch. III, p. 49-50 ([scan, leaf 58](https://archive.org/details/bub_gb_SWgLVT0otY8C/page/n58), [leaf 59](https://archive.org/details/bub_gb_SWgLVT0otY8C/page/n59)).")

(page/blockquote
 "I desire to direct attention also to the circumstance that the equation (1) in which that fundamental law of thought is expressed is an equation of the second degree."
 "Boole, ch. III, p. 50 ([scan, leaf 59](https://archive.org/details/bub_gb_SWgLVT0otY8C/page/n59)).")

;; ## Elimination: what remains when a term is removed
;;
;; Reasoning, for Boole, is to eliminate the terms one does not want, from
;; equations, and read what is left. Chapter VII gives the rule in one
;; proposition. With $f(1)$ meaning $f$ with $x$ replaced by $1$ and $f(0)$
;; the same with $0$:

(page/blockquote
 "If f(x) = 0 be any logical equation involving the class symbol x, with or without other class symbols, then will the equation f(1)f(0) = 0 be true, independently of the interpretation of x; and it will be the complete result of the elimination of x from the above equation."
 "Boole, ch. VII, Proposition I, §5, p. 101 ([scan, leaf 110](https://archive.org/details/bub_gb_SWgLVT0otY8C/page/n110)).")

;; The proof uses *development*, $f(x) = f(1)\,x + f(0)\,(1-x)$, which
;; holds because $x$ is $0$ or $1$ in the algebra, and then puts that into
;; $x(1-x)=0$. Boole remarks that the second premise needed to eliminate
;; is supplied by the law of thought itself, $x(1-x)=0$. Development in
;; several letters, a sum of coefficients times *constituents* such as
;; $xy$, $x(1-y)$, $(1-x)y$, $(1-x)(1-y)$, is `develop` in the library, and
;; `eliminate` is $f(1)f(0)$, both pure functions on a ring, tested on the
;; ring of classes against the original polynomial.

;; ## The Aristotelian syllogism
;;
;; In Chapter XV Boole sets the four categorical forms of the logicians
;; against his algebra. With an indefinite class $v$ (for "some") the
;; propositions about classes $y$ and $x$ read:

(page/blockquote
 "1. All Y’s are X’s, y = vx. 2. No Y’s are X’s, y = v(1 − x). 3. Some Y’s are X’s, vy = vx. 4. Some Y’s are not-X’s, vy = v(1 − x)."
 "Boole, ch. XV, §3, the first four of his eight types, p. 228 ([scan, leaf 237](https://archive.org/details/bub_gb_SWgLVT0otY8C/page/n237)).")

;; Eliminating $v$ from the first gives $y(1-x)=0$, which is why we can write
;; "all $Y$ are $X$" as $y(1-x)=0$ (no $Y$ lies outside $X$), and "no $Y$
;; are $X$" as $yx = 0$. A syllogism is then two such equations with a
;; middle term $m$ in common. Add them (a class that is nowhere is the sum of
;; classes that are nowhere), eliminate $m$ by $f(1)f(0)$, develop what is
;; left in the constituents of $s$ and $p$, and every constituent whose
;; coefficient is $1$ is a class that does not exist. That is the conclusion.
;;
;; Take Barbara, *all $M$ are $P$, all $S$ are $M$*. The equations are
;; $m(1-p)=0$ and $s(1-m)=0$. Their sum, with $m=1$, is $(1-p)$; with
;; $m=0$ it is $s$; the product of the two is $s(1-p)$. The residue is
;; $s(1-p)=0$: no $S$ is outside $P$, all $S$ are $P$.
;;
;; Choose a figure and the form of each premise. Premises are shown as
;; Boole's equations and in words. For two universals the widget does the
;; elimination, draws the four regions of $s$ and $p$ (grey ones are the
;; ones the residue empties), and checks the conclusion on the $2^3 = 8$
;; assignments of $s, p, m$ to $0$ or $1$: wherever both premises hold the
;; conclusion holds. For a particular premise, which is not an equation but
;; the denial of one, it tests against all models instead.

(w/syllogism-widget)

;; The last button checks all $4 \times 4 \times 4 \times 4 = 256$ ways to
;; choose a figure, two premises and a conclusion about $s$ and $p$. There
;; are 15 valid outright. Another 9 are valid only if the classes involved
;; are not empty: Barbari ($AAI$ in the first figure) concludes "some $S$
;; are $P$" from universals, which in this algebra cannot be, since
;; $s(1-m)=0$ is satisfied by $s=0$. The tradition counted these 24 moods
;; as valid because it read every term as naming something. Boole's
;; equations do not carry that assumption, and the widget marks the nine
;; with a star: **existential import**, the point on which the algebra and
;; the old logic part company.

;; ## What the idea became
;;
;; Boole's algebra was first of all a calculus: it did not need to say
;; what the symbols mean for the rules to run. Jevons (1864) replaced his
;; sum by an inclusive union; Peirce and Schröder built the algebra of
;; relations on it; Huntington (1904) gave a postulate set; Stone (1936)
;; proved that every algebra of this kind is the algebra of classes of a
;; set, and the ring with $x^2 = x$ is called, after him, a *Boolean ring*.
;; Shannon (1937) noticed that the same algebra describes relay circuits, so
;; $x^2 = x$ became the law of a switch that is open or closed, which is how
;; it entered computing. The step from this notebook to the next is
;; Frege's *Begriffsschrift* (1879), which replaced the equations by a
;; formal language with quantifiers.

^{::clerk/no-cache true}
(page/prev-next "notebooks/logic_boole_1854.clj")

(lens/section "logic-boole-1854")
