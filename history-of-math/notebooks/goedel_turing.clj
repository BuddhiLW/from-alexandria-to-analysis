;; # Gödel and Turing: when a system speaks about itself
;;
;; Hilbert asked for a complete, consistent formal account of mathematics
;; and a decision procedure for its statements. Gödel's 1931 paper encoded
;; syntax as arithmetic; Turing's 1936 paper gave an exact model of a
;; mechanical calculation. Their limits are related but not identical:
;; incompleteness concerns proof in a specified theory, undecidability
;; concerns algorithms for a specified decision problem.
;;
;; Gödel's original is *Über formal unentscheidbare Sätze der Principia
;; Mathematica und verwandter Systeme I*, Monatshefte für Mathematik und
;; Physik 38 (1931), pp. 173-198. The English below is our translation.
;; Turing's original is *On computable numbers, with an application to the
;; Entscheidungsproblem*, Proceedings of the London Mathematical Society,
;; series 2, 42 (1936-37), pp. 230-265.
^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns goedel-turing
  {:history/year 1931
   :history/title "Gödel and Turing: limits of proof and computation"
   :history/era "Structures, foundations and computation"
   :history/fields #{:logic-computing :number-theory}
   :nextjournal.clerk/visibility {:code :hide :result :show}
   :nextjournal.clerk/toc true}
  (:require [history-of-math.lens :as lens]
            [history-of-math.page :as page]
            [history-of-math.widgets :as hom]
            [nextjournal.clerk :as clerk]
            [history-of-math.goedel-turing-widgets :as w]))

^{::clerk/visibility {:code :hide :result :hide}}
(hom/install!)

;; ## 1931: formulas become numbers
;;
;; Gödel first assigns a positive integer to every sign, then raises
;; successive primes to those integers. The resulting product is a number
;; that names a sequence of signs. Decoding is possible by factorization.

(page/blockquote
 "Wir wollen die Zeichen und Formeln des PM-Systems durch natürliche Zahlen ersetzen."
 "Gödel, *Über formal unentscheidbare Sätze* (1931), §1, p. 176. Our English: 'We want to replace the signs and formulas of the PM system by natural numbers.'")

(w/numbering)

;; The table diagonalizes a finite list of binary strings. Its opposite
;; differs from each listed row at that row's own index. The actual
;; incompleteness argument also needs formula encoding and a fixed-point
;; construction: this table illustrates self-reference, not its proof.

(w/diagonal)

;; Gödel's exact alphabet and the numbering below are different: the small
;; alphabet here is for experimenting, not a transcription of his code.
;; The key move is that a relation about formulas becomes a relation about
;; integers. Proposition VI constructs a sentence that asserts of its own
;; number that it has no proof in the formal system. The conclusion requires
;; the hypotheses Gödel states, including omega-consistency for the
;; undecidability of its negation in the original formulation.

(page/blockquote
 "Die allgemeinen Resultate über die Existenz formal unentscheidbarer Sätze ... gelten für eine sehr ausgedehnte Klasse formaler Systeme."
 "Gödel, *Über formal unentscheidbare Sätze* (1931), introduction, p. 173. Our English: 'The general results on the existence of formally undecidable propositions ... hold for a very extensive class of formal systems.'")

;; ## 1936: a calculation becomes a machine
;;
;; Turing asks which numbers can be computed by a finite description of
;; instructions, rather than assuming 'effective method' is self-evident.

(page/blockquote
 "We may compare a man in the process of computing a real number to a machine which is only capable of a finite number of conditions q₁, q₂, ..., qᵣ which will be called 'm-configurations'."
 "Turing, *On computable numbers* (1936), §1, Proc. LMS (2) 42, p. 231.")

(page/blockquote
 "There can be no machine E which, when supplied with the S.D of an arbitrary machine M, will determine whether M ever prints a given symbol (0 say)."
 "Turing, *On computable numbers* (1936), §8, Proc. LMS (2) 42, p. 248. S.D. means standard description; this is Turing's printing problem, not literally the later halting formulation.")

;; A binary-increment machine below makes the finite rules tangible. The
;; halting table later uses the modern diagonal argument: if a proposed
;; decider H says that program P halts on input P, a program D can do the
;; opposite on that very input. That table is a teaching reduction, not a
;; quotation from Turing's 1936 proof.
;;
;; The machine has one state that matters, *carry*. Reading 1 it writes 0
;; and moves one cell left; reading 0 or a blank cell (shown as `_`) it
;; writes 1 and enters *halt*. A **configuration** is the triple of tape,
;; head position and state, which Turing calls the "complete configuration"
;; (§2, p. 232). Press Step to apply one rule, or Run to halt to apply
;; rules until the machine reaches *halt*.

(w/turing-machine)

;; Now suppose a program H answered, for every program P and input x,
;; whether P halts on x. Tabulate H on programs P1 to P4, each run on its
;; own description. Define D to read a description P, ask H about P on P,
;; and do the opposite: loop if H says halts, halt if H says loops. Then D
;; disagrees with each listed program on the diagonal, so D is not in the
;; list. If the list held every program, D would be some Pk, and D on its
;; own description would both halt and loop. So no such H exists.

(w/halting)

;; ## Tangents
;;
;; **Hilbert, Paris 1900.** In *Mathematische Probleme* Hilbert asked, as
;; his second problem, for a proof that the axioms of arithmetic never lead
;; to a contradiction, and as his tenth for a method that decides whether a
;; Diophantine equation is solvable. He also stated his conviction that
;; every definite mathematical problem admits a settlement, "in der
;; Mathematik gibt es kein Ignorabimus" (Göttinger Nachrichten 1900, p. 262;
;; "in mathematics there is no we shall not know"). Gödel's second theorem
;; shows that the consistency proof cannot be carried out inside arithmetic
;; itself, and Matiyasevich (1970), building on Turing's notion of
;; computation, showed that the tenth problem has no such method.
;;
;; **The set-theory course.** Both arguments above are Cantor's diagonal
;; argument of 1891 in new clothes. The [set-theory course](/set-theory/)
;; proves that no list of infinite binary sequences contains every one,
;; which is the diagonal table at the top of this page without the bound
;; of four rows.
;;
;; **Computing.** Turing's §6 describes a *universal machine* U, a single
;; machine that, given the standard description of any machine M, computes
;; what M computes. Every interpreter is such a machine, including the
;; Clojure runtimes of [Clojure on Zig and Rust](/clojure-on-zig-and-rust/).


^{::clerk/no-cache true}
(page/prev-next "notebooks/goedel_turing.clj")

(lens/section "goedel-turing")
