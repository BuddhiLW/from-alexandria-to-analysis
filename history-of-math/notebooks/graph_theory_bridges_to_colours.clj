;; # From bridges to colours: Euler, Kirchhoff, Cayley, Hamilton, Kempe
^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns graph-theory-bridges-to-colours
  {:history/year 1736
   :history/title "Euler, Solutio problematis ad geometriam situs pertinentis (1736): from bridges to colours"
   :history/era "Enlightenment"
   :history/fields #{:graph-theory :topology}
   :nextjournal.clerk/visibility {:code :hide :result :show}
   :nextjournal.clerk/toc true}
  (:require [alexandria.graph-theory.widgets :as w]
            [history-of-math.lens :as lens]
            [history-of-math.page :as page]
            [history-of-math.widgets :as hom]
            [nextjournal.clerk :as clerk]))

^{::clerk/visibility {:code :hide :result :hide}}
(hom/install!)

;; This is the first notebook of the graph-theory strand. A graph is a set
;; of points and the lines between them. Here a line may be doubled, as
;; Euler's bridges are, and where a point lies does not matter. Five
;; problems from 1736 to 1879 made that object: Euler's bridges, Kirchhoff's
;; electrical networks, Cayley's trees, Hamilton's tour of the
;; dodecahedron, and Kempe's four colours.
;;
;; Every widget below applies one library structure,
;; `alexandria.structure.graph`, to a special case. The structure gives
;; degrees, Euler's rule with its witness walk, Kirchhoff's determinant,
;; faces, colourings and the automorphism group. The
;; [Euler notebook](../euler/) tells E53 paragraph by paragraph, with the
;; city map collapsing to four points, Euler's table and his fifteen-bridge
;; example. This notebook does not repeat those. It starts from his rule
;; and follows where it led.

;; ## Euler 1736: the seven bridges
;;
;; Euler states the question as he heard it in Königsberg:

(page/blockquote
 "Circa hos pontes iam ista proponebatur quaestio, num quis cursum ita instituere queat, ut per singulos pontes semel et non plus quam semel transeat."
 "Euler, Solutio problematis ad geometriam situs pertinentis (E53), Commentarii academiae scientiarum Petropolitanae 8 (1736, publ. 1741), §2, spelling modernised. Our translation: \"About these bridges the question was asked whether one could arrange a walk so as to cross each bridge once and not more than once.\"")

;; He then asks the general question at once, for any river and any number
;; of bridges:

(page/blockquote
 "Ego ex hoc mihi sequens maxime generale formavi problema; quaecunque sit fluvii figura et distributio in ramos, atque quicunque fuerit numerus pontium, invenire, utrum per singulos pontes semel tantum transiri queat, an vero secus?"
 "Euler, E53 §2. Our translation: \"From this I formed the following very general problem for myself: whatever the shape of the river and its division into branches, and whatever the number of bridges, to find whether one can cross each bridge exactly once or not.\"")

;; His notation, one sentence per step. A walk is a string of capitals,
;; and the count of how often a capital must appear is fixed by the
;; number of bridges at that region:

(w/euler-notation)

;; The rule he reaches in §20:

(page/blockquote
 "Si fuerint plures duabus regiones, ad quas ducentium pontium numerus est impar, tum certo affirmari potest, talem transitum non dari."
 "Euler, E53 §20. Our translation: \"If there are more than two regions to which an odd number of bridges lead, it can be affirmed with certainty that no such walk exists.\"")

(page/blockquote
 "Si autem ad duas tantum regiones ducentium pontium numerus est impar, tunc transitus fieri poterit, si modo cursus in altera harum regionum incipiatur."
 "Euler, E53 §20. Our translation: \"But if an odd number of bridges lead to two regions only, the walk can be made, provided it starts in one of these two regions.\"")

;; Below, the bridges are yours. Add or remove them pair by pair with + and
;; −, and drag the regions. A raster kernel counts the degrees, the odd
;; regions and the pieces. The case is named from those counts by data,
;; Euler's rule as `structure.graph/euler-cases`. When a walk exists,
;; Hierholzer's construction (1873) finds one. The walk is drawn bridge by
;; bridge, and its record is written in Euler's letters. Euler proved only
;; that the walk is impossible when there are more than two odd regions.
;; That it exists when there are none or two was proved by Hierholzer.

(w/multigraph :euler)

;; ## Kirchhoff 1847: one equation per independent circuit
;;
;; Ninety years later the same kind of drawing was a network of wires.
;; Kirchhoff's first law is about closed figures in the network:

(page/blockquote
 "Wenn die Drähte k₁, k₂, … eine geschlossene Figur bilden, und w_k bezeichnet den Widerstand des Drahtes k, E_k die elektromotorische Kraft, die in demselben ihren Sitz hat, nach derselben Richtung positiv gerechnet als I_k, so ist, falls I_{k₁}, I_{k₂}, … alle nach einer Richtung als positiv gerechnet werden: w_{k₁}I_{k₁} + w_{k₂}I_{k₂} + … = E_{k₁} + E_{k₂} + …"
 "Kirchhoff, Ueber die Auflösung der Gleichungen, auf welche man bei der Untersuchung der linearen Vertheilung galvanischer Ströme geführt wird, Annalen der Physik und Chemie 72 (1847), p. 497, Satz I. Our translation: \"If the wires k₁, k₂, … form a closed figure, w_k is the resistance of wire k and E_k the electromotive force seated in it, counted positive in the same direction as I_k, then, all the I counted positive in one direction: w₁I₁ + w₂I₂ + … = E₁ + E₂ + …\"")

;; His second law adds one equation per junction. He counts (pp. 506–507) how
;; many of the first-law equations are independent: for $\lambda$ wires
;; and $n$ junctions in one connected piece there are
;; $\mu = \lambda - n + 1$. That number is the cycle rank $E - V + 1$.
;; He then solves the equations. Every current is a sum over the
;; *combinations* of $n - 1$ wires that close no figure, which today we
;; call the spanning trees. The common denominator is the sum of their
;; resistance products. With every resistance 1 it is the number of
;; trees, the determinant of the network's matrix with one row and column
;; struck out (the matrix-tree theorem).
;;
;; Take the same four regions as a network, each bridge a wire. Change the
;; bridges and watch the matrix, its determinant, the circuit count and
;; the list of trees. Click a tree to draw it.

(w/multigraph :kirchhoff)

;; Emmy, with the six multiplicities left as letters, gives the
;; determinant as a polynomial. Each term is one spanning tree of the
;; four regions, read off as its three pairs. The checks below are graded
;; on the structure itself, on the page's own search code run on the JVM,
;; and on brute force:

(w/derivation)

;; ## Cayley 1857 and 1889: counting trees
;;
;; Cayley met trees in 1857 in the calculus of operators, as the forms
;; that record the order of differentiations:

(page/blockquote
 "This, however, can be at once effected by means of the analytical forms called trees (see figs. 1, 2, 3), which contain all the trees which can be formed with one branch, two branches, and three branches respectively."
 "Cayley, On the theory of the analytical forms called trees, Philosophical Magazine 13 (1857), 172–176; Collected Mathematical Papers III, no. 203, p. 242")

;; In 1889 he stated the count for labelled points:

(page/blockquote
 "The number of trees which can be formed with n + 1 given knots α, β, γ, … is = (n + 1)^{n−1}; for instance n = 3, the number of trees with the 4 given knots α, β, γ, δ is 4² = 16."
 "Cayley, A theorem on trees, Quarterly Journal of Pure and Applied Mathematics 23 (1889), 376–378; Collected Mathematical Papers XIII, no. 895, p. 26")

;; With $n$ points his formula reads $n^{n-2}$. It is Kirchhoff's
;; determinant on the complete graph $K_n$. Struck down to $n - 1$ rows,
;; its matrix has $n - 1$ on the diagonal and $-1$ elsewhere, and its
;; determinant is $n^{n-2}$ (graded above for every $d$). The number of
;; points $n$, the edges $n(n-1)/2$ and the independent circuits
;; $(n-1)(n-2)/2$ are tied: move any one of them and the others follow.
;; Step through the trees one by one. Each comes from its Prüfer code.

(w/cayley)

;; ## Hamilton 1857: the icosian game
;;
;; Hamilton sold a board game in 1859, built on the twenty corners of the
;; dodecahedron. The board is lettered with twenty consonants, and the
;; player is to visit every corner once and return. His *Memorandum
;; respecting a new system of roots of unity* (1856) and the icosian
;; calculus behind the game are not yet in our corpus, so we do not quote
;; them here. The game itself is below, on the dodecahedron's graph drawn
;; through one face.

(w/icosian)

;; The two questions look alike and are not. Euler's asks for every
;; *edge* once, and the parity of the degrees decides it at a glance. On
;; this board all twenty vertices are odd, so the answer is no. Hamilton's
;; asks for every *vertex* once. No counting rule like Euler's is known
;; for it, and deciding it for a general graph is NP-complete (Karp, 1972).

;; ## Kempe 1879: four colours
;;
;; Kempe begins from the map-maker's practice:

(page/blockquote
 "In cases where it is important that the distinction should be clearly marked, the artifice has been adopted by map-makers of painting the districts in different colours, so that the boundaries are clearly defined as the places where one colour ends and another begins; thus rendering it possible to omit the boundary lines altogether."
 "Kempe, On the geographical problem of the four colours, American Journal of Mathematics 2 (1879), p. 193")

;; Put a point in each district and join two points when the districts
;; share a border: the map becomes a graph, and colouring the map is
;; colouring the points so that no line joins two of one colour. Click the
;; districts to colour this one:

(w/colour-map)

;; Kempe's tool is the chain of two colours:

(page/blockquote
 "It will readily be seen that we can interchange the colours of the districts in one or more of the red and green regions without doing so in any others, and the map will still be properly coloured."
 "Kempe, On the geographical problem of the four colours (1879), p. 195")

;; Step through his argument on a district with four neighbours:

(w/kempe-swap-steps)

;; Kempe's proof also treated a district with five neighbours, and that
;; part is where it fails. Heawood found the flaw in 1890 and showed that
;; the same chains prove that five colours always suffice (P. J. Heawood,
;; Map-colour theorem, Quarterly Journal of Mathematics 24, 1890). Four
;; colours waited until Appel and Haken, 1976.

;; ## What came after
;;
;; Listing's *Vorstudien zur Topologie* (1847) named the field Euler had
;; called *geometria situs*. Sylvester (1878) gave the word *graph*.
;; Kőnig's *Theorie der endlichen und unendlichen Graphen* (1936) was its
;; first textbook. Kirchhoff's determinant is used today to count trees in
;; networks, and its eigenvalues are the spectral graph theory of
;; clustering and random walks. Euler's own count of faces, V − E + F = 2,
;; is in the [Euler notebook](../euler/).

^{::clerk/no-cache true}
(lens/section "graph-theory-bridges-to-colours")

^{::clerk/no-cache true}
(page/prev-next "notebooks/graph_theory_bridges_to_colours.clj")
