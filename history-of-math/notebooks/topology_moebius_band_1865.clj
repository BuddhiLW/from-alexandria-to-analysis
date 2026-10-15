;; # Möbius, the surface with one side (1865)
;;
;; In 1858 Möbius and Listing found, independently, that a strip of paper
;; given a half twist before its ends are glued has one edge and one side.
;; The printed account that survives in Möbius' own words is a by-product
;; of a different question, which he published in 1865 in the *Berichte* of
;; the Royal Saxon Society of Sciences in Leipzig: how to define the content
;; (the volume) of a polyhedron whose faces may cut one another. Answering
;; it, he had to say when a polyhedron has a content at all, and found that
;; some have none. We quote the paper as reprinted in his *Gesammelte
;; Werke*, vol. 2 (Leipzig 1886), pp. 473-512, and translate it ourselves.
;;
;; Notation used below. A *cell structure* of a surface lists its vertices
;; $V$, edges $E$ and faces $F$; its *Euler characteristic* is
;; $\chi = V - E + F$. To *orient* a surface is to give each face a sense of
;; circulation (clockwise or not, seen from one side) so that every edge
;; shared by two faces is run in opposite directions by them. The figures
;; and checks come from `alexandria.structure.surface`, which holds the cell
;; complex, the orientation and the signed volume, and from
;; `alexandria.structure.ring`'s *character* (a map from a group to
;; $\{+1,-1\}$ respecting products), which is how the orientation of a loop
;; is recorded.
^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns topology-moebius-band-1865
  {:history/year 1865
   :history/title "Möbius, the one-sided band: orientation and the content of a polyhedron"
   :history/era "Structures, foundations and computation"
   :history/fields #{:topology :geometry}
   :nextjournal.clerk/visibility {:code :hide :result :show}
   :nextjournal.clerk/toc true}
  (:require [history-of-math.lens :as lens]
            [history-of-math.page :as page]
            [history-of-math.widgets :as hom]
            [nextjournal.clerk :as clerk]
            [alexandria.moebius.widgets :as w]))

^{::clerk/visibility {:code :hide :result :hide}}
(hom/install!)

;; ## The question: the content of a polyhedron
;;
;; Möbius begins by recalling that the content of a plane polygon, even a
;; self-crossing one, was settled by Meister a century earlier. For
;; polyhedra nobody had gone so far:

(page/blockquote
 "Had one explored the peculiarities of polyhedra of this kind in their full extent, one would have arrived at the proposition, apparently not yet stated, that polyhedra can be constructed whose content cannot be given at all."
 "Möbius, Ueber die Bestimmung des Inhaltes eines Polyeders, Ber. Königl. Sächs. Ges. Wiss., math.-phys. Cl. 17 (1865) 31-68 = Werke II, p. 475; our translation. *Hätte man die Eigenthümlichkeiten von Polyedern dieser Art in ihrem ganzen Umfange durchforscht, so würde man auf den bis jetzt wohl noch nicht ausgesprochenen Satz gekommen sein, dass sich Polyeder construiren lassen, deren Inhalt gar nicht angebbar ist.*")

;; ## The law of opposite edges
;;
;; The content is a sum of signed pyramids over the faces, and the signs
;; make sense only when the faces are given senses of circulation that agree
;; along every shared edge. Möbius states this as a law he calls the
;; *Kantengesetz*:

(page/blockquote
 "The perimeters of the faces bounding a polyhedron can be given such senses that, for every edge of the polyhedron, the two directions which belong to it as the common edge of two faces, in consequence of the senses of these two faces, are opposite to one another."
 "Möbius, Werke II, p. 477; our translation. *Den Perimetern der ein Polyeder umgrenzenden Flächen können solche Sinne beigelegt werden, dass für jede Kante des Polyeders die zwei Richtungen, welche derselben, als der gemeinschaftlichen Kante zweier Polyederflächen, in Folge der Sinne dieser zwei Flächen zukommen, einander entgegengesetzt sind.*")

;; He calls it "the law of opposite edges, or simply the law of edges" and
;; checks it on the tetrahedron $ABCD$, whose faces $BCD, CAD, DAB, ACB$
;; run every edge once each way. Then he asks for a polyhedron that breaks
;; it. Take the endless sequence of vertices $\dots ABCDEABC\dots$ of period
;; five. It makes the five triangles $ABC, BCD, CDE, DEA, EAB$, a *zone*,
;; each sharing an edge with the next; the sixth triangle is $ABC$ again.
;; Closing the zone with five more triangles through a sixth vertex $F$
;; gives a polyhedron with six vertices, ten faces and fifteen edges:

(page/blockquote
 "It has six corners, ten faces and fifteen edges (so it is not an Eulerian one)."
 "Möbius, Werke II, p. 483, §8; our translation. *Es hat sechs Ecken, zehn Flächen und fünfzehn Kanten (ist also kein Euler'sches).*")

;; Here $\chi = 6 - 15 + 10 = 1$, where Euler's formula for polyhedra
;; without holes gives 2 ([the Euler notebook](..\/euler\/)). The reason it
;; fails is the next paragraph:

(page/blockquote
 "To one who runs through the triangles of the zone (R) in turn, their senses appear alternately positive and negative; so, if ABC is the first triangle, the sense of the sixth, which is again ABC, is negative. This is possible only if the sides of ABC on which he stands at the beginning and at the end of his path are opposite to each other."
 "Möbius, Werke II, p. 483, §9; our translation. *Dem, welcher die Dreiecke der Zone (R) in ihrer Folge durchgeht, erscheinen die [...] Sinne derselben abwechselnd positiv und negativ, mithin, wenn ABC das erste Dreieck ist, der Sinn des sechsten, welches wiederum ABC ist, negativ. Dieses ist aber nicht anders möglich, als wenn die Seiten des ABC, auf denen er sich am Anfange und am Ende seines Weges befindet, einander entgegengesetzt sind.*")

;; ## The strip of paper
;;
;; Möbius gives the picture himself, with a rectangular strip of paper
;; $ABB'A'$:

(page/blockquote
 "One can also, provided the pair of parallel edges AA' and BB' is large enough against the other pair, bring A' to B and B' to A, by first holding one end AB of the strip fixed and turning the other end A'B' half round the longitudinal axis. The surface so formed has only one boundary line [...]. It also has only one side; for if one begins to paint it from any place with a colour and goes on, without taking the brush over the boundary line to the other side, then at the end the two opposite sides of the surface will nevertheless be coloured at every place."
 "Möbius, Werke II, pp. 484-485, §11; our translation. *Man kann aber auch, dafern das eine Paar paralleler Kanten AA' und BB' gegen das andere AB und A'B' hinreichend gross ist, A' mit B, und B' mit A zur Coincidenz bringen, indem man zuvor, das eine Ende AB des Streifens festhaltend, das andere Ende A'B' um die Längenaxe des Streifens halb herumdreht. [...] Auch hat diese Fläche nur eine Seite; denn wenn man sie von einer beliebigen Stelle aus mit einer Farbe zu überstreichen anfängt [...] so werden nichtsdestoweniger zuletzt an jeder Stelle die zwei daselbst einander gegenüberliegenden Seiten der Fläche gefärbt sein.*")

;; Choose the number of half twists $t$ and drag the arrow once round the
;; core of the strip. The strip is cut into cells; $V$, $E$, $F$ are counted
;; from them, and the orientation is tested on the cells, not read off the
;; picture. For $t$ even (a ring, or an annulus) the arrow returns the way it
;; left; for $t$ odd it returns reversed. The *orientation character* of the
;; core loop $\mathbb{Z}/2$ is $\chi(a) = (-1)^{ta}$: trivial for even $t$,
;; non-trivial for odd $t$. The Euler characteristic is $0$ in every case. It
;; does not see the twist; the orientation does.

(w/band-widget)

;; Cutting along the core gives two bands of the same twisting when $t$ is
;; even, and one band twice as long, with $2t$ half twists (so two-sided),
;; when $t$ is odd. Try the cut for $t = 1$ and $t = 2$. The boundary is one
;; curve for $t$ odd, two for $t$ even; Möbius counted the same in the
;; polyhedron: a zone of an odd number $2n+1$ of triangles has a single
;; $(2n+1)$-gon as boundary, one of an even number $2n$ has two $n$-gons
;; (§10, Werke II, p. 484).
;;
;; ## Which solids have a volume
;;
;; The content of a closed polyhedron is the sum, over its faces, of the
;; signed volume of the pyramid from a chosen corner to the face, each face
;; taken in its sense. If the senses obey the law of opposite edges, the
;; result does not depend on the corner. Check it on the cube, on the same
;; cube with the top face written backwards (an error of the sense, repairable
;; by reversing that face), and on the two cases where no choice of senses
;; works: Möbius' decahedron and the twisted strip.

(w/content-widget)

;; The first two show that the sum of pyramids is not a property of the
;; faces alone but of their senses; the last two that for some surfaces no
;; consistent sense exists, so the content is not a number.

;; ## What the idea became
;;
;; Möbius did not draw the topological moral; that came with Listing's
;; *Vorstudien zur Topologie* (1848, published 1847-48) and, above all,
;; with Riemann's surfaces and Poincaré's *Analysis situs* (1895), where
;; orientability and the Euler characteristic became two of the first
;; invariants of a surface, and then Betti's numbers (1871). The two
;; invariants are independent: $\chi$ is a count of cells and does not change
;; when cells are subdivided, while orientation is a homomorphism of the
;; loops of the surface to $\{+1,-1\}$. In modern terms it is the first
;; Stiefel-Whitney class $w_1 \in H^1(X;\mathbb{Z}/2)$.

^{::clerk/no-cache true}
(page/prev-next "notebooks/topology_moebius_band_1865.clj")

(lens/section "topology-moebius-band-1865")
