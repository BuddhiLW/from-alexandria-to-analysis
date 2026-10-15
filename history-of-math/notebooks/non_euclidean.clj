;; # Lobachevsky, Bolyai, Beltrami: geometry without the parallel postulate
;;
;; For two thousand years Euclid's fifth postulate was the one that looked
;; like a theorem waiting for its proof. Ptolemy, Proclus, al-Haytham,
;; Omar Khayyam, Wallis, Saccheri (1733) and Lambert (1766) all tried to
;; derive it from the other four; each proof assumed, somewhere, an
;; equivalent of what it set out to prove. Between 1829 and 1832 two men
;; who never met took the other road. Nikolai Ivanovich Lobachevsky, rector
;; of the university of Kazan, published *On the principles of geometry*
;; in the *Kazan Messenger* (1829-30) and summed it up in German in
;; *Geometrische Untersuchungen zur Theorie der Parallellinien* (Berlin
;; 1840). János Bolyai, a Hungarian officer of engineers, printed his
;; *Appendix scientiam spatii absolute veram exhibens* in 1832 at the end
;; of his father's textbook. Both assumed the postulate false and found
;; not a contradiction but a geometry. Gauss, who had reached the same
;; point privately, published nothing.
;;
;; Nobody could yet say whether that geometry was consistent. In 1868
;; Eugenio Beltrami gave it a model: on a surface of constant negative
;; curvature the geodesics obey every axiom but the fifth. The same year
;; Riemann's 1854 lecture was printed, and four years later Klein's
;; Erlangen programme made all these geometries chapters of one theory.
;;
;; We quote Halsted's English of Lobachevsky (Austin 1891) and of Bolyai
;; (Austin 1896), and Beltrami's Italian (*Opere matematiche* I, 1902).
;; The figures and checks come from Alexandria's shelf
;; `alexandria.non-euclidean`: *proved* means Emmy simplified a difference
;; to exactly 0, *numeric* that a number from raster was within the bound.
^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns non-euclidean
  {:history/year 1829
   :history/title "Lobachevsky, Bolyai, Beltrami: non-Euclidean geometry"
   :history/era "Structures, foundations and computation"
   :history/fields #{:geometry :differential-geometry}
   :nextjournal.clerk/visibility {:code :hide :result :show}
   :nextjournal.clerk/toc true}
  (:require [alexandria.non-euclidean.disk :as disk]
            [alexandria.non-euclidean.parallels :as parallels]
            [alexandria.non-euclidean.pseudosphere :as pseudosphere]
            [alexandria.non-euclidean.widgets :as w]
            [history-of-math.lens :as lens]
            [history-of-math.page :as page]
            [history-of-math.widgets :as hom]
            [nextjournal.clerk :as clerk]))

^{::clerk/visibility {:code :hide :result :hide}}
(hom/install!)

;; ## The fifth postulate and the angle of parallelism (1829-1840)
;;
;; Euclid's last postulate, in Heath's English of Book I:

(page/blockquote
 "That, if a straight line falling on two straight lines make the interior angles on the same side less than two right angles, the two straight lines, if produced indefinitely, meet on that side on which are the angles less than the two right angles."
 "Euclid, *Elements* I, Postulate 5, tr. T. L. Heath (1908).")

;; Equivalently (Playfair, 1795): through a point not on a line there is
;; exactly one line that does not meet it. Lobachevsky does not assume
;; this. From a point $A$ he drops the perpendicular $AD$ to the line $BC$
;; and sorts the lines through $A$:

(page/blockquote
 "All straight lines which in a plane go out from a point can, with reference to a given straight line in the same plane, be divided into two classes — into cutting and not-cutting. The boundary lines of the one and the other class of those lines will be called parallel to the given line."
 "Lobachevsky, *Geometrical researches on the theory of parallels* (1840), §16, tr. G. B. Halsted (Austin 1891).")

;; Between the cutting lines $AF$ and the not-cutting lines $AG$ there must
;; be a boundary $AH$, and the angle it makes with the perpendicular
;; depends only on the distance $p = AD$:

(page/blockquote
 "The angle HAD between the parallel HA and the perpendicular AD is called the parallel angle (angle of parallelism), which we will here designate by Π(p) for AD = p."
 "Lobachevsky, §16, tr. Halsted.")

;; If $\Pi(p)$ is a right angle for every $p$, the boundary is the single
;; perpendicular $AE$ and we are in Euclid. If not, there are two
;; boundaries, one on each side, and every line between them misses $BC$.
;; In §36 Lobachevsky finds how $\Pi$ depends on the distance; with the
;; unit of length chosen so that the base is Napier's $e$,
;;
;; $$\tan\frac{\Pi(x)}{2} = e^{-x}.$$
;;
;; Halsted's text: "we may also understand by e the base of the Napierian
;; Logarithms" (§36). Emmy states the relation and its inverse and checks
;; that each undoes the other:

(w/derivation-parallels)

(page/grade-badges (parallels/graded) #{:grade/proved :grade/numeric})

;; Now try it yourself: $x$, $\Pi(x)$ and $t = \tan(\Pi/2) = e^{-x}$ are
;; three tied controls; move any one and the other two are solved by the
;; raster kernel. Lobachevsky's figure is drawn in Poincaré's disk (the
;; model of the next section): $BC$ is a diameter, $A$ sits at distance
;; $x$ above $D$, and the two red lines are his parallels. Bring $A$ down
;; to the line and the two parallels close onto Euclid's $AE$: for small
;; $x$, $\Pi(x) \approx 90^\circ - x$ (in radians), so in a small region
;; the new geometry cannot be told from Euclid's.

(w/parallel-tie)
;; ## Bolyai's new world: triangles and parallels in the disk (1832)
;;
;; Bolyai reached the same geometry in Temesvár in 1823 and wrote to his
;; father, who had spent his own life on the postulate:

(page/blockquote
 "Now I can not say more, only so much: that from nothing I have created another wholly new world."
 "János Bolyai to Farkas Bolyai, 3 November 1823, in Halsted's translator's introduction to *The science absolute of space* (Austin 1896).")

;; To *see* that world we need a map of it, and every flat map distorts.
;; Two maps are classical. In **Poincaré's disk** (1882) the whole plane is
;; the inside of a circle, the rim is at infinite distance, the straight
;; lines (geodesics) are the arcs of circles that meet the rim at right
;; angles, and angles are drawn true. In the **Beltrami-Klein disk**
;; (Beltrami 1868, Klein 1871) the lines are straight chords, but angles
;; are not drawn true. The same point $z$ of Poincaré's disk sits at
;; $k = 2z/(1 + |z|^2)$ in Klein's. Distances are
;;
;; $$\cosh d(p,q) = 1 + \frac{2|p-q|^2}{(1-|p|^2)(1-|q|^2)},$$
;;
;; and the Möbius map $T_p(z) = (z-p)/(1-\bar p z)$ is an isometry that
;; carries $p$ to the centre, where lines are diameters. Through it the
;; page computes every angle; the hyperbolic law of cosines
;; $\cosh c = \cosh a\cosh b - \sinh a\sinh b\cos C$ computes them again
;; from the sides.
;;
;; The striking theorem is about the angle sum. Lobachevsky and Bolyai both
;; found that it is less than two right angles, and that the defect
;; measures the area. In Gauss' language (*Disquisitiones generales*,
;; 1827, the curvatura integra of a geodesic triangle) with curvature
;; $K = -1$:
;;
;; $$\text{area} = \pi - (A + B + C).$$
;;
;; So no triangle has area more than $\pi$, the area of the *ideal*
;; triangle whose three vertices are on the rim and whose angles are all
;; zero. Emmy and raster check the machinery:

(page/grade-badges (disk/graded) #{:grade/proved :grade/numeric})

;; Now try it yourself: drag the vertices $A$, $B$, $C$. The sides are
;; drawn as geodesics; the angles, their sum $S$ and the area $\pi - S$ are
;; read from one raster kernel and named live: almost Euclidean near the
;; centre, hyperbolic, ideal at the rim. Switch to Beltrami-Klein: the
;; sides become straight and the drawn triangle has Euclidean angle sum
;; $180^\circ$, though its true sum (the badge) has not changed.

(w/triangle-playground)

;; Now the parallels themselves. Drag $P$ above the line $BC$ (a diameter).
;; The two red lines through $P$ are Lobachevsky's parallels: they meet
;; $BC$ only on the rim, at infinity. Every orange line between them is a
;; line through $P$ that never meets $BC$, infinitely many where Euclid
;; allows one. Turn the black line with the slider and the page names
;; whether it cuts $BC$, is a limiting parallel (its angle with $PD$ equals
;; $\Pi(d)$, the readout of section 1), or misses. Bring $P$ down to the
;; line and the two parallels close up into one.

(w/parallels-playground)

;; ## Beltrami's pseudosphere: a model, and consistency (1868)
;;
;; Lobachevsky and Bolyai had derived a long chain of theorems without a
;; contradiction, but that is not a proof that none will come. Beltrami
;; settled it in the *Saggio di interpretazione della geometria
;; non-euclidea* (1868). Gauss had shown that the curvature $K$ of a
;; surface depends only on its line element $E\,du^2 + 2F\,du\,dv +
;; G\,dv^2$; Beltrami observed that on a surface of constant negative
;; curvature the geodesic triangles obey exactly Lobachevsky's
;; trigonometry, with the constant $k$ of his formulas the radius of the
;; surface. Of Lobachevsky's constant, which Gauss had said experience
;; shows to be very large, Beltrami writes that it is nothing other than

(page/blockquote
 "il raggio di quella superficie pseudosferica che noi introduciamo inconsapevolmente nella planimetria al posto del piano euclideo"
 "Beltrami, *Saggio di interpretazione della geometria non-euclidea*, Giornale di Matematiche 6 (1868), in *Opere matematiche* I (Roma 1902). (\"the radius of that pseudospherical surface which we unknowingly put into plane geometry in place of the Euclidean plane\")")

;; The simplest such surface is the **pseudosphere**, the surface of
;; revolution of the tractrix:
;;
;; $$r(u,v) = (R\,\mathrm{sech}\,v\cos u,\; R\,\mathrm{sech}\,v\sin u,\; R\,(v - \tanh v)).$$
;;
;; Emmy computes its first fundamental form, and the curvature from $E$,
;; $F$, $G$ alone with Brioschi's formula, the same
;; `alexandria.gauss.egregium` the Gauss notebook uses for the *Theorema
;; egregium*:

(w/derivation-pseudosphere)

(page/grade-badges (pseudosphere/graded) #{:grade/proved :grade/numeric})

;; Now try it yourself: orbit the surface (drag to rotate, scroll to zoom)
;; and move the point $P$ with the sliders. $K$ at $P$ is computed from
;; $E, F, G$ at every move, and from the embedding as a cross-check; the
;; badge stays green wherever $P$ goes, because the curvature is the same
;; constant $-1/R^2$ everywhere. The red meridian is a geodesic (a
;; tractrix); the green circle is not. Switch to the sphere: $K = +1/R^2$,
;; Riemann's elliptic case, where triangles have angle sum *above* $\pi$.
;;
;; The pseudosphere carries only a piece of the hyperbolic plane (it has a
;; rim, and it wraps around); Hilbert proved in 1901 that no complete
;; surface in space carries all of it. The disks of section 2 do: Beltrami
;; himself drew the disk with chords in the same 1868 paper, the picture
;; that Klein (1871) recognised as projective geometry inside a conic,
;; with Cayley's cross-ratio as the distance. That is the road to the
;; Erlangen programme (1872, see the Erlangen deck at /erlangen-program/
;; and the projective deck at /projective/): Euclidean, hyperbolic and
;; elliptic geometry are the invariants of three groups acting on one
;; projective plane.

(w/pseudosphere)

^{::clerk/no-cache true}
(page/prev-next "notebooks/non_euclidean.clj")

(lens/section "non-euclidean")
