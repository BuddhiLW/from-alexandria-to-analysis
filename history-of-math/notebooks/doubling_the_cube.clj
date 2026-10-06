;; # The doubling of the cube
;;
;; Around 430 BC Greek geometers met a problem they could state in one line
;; and could not solve: given a cube, construct the side of a cube twice as
;; large. They called it the doubling of the cube. Later writers called it
;; the Delian problem, after the oracle of Delos that, in the best known
;; telling, demanded a doubled altar.
;;
;; The problem shaped Greek geometry for two centuries. Hippocrates turned
;; it into the search for two mean proportionals. Archytas solved it in
;; three dimensions with a cone, a cylinder and a ring. Menaechmus, to solve
;; it, found the curves we call the parabola and the hyperbola: the conic
;; sections begin here. Eratosthenes built an instrument for it; Nicomedes
;; and Diocles invented new curves for it. In 1837 Pierre-Laurent Wantzel
;; proved what all of them had found by trial: ruler and compass alone
;; cannot do it.
;;
;; Our source for the ancient solutions is Eutocius of Ascalon (about AD
;; 500), who collected them in his commentary on Archimedes' *Sphere and
;; Cylinder* II.1. The Greek is J. L. Heiberg's edition (*Archimedis opera
;; omnia* III, Teubner 1881); the English is T. L. Heath's (*Apollonius of
;; Perga*, 1896) where Heath translates, and the series' own rendering,
;; marked "[series]", where he does not.
^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns doubling-the-cube
  {:history/year -350
   :history/title "The doubling of the cube (the Delian problem)"
   :history/era "Greek geometry"
   :nextjournal.clerk/visibility {:code :hide :result :show}
   :nextjournal.clerk/toc true}
  (:require [alexandria.delian.duplication :as d]
            [alexandria.delian.duplication-view :as view]
            [alexandria.delian.duplication-widgets :as w]
            [alexandria.proofs :as proofs]
            [history-of-math.page :as page]
            [history-of-math.widgets :as hom]
            [nextjournal.clerk :as clerk]))

^{::clerk/visibility {:result :hide}}
(hom/install!)

^{::clerk/visibility {:result :hide}}
(def graded (d/graded))

^{::clerk/visibility {:result :hide}}
(def numbers (view/data))

;; ## 1. The problem and its story
;;
;; Eratosthenes of Cyrene, librarian at Alexandria, told the story in a
;; letter to King Ptolemy III (about 240 BC). Eutocius copied the letter
;; whole. It opens:
;;
;; > *Βασιλεῖ Πτολεμαίῳ Ἐρατοσθένης χαίρειν.*
;; >
;; > — Heiberg III (1881), p. 102
;;
;; > *There is a story that one of the old tragedians represented Minos as
;; > wishing to erect a tomb for Glaucus and as saying, when he heard that it
;; > was a hundred feet every way, "Too small thy plan to bound a royal tomb.
;; > Let it be double; yet of its fair form fail not, but haste to double
;; > every side."*
;; >
;; > — Eratosthenes, in Heath 1896, p. xvii
;;
;; > *But he was clearly in error; for, when the sides are doubled, the area
;; > becomes four times as great, and the solid content eight times as
;; > great.*
;; >
;; > — Heath 1896, p. xviii
;;
;; Then the oracle. The Delians, told to double an altar, fell into the
;; same difficulty and sent to Plato's Academy:
;;
;; > *μετὰ χρόνον δέ τινάς φασι Δηλίους ἐπιβαλλομένους κατὰ χρησμὸν
;; > διπλασιάσαι τινὰ τῶν βωμῶν ἐμπεσεῖν εἰς τὸ αὐτὸ ἀπόρημα.*
;; >
;; > — Heiberg III (1881), p. 104
;;
;; > *Afterwards, they say, some Delians attempting, in accordance with an
;; > oracle, to double one of the altars fell into the same difficulty.*
;; >
;; > — Heath 1896, p. xviii
;;
;; Why is the square easy and the cube hard? In Plato's *Meno* (84d-85b)
;; Socrates shows a slave boy that the square on the diagonal of a square
;; is twice the square: one line, drawn with a ruler, doubles it. That is
;; *Elements* I.47 for the isosceles right triangle. The cube has no such
;; line. Its double needs a side $s$ with $s^3 = 2a^3$, and no diagonal of
;; anything gives it.

(view/hippocrates)

;; ## 2. Hippocrates: two mean proportionals
;;
;; Hippocrates of Chios (about 430 BC), the first writer of *Elements* we
;; know of, changed the question:
;;
;; > *πρῶτος Ἱπποκράτης ὁ Χῖος ἐπενόησεν, ὅτι, ἐὰν εὑρεθῇ δύο εὐθειῶν
;; > γραμμῶν ... δύο μέσας ἀνάλογον λαβεῖν ἐν συνεχεῖ ἀναλογίᾳ,
;; > διπλασιασθήσεται ὁ κύβος.*
;; >
;; > — Heiberg III (1881), p. 104
;;
;; > *Hippocrates of Chios was the first to observe that, if between two
;; > straight lines of which the greater is double of the less it were
;; > discovered how to find two mean proportionals in continued proportion,
;; > the cube would be doubled.*
;; >
;; > — Heath 1896, p. xviii
;;
;; Two *mean proportionals* between $a$ and $b$ are lengths $x$, $y$ with
;;
;; $$a : x = x : y = y : b.$$
;;
;; Then the ratio $a : b$ is the ratio $a : x$ taken three times over, so
;; $(a/x)^3 = a/b$ and $x^3 = a^2 b$. With $b = 2a$, $x^3 = 2a^3$: the cube
;; on the first mean is double the cube on $a$. The steps:

(page/steps (proofs/steps d/proofs-resource :delian/hippocrates))

;; Emmy checks the algebra with $a$ and $t$ left as symbols (the means
;; written $x = at$, $y = at^2$, $b = at^3$), so it holds for every length:

(page/grade-badges (:hippocrates graded))

;; **The pattern.** A continued proportion is Eudoxus' theory of ratio at
;; work: "the ratio taken three times" is the triplicate ratio of
;; *Elements* V, Definition 10, and Eudoxus was Menaechmus' teacher. One
;; mean proportional, $a : x = x : b$, gives a square root (*Elements*
;; VI.13); two means give a cube root. See the
;; [Eudoxus notebook](../eudoxus/).

;; ## 3. Archytas: a cone, a cylinder and a ring
;;
;; Archytas of Tarentum (about 400 BC), a Pythagorean and a friend of Plato,
;; found the means as a point in space. Eutocius took the solution from
;; Eudemus' lost *History of Geometry*:
;;
;; > *Ἡ Ἀρχύτου εὕρησις, ὡς Εὔδημος ἱστορεῖ.*
;; >
;; > — Heiberg III (1881), p. 98
;;
;; > *This solution, in itself perhaps more remarkable than any other,
;; > determines a certain point as the intersection of three surfaces of
;; > revolution.*
;; >
;; > — Heath 1896, p. xxii
;;
;; The construction puts four objects in space, one per sentence. Step
;; through them below (click a sentence, or ◀ and ▶▶): each step adds
;; exactly what its sentence introduces, with $AD = 2$ and the chord $AB$
;; on the slider. The half-cylinder is blue, the ring amber, the cone
;; green. Drag to rotate, scroll to zoom, right-drag to pan. MathBox draws
;; the surfaces through emmy-viewers' raster backend, and $B$, $K$, $I$ and
;; the numbers beside the figure come from one raster kernel:

(w/archytas)

;; The same construction in Eutocius' lettering, as a proof:

(page/steps (proofs/steps d/proofs-resource :delian/archytas))

;; With $AB = 1$ the meeting point also comes from a raster Brent root of
;; the meeting condition $AI^4/AB^2 = AD \cdot AI$ ($AI = AM$ in Heath's
;; letters), computed on the JVM:

(clerk/md (format "$AI = %.12f$ and $AK = %.12f$; their cubes are 2 and 4 (raster)."
                  (get-in numbers [:archytas :am]) (get-in numbers [:archytas :ap])))


;; Emmy proves that this point lies on all three surfaces (Heath's
;; equations, p. xxiii) and that the four lines are in continued
;; proportion:

(page/grade-badges (:archytas graded))

;; ## 4. Menaechmus: the birth of the conic sections
;;
;; Menaechmus, a pupil of Eudoxus and a contemporary of Plato (about 350
;; BC), did the problem in the plane. He asked where the point lies whose
;; coordinates are the two means. From $a : x = x : y = y : b$ come three
;; equations:
;;
;; $$x^2 = a y, \qquad y^2 = b x, \qquad x y = a b.$$
;;
;; Each is a curve. The first two are what we call parabolas; the third is
;; a hyperbola, drawn in its asymptotes. Any two of them meet at the point
;; whose coordinates are the means. Eutocius gives both ways:
;;
;; > *τὸ ἄρα ὑπὸ δοθείσης τῆς Α καὶ τῆς ΔΖ ἴσον ἐστὶ τῷ ἀπὸ τῆς ΖΘ.*
;; >
;; > — Heiberg III (1881), p. 92
;;
;; "So the rectangle under the given line A and DZ equals the square on
;; ZΘ" [series]: that is $y^2 = A x$, the parabola, as a *locus*, the place
;; of all points with a property.

(page/steps (proofs/steps d/proofs-resource :delian/menaechmus))

;; The three loci, one per sentence, with $b = 2a$. Step through them, then
;; move $a$ in the panel: the parabola $x^2 = a y$, the parabola
;; $y^2 = 2a x$ and the hyperbola $x y = 2a^2$ (dashed) are drawn by raster
;; kernels, and they always meet at one point $\Theta$ whose abscissa is $a$
;; times the cube root of 2:

(w/menaechmus)

;; The cube root itself is a raster Brent root of $x^3 - 2$:

(clerk/md (format "$\\sqrt[3]{2} = %.15f$ (raster brent)." (:cube-root-2 numbers)))

(page/grade-badges (:menaechmus graded))

;; **What this means.** The curves that Apollonius named *parabola*,
;; *ellipse* and *hyperbola* about 150 years later appear here first, as
;; loci serving this one problem. Menaechmus did not have those names. He
;; cut each curve from a right circular cone by a plane at right angles to
;; a generator: the "section of a right-angled cone" (our parabola), of an
;; "acute-angled cone" (the ellipse) and of an "obtuse-angled cone" (the
;; hyperbola). Archimedes still uses those names. Eratosthenes' epigram
;; calls them "the triads of Menaechmus". Heath:
;;
;; > *Thus the evidence so far shows (1) that Menaechmus (a pupil of Eudoxus
;; > and a contemporary of Plato) was the discoverer of the conic sections,
;; > and (2) that he used them as a means of solving the problem of the
;; > doubling of the cube.*
;; >
;; > — Heath 1896, p. xix
;;
;; Apollonius cut all three from one cone and gave them their names: see
;; the [Apollonius notebook](../apollonius-conics/).

;; ## 5. Machines and new curves
;;
;; ### Eratosthenes' mesolabe
;;
;; Eratosthenes' letter ends with his own device, the *mesolabe*
;; ("mean-taker"): three equal frames, each with a diagonal, slid over one
;; another until four points line up. He set it up on a column with an
;; epigram:
;;
;; > *μηδὲ σύ γ᾽ Ἀρχύτεω δυσμήχανα ἔργα κυλίνδρων μηδὲ Μενεχμείους
;; > κωνοτομεῖν τριάδας δίζηαι.*
;; >
;; > — Heiberg III (1881), p. 112

(page/steps (proofs/steps d/proofs-resource :delian/mesolabe))

(view/mesolabe)

;; Emmy proves that at every slide the four points lie on one line and
;; their heights are in continued proportion; raster finds the slide where
;; the line reaches the given end:

(page/grade-badges (:mesolabe graded))

;; ### Nicomedes' conchoid
;;
;; Nicomedes (about 200 BC) drew a curve with an instrument: a ruler, a pole
;; and a fixed interval. On each line through the pole, mark the point one
;; interval beyond the ruler. The curve does a *neusis*: it fits a segment
;; of given length between two lines, pointing at a pole. Nicomedes reduced
;; the two means to one neusis.

(page/steps (proofs/steps d/proofs-resource :delian/conchoid))

(view/conchoid)

(page/grade-badges (:conchoid graded))

;; ### Diocles' cissoid
;;
;; Diocles (about 180 BC), in his book *On burning mirrors*, drew a curve in
;; a circle whose ordinates give two means directly.

(page/steps (proofs/steps d/proofs-resource :delian/cissoid))

(view/cissoid)

(page/grade-badges (:cissoid graded))

;; ## 6. Why the problem is so important
;;
;; **It created the conic sections.** Menaechmus found the parabola and the
;; hyperbola to double the cube. From them came Apollonius' *Conics* (about
;; 200 BC), the ellipse Kepler found Mars travels on (1609), the orbits
;; Newton derived from gravitation (1687), and Descartes' analytic
;; geometry, which writes curves as equations (1637). In this series:
;; [Apollonius](../apollonius-conics/), [Kepler](../kepler-laws/),
;; [Newton](../newton-principia/), [Descartes and Fermat](../descartes-fermat/).
;;
;; **It forced the Greeks to classify constructions.** A problem done with
;; ruler and compass is *plane*; one that needs conics is *solid*; one that
;; needs other curves is *linear*. Pappus states the rule (about AD 320):
;;
;; > *Those which can be solved by means of a straight line and a
;; > circumference of a circle may properly be called plane; ... Those
;; > however which are solved by using for their discovery one or more of
;; > the sections of the cone have been called solid.*
;; >
;; > — Pappus IV, pp. 270-272, in Heath, *The Works of Archimedes* (1897),
;; > p. lxvi
;;
;; The doubling of the cube is the model solid problem.
;;
;; **It drove new curves and instruments.** The conchoid, the cissoid and
;; the mesolabe were made for it. They are early mechanical mathematics:
;; curves defined by a motion, numbers produced by a machine.
;;
;; **It gave Descartes his showcase.** Book III of *La Géométrie* (1637)
;; solves the cubic problems, the two means included, by one parabola and
;; one circle. Descartes reads the Greek classes as degrees of equations:
;; plane problems are quadratic, solid ones cubic or quartic. See
;; [Descartes and Fermat](../descartes-fermat/).
;;
;; **It was settled 2,200 years later.** In 1837 Pierre-Laurent Wantzel, a
;; 23-year-old engineering student, proved it impossible with ruler and
;; compass. His argument, in his order:

(page/steps (proofs/steps d/proofs-resource :delian/wantzel))

;; > *Tout problème qui conduit à une équation irréductible dont le degré
;; > n'est pas une puissance de 2, ne peut être résolu avec la ligne droite
;; > et le cercle.*
;; >
;; > — Wantzel, *J. math. pures appl.* 2 (1837), p. 369
;;
;; "Every problem that leads to an irreducible equation whose degree is not
;; a power of 2 cannot be solved with the straight line and the circle"
;; [series]. In modern words: $\sqrt[3]{2}$ has degree 3 over the rationals,
;; and every constructible number has degree a power of 2. The checks:

(page/grade-badges (:wantzel graded))

;; In the same paper Wantzel settles the trisection of the angle the same
;; way. The third classical problem, squaring the circle, fell in 1882, when
;; Ferdinand Lindemann proved that $\pi$ is not a root of any rational
;; equation. Wantzel's paper is one of the first proofs that a problem
;; *cannot* be solved, and its method, counting the degree of a tower of
;; square roots, leads from Greek geometry to field theory and to Galois'
;; theory of equations.

^{::clerk/no-cache true}
(page/prev-next "notebooks/doubling_the_cube.clj")
