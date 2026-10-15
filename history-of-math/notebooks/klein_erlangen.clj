;; # Klein, the Erlangen programme: a geometry is a group
;;
;; By 1870 there were too many geometries. Euclid's metric geometry had been
;; joined by Poncelet's projective geometry (1822), by the geometries of
;; Lobachevsky and Bolyai without the parallel postulate, by Riemann's
;; manifolds of any dimension and curvature (1854), by Plücker's line
;; geometry, by Möbius' and Lie's geometries of circles and spheres. Each
;; had its own methods, and nobody could say how they were related.
;;
;; In October 1872 Felix Klein, twenty-three years old, was appointed
;; professor at Erlangen. The custom asked for a printed *Programm* on
;; entering the faculty, and his was a pamphlet of 48 pages, *Vergleichende
;; Betrachtungen über neuere geometrische Forschungen* (A comparative
;; review of recent researches in geometry). Its idea, worked out with
;; Sophus Lie in Paris in 1870 and with Camille Jordan's *Traité des
;; substitutions* (1870) at hand, is in one sentence: a geometry is the study
;; of the properties of figures that a group of transformations does not
;; change. The pamphlet had, Klein says himself, "but a limited circulation
;; at first"; it became famous through the translations of the 1890s. We
;; quote the English of Mellen W. Haskell (Bulletin of the New York
;; Mathematical Society 2, 1893, pp. 215-249) and Klein's German (Erlangen,
;; A. Deichert, 1872).
;;
;; The figures and checks come from Alexandria's shelf
;; `alexandria.klein`: *proved* means Emmy simplified a difference to exactly
;; 0, *numeric* that a number from raster was within the bound.
^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns klein-erlangen
  {:history/year 1872
   :history/title "Klein, the Erlangen programme: geometry as invariants of a group"
   :history/era "Structures, foundations and computation"
   :history/fields #{:geometry :algebra :abstract-algebra}
   :nextjournal.clerk/visibility {:code :hide :result :show}
   :nextjournal.clerk/toc true}
  (:require [alexandria.klein.erlangen :as erlangen]
            [alexandria.klein.widgets :as w]
            [history-of-math.lens :as lens]
            [history-of-math.page :as page]
            [history-of-math.widgets :as hom]
            [nextjournal.clerk :as clerk]))

^{::clerk/visibility {:code :hide :result :hide}}
(hom/install!)

;; ## Klein's problem, in his own words (1872)
;;
;; Klein starts from ordinary space. Some transformations change nothing a
;; geometer cares about: motions, changes of scale, reflections. He calls
;; them together the *principal group*, and turns the observation around:

(page/blockquote
 "The totality of all these transformations we designate as the principal group of space-transformations; geometric properties are not changed by the transformations of the principal group. And, conversely, geometric properties are characterized by their remaining invariant under the transformations of the principal group."
 "Klein, *A comparative review of recent researches in geometry* (1872), §1, tr. M. W. Haskell, Bull. New York Math. Soc. 2 (1893), p. 218.")

;; Then he drops space itself. A *manifoldness* of $n$ dimensions has
;; transformations too, they form groups, and no group is privileged:

(page/blockquote
 "Given a manifoldness and a group of transformations of the same; to develop the theory of invariants relating to that group."
 "Klein, §1, tr. Haskell, p. 219. In the German of 1872: *Es ist eine Mannigfaltigkeit und in derselben eine Transformationsgruppe gegeben. Man entwickele die auf die Gruppe bezügliche Invariantentheorie.*")

(page/blockquote
 "This is the general problem, and it comprehends not alone ordinary geometry, but also and in particular the more recent geometrical theories which we propose to discuss, and the different methods of treating manifoldnesses of n dimensions."
 "Klein, §1, tr. Haskell, p. 219.")

;; So the plane carries not one geometry but a chain of them, one for each
;; group. The four classical ones on the plane, each group containing the
;; previous one:
;;
;; $$\text{motions} \subset \text{similarities} \subset \text{affine maps} \subset \text{projective maps}.$$
;;
;; Step through the chain. Each step applies one map of the next group to
;; the same figure (a triangle with its circumcircle, the line through $C$
;; parallel to $AB$, the midpoint $M$ of $AB$, four points $D, C, F, E$ on a
;; line), and the list of properties that survived is computed live by one
;; raster kernel:

(w/hierarchy-player)

;; Klein (§3) says why projective geometry came to stand on its own: it
;; arose only "as it became customary to regard the original figure as
;; essentially identical with all those deducible from it by projection".
;; Emmy proves the two facts the player leans on, that the cross-ratio
;; survives every projective map of a line and that an affine map keeps
;; midpoints, and raster checks the rest:

(w/derivation)

(page/grade-badges (erlangen/graded) #{:grade/proved :grade/numeric})

;; ## Pick a group, watch what survives
;;
;; Now try it yourself. Choose a group, then move its map: the sliders are
;; exactly the group's parameters (3 for motions, 4 for similarities, 6 for
;; affine maps, 8 for projective maps), and the red handle on the figure
;; drags the translation. Drag $A$, $B$, $C$ to change the figure itself.
;; Each property carries a badge computed from the two numbers beside it:
;; **invariant** when the map left it unchanged, **not invariant** when it
;; moved. The last group is not in the chain: the motions of
;; Poincaré's disk, the Möbius maps of
;; [the non-Euclidean notebook](../non-euclidean/) (alexandria's
;; `alexandria.non-euclidean.disk`). They keep the hyperbolic length
;; $d(A,B)$, angles and circles, and destroy Euclid's lengths, parallels and
;; midpoints. The line under the table names the geometry of the map you
;; made. If you pick a large group but move only the parameters of a
;; smaller one, it names the smaller group: your map is in it.

(w/playground)

;; What the playground shows, for one generic map of each group (raster on
;; the server; the same kernel as above):

(w/invariant-table)

;; ## Smaller group, more invariants: the nesting of geometries
;;
;; Read the table from the bottom up. The projective group is the largest,
;; with 8 parameters, and keeps only the cross-ratio (and incidence). Every
;; subgroup keeps that and more: the affine maps add parallels and
;; midpoints, the similarities angles and circles, the motions lengths.
;; This is Klein's §2, where groups "one of which includes the other, are
;; successively adjoined":

(page/blockquote
 "it is exactly the same thing whether we investigate the configurations of space taken in connection with the given point from the point of view of the principal group or whether, without any such connection, we replace the principal group by that partial group whose transformations leave the point in question unchanged."
 "Klein, §2, tr. Haskell, p. 219.")

;; Fixing something (a point, a line, a conic) shrinks the group, and the
;; geometry of the smaller group can be read as the geometry of the larger
;; one *together with the fixed thing*. Metric geometry is projective
;; geometry relative to the circle at infinity:

(page/blockquote
 "Metrical properties are to be considered as projective relations to a fundamental configuration, the circle at infinity, a configuration having the property that it is transformed into itself only by those transformations of the projective group which belong at the same time to the principal group."
 "Klein, §3, tr. Haskell, pp. 221-222.")

;; Replace the circle at infinity by a real conic, and the projective maps
;; that keep the conic form a different subgroup: its invariant theory is
;; Lobachevsky's geometry, with Cayley's cross-ratio as the distance. That
;; is Klein's own model of 1871 (*Ueber die sogenannte Nicht-Euklidische
;; Geometrie*), the Beltrami-Klein disk of
;; [the non-Euclidean notebook](../non-euclidean/), and the reason the
;; hyperbolic group sits in the playground. Riemann's manifolds of
;; [1854](../riemann-hypothesen/) are the other road: a metric first, its
;; group of isometries second, and for most metrics that group is trivial.
;; Klein's programme reaches the homogeneous ones, where the group moves
;; every point to every other.
;;
;; The idea outgrew geometry. Lie's continuous groups, Poincaré's
;; automorphic functions, Noether's theorem (1918: every symmetry of a
;; physical law has its conserved quantity), the relativity of 1905
;; (physics as the invariants of the Lorentz group) and Bourbaki's
;; structures all speak Klein's language. The live
;; [Erlangen deck](/erlangen-program/) follows the programme through its
;; groups, the [projective deck](/projective/) builds the largest of the
;; four planar geometries, and a group-theory journey is planned.

^{::clerk/no-cache true}
(page/prev-next "notebooks/klein_erlangen.clj")

(lens/section "klein-erlangen")
