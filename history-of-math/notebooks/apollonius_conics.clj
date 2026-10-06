;; # Apollonius of Perga, *Conics*: one cone, one equation, three names
^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns apollonius-conics
  {:history/year -200
   :history/title "Apollonius of Perga, Conics"
   :history/era "Hellenistic"
   :nextjournal.clerk/visibility {:code :fold :result :show}
   :nextjournal.clerk/toc true}
  (:require [alexandria.apollonius.areas-widget :as areas]
            [alexandria.apollonius.cone-widget :as cone]
            [alexandria.apollonius.conics-view :as conics]
            [alexandria.apollonius.notation-steps-view :as notation]
            [alexandria.apollonius.playground-widget :as playground]
            [clojure.string :as str]
            [history-of-math.page :as page]
            [history-of-math.widgets :as hom]
            [nextjournal.clerk :as clerk]))

^{::clerk/visibility {:code :hide :result :hide}}
(hom/install!)

^{::clerk/visibility {:result :hide}}
(defn badges
  "The Emmy grades of a proposition's checks, one badge each."
  [id]
  (clerk/html
   (into [:div {:style {:display "flex" :flex-wrap "wrap" :gap "0.4em" :margin "0.6em 0"}}]
         (for [{:keys [label grade]} (conics/grades id)
               :let [ok? (= :grade/proved grade)
                     tone (if ok? "#2f8a3e" "#c0392b")]]
           [:div {:style {:padding "0.15em 0.8em" :border-radius "1em" :font-size "0.82em"
                          :font-family "ui-monospace, monospace"
                          :border (str "1px solid " tone) :color tone}}
            (str (if ok? "proved by Emmy: " "FAILS: ") label)]))))

^{::clerk/visibility {:result :hide}}
(defn quote-block
  "Passages quoted verbatim, with their source."
  [{:keys [passages source]}]
  (clerk/md (str (str/join "\n\n" (map #(str "> " %) passages))
                 "\n\n*" source "*")))

;; Apollonius was born at Perga, on the south coast of Asia Minor, in the
;; reign of Ptolemy III (247-222 BC). He went young to Alexandria and studied
;; there under the successors of Euclid. He wrote eight books on the sections
;; of the cone. Four survive in Greek and three in Arabic; the eighth is lost.
;; His contemporaries called him the "great geometer."
;;
;; We read him in T. L. Heath's English, *Apollonius of Perga: Treatise on
;; Conic Sections* (Cambridge, 1896), and in J. L. Heiberg's Greek (Leipzig,
;; 1891). Heath numbers the propositions anew; "I.11" here is Apollonius' own
;; Book I, Proposition 11, which is Heath's Proposition 1. We keep Heath's
;; letters: the axial triangle $ABC$, the base diameter $BC$, the trace $DME$,
;; the diameter $PM$, the ordinate $QV$, the parameter $PL$.
;;
;; He sent the first books to Eudemus at Pergamum:

(quote-block (conics/passages :apollonius/preface))

;; ## Before Apollonius
;;
;; The curves were a century and a half old. Hippocrates of Chios had turned
;; the old problem of doubling the cube into a problem of two mean
;; proportionals. Menaechmus, a pupil of Eudoxus, found the means as the
;; meeting point of two curves cut from cones (the
;; [doubling of the cube](../doubling-the-cube/) tells that story).
;; Eratosthenes told it to King Ptolemy:

(quote-block (conics/passages :apollonius/menaechmus))

;; Menaechmus used three different cones, each with a different angle at the
;; apex (right, acute, obtuse), and cut each one by a plane at right angles
;; to a side. Euclid wrote four books of *Conics*, now lost, and Archimedes
;; still called the parabola "the section of a right-angled cone."
;;
;; Apollonius changed the starting point. He took **one** cone, any cone,
;; double, and cut it with planes at **every** tilt. All three curves come
;; out of the same cone, in four propositions of Book I: the parabola (I.11),
;; the hyperbola (I.12), the ellipse (I.13) and the opposite branches (I.14).
;; He gave them the names we still use.

;; ## Notation, fixed once
;;
;; Before I.11 Heath sets up the figure that all four propositions share:
;; "Suppose, as usual, that the plane of section cuts the circular base in a
;; straight line DME and that ABC is the axial triangle whose base BC is that
;; diameter of the base of the cone which bisects DME at right angles at the
;; point M" (p. 7). In our words:
;;
;; - The cone is the surface traced by a line through a fixed point, the
;;   **apex**, as the line runs round a circle. Continued past the apex, it
;;   makes a **double cone** of two **nappes**.
;; - The **axial triangle** $ABC$ is the cut through the apex $A$ and a
;;   diameter $BC$ of the base circle.
;; - The cutting plane meets the axial triangle in a line $PM$, the
;;   **diameter** of the section. $P$ is its **vertex**.
;; - A chord of the section parallel to the base line $DE$ is bisected by $PM$.
;;   Half of it, $QV$, is an **ordinate**; we write $y = QV$. The piece
;;   $PV = x$ of the diameter is the **abscissa**.
;; - $p$ is a length fixed by the cone and the plane, the **parameter**
;;   (Apollonius' "upright side," Latin *latus rectum*). $d = PP'$ is the
;;   **transverse diameter**, when the plane meets the opposite side of the
;;   axial triangle again at $P'$.
;;
;; The figure draws the same eight sentences one at a time, each adding only
;; what it names. Play runs through all eight; ◀ and ▶▶ (or a click on a
;; sentence) step. Drag the figure to turn it.

(notation/figure)

;; One more line belongs to the argument. Through the foot $V$ of an
;; ordinate, Apollonius draws $HK$ parallel to $BC$. The plane through $HK$
;; and $QQ'$ is parallel to the base, so it cuts the cone in a **circle** with
;; diameter $HK$. In a circle the square on a half-chord equals the rectangle
;; of the two pieces of the diameter (Euclid II.14, III.35):
;;
;; $$QV^2 = HV \cdot VK.$$
;;
;; Every one of I.11-13 begins with that circle. What changes from one to the
;; next is how similar triangles turn $HV$ and $VK$ into lengths on the
;; diameter.

;; ## I.11: the parabola
;;
;; > καλείσθω δὲ ἡ μὲν τοιαύτη τομὴ παραβολή
;; >
;; > *"let such a section be called a parabola"* (Heiberg, vol. I, p. 42)
;;
;; The case: the diameter $PM$ is parallel to the side $AC$ of the axial
;; triangle. Take $PL$ at right angles to $PM$, in the cutting plane, with
;;
;; $$PL : PA = BC^2 : BA \cdot AC.$$
;;
;; Heath's steps (pp. 8-9):
;;
;; 1. $HK$ through $V$ parallel to $BC$ gives the circle: $QV^2 = HV \cdot VK$.
;; 2. By similar triangles and parallels, $HV : PV = BC : AC$ and
;;    $VK : PA = BC : BA$.
;; 3. Compounding, $HV \cdot VK : PV \cdot PA = BC^2 : BA \cdot AC$.
;; 4. Hence $QV^2 : PV \cdot PA = PL : PA = PL \cdot PV : PV \cdot PA$.
;; 5. Therefore $QV^2 = PL \cdot PV$.
;;
;; Heath's conclusion: "It follows that the square on any ordinate to the
;; fixed diameter PM is equal to a rectangle applied to the fixed straight
;; line PL drawn at right angles to PM with altitude equal to the
;; corresponding abscissa PV. Hence the section is called a Parabola" (p. 9).
;;
;; The cone below opens at the tilt of I.11, with $PM$ parallel to $AC$. Its
;; letters are Heath's. The circle $HK$ through $V$ and the ordinate $QV$
;; are drawn. Beside the scene, three numbers are computed separately:
;; $QV^2$ (from $Q$ and $V$), $HV \cdot VK$ (from the circle) and
;; $PV \cdot VR$ (from the symptoma). They agree. Move $V$ along $PM$, raise
;; $P$, or turn the plane: a smaller tilt gives I.13, a larger one I.12 and
;; then I.14. Drag to rotate, scroll to zoom.

(cone/cone)

;; ## I.12: the hyperbola
;;
;; > καλείσθω δὲ ἡ μὲν τοιαύτη τομὴ ὑπερβολή
;; >
;; > *"let such a section be called a hyperbola"* (Heiberg, vol. I, p. 46)
;;
;; The case: $PM$ is not parallel to $AC$ but meets $CA$ produced beyond the
;; apex in $P'$. Draw $AF$ through $A$ parallel to $PM$, meeting $BC$ in $F$.
;; Take $PL$ at right angles to $PM$ with
;;
;; $$PL : PP' = BF \cdot FC : AF^2,$$
;;
;; draw $VR$ parallel to $PL$, and join $P'L$, produced to meet $VR$ in $R$.
;; Heath's steps (p. 10):
;;
;; 1. As before, $QV^2 = HV \cdot VK$.
;; 2. By similar triangles, $HV : PV = BF : AF$ and $VK : P'V = FC : AF$.
;; 3. So $HV \cdot VK : PV \cdot P'V = BF \cdot FC : AF^2$.
;; 4. Hence $QV^2 : PV \cdot P'V = PL : PP' = VR : P'V = PV \cdot VR : PV \cdot P'V$.
;; 5. Therefore $QV^2 = PV \cdot VR$.
;;
;; Heath: the square on the ordinate "is equal to a rectangle whose height is
;; equal to the abscissa and whose base lies along the fixed straight line PL
;; but overlaps it by a length equal to the difference between VR and PL.
;; Hence the section is called a Hyperbola" (p. 10).

;; ## I.13: the ellipse
;;
;; > καλείσθω δὲ ἡ μὲν τοιαύτη τομὴ ἔλλειψις
;; >
;; > *"let such a section be called an ellipse"* (Heiberg, vol. I, p. 52)
;;
;; The case: $PM$ meets $AC$ in $P'$ and $BC$ in $M$; $AF$, parallel to
;; $PM$, meets $BC$ produced in $F$. $PL$, $VR$ and $R$ are drawn as in I.12,
;; with $PL : PP' = BF \cdot FC : AF^2$. The steps are the same five, word for
;; word (p. 12), and end again in $QV^2 = PV \cdot VR$. Only the place of $R$
;; differs: now $VR$ is shorter than $PL$.
;;
;; Heath: the square on the ordinate "is equal to a rectangle whose height is
;; equal to the abscissa and whose base lies along the fixed straight line PL
;; but falls short of it by a length equal to the difference between VR and
;; PL. The section is therefore called an Ellipse" (p. 12).

(badges :apollonius/I.11-13)

;; Emmy builds the cone and the plane in space with letters, not numbers:
;; slope $k$ of the cone, tilt $\theta$, height $h$ of the vertex. From that
;; one construction it places $H$, $V$, $K$, $P$, $L$ and checks each claim by
;; simplifying the difference to $0$. The claims are written in the
;; vocabulary (`length`, `area`, `ratio`), never in coordinates.

;; ## I.14: the opposite branches
;;
;; > καλείσθωσαν δὲ αἱ τοιαῦται τομαὶ ἀντικείμεναι
;; >
;; > *"let such sections be called opposite"* (Heiberg, vol. I, p. 52)
;;
;; Heath's enunciation: "If a plane cuts both parts of a double cone and does
;; not pass through the apex, the sections of the two parts of the cone will
;; both be hyperbolas which will have the same diameter and equal latera
;; recta corresponding thereto. And such sections are called opposite
;; branches" (p. 13).
;;
;; The second nappe has its own base $B'C'$, parallel to $BC$, and the plane
;; cuts it in $D'E'$. The argument of I.12 applied to each nappe gives the
;; two hyperbolas, with the transverse $PP'$ running between their vertices
;; through the apex side. In the cone above, turn the tilt past I.12 until
;; $D'M'E'$ appears on $B'C'$: the second branch is drawn through $P'$ and
;; the caption adds I.14.

;; ## The names come from the application of areas
;;
;; Write $QV = y$, $PV = x$, $PL = p$, $PP' = d$. Heath's footnotes to I.12
;; and I.13 turn $QV^2 = PV \cdot VR$ into one equation for all three
;; propositions, the *symptoma* of the section:
;;
;; $$y^2 = p\,x + c\,x^2, \qquad c = -\frac{p}{d},$$
;;
;; with $d$ counted negative when $P'$ lies beyond the apex (I.12).
;;
;; The Pythagoreans had a standard problem, preserved in Euclid I.44 and
;; VI.28-29: **apply** to a given line a rectangle equal to a given area. The
;; rectangle may fit the line exactly, **exceed** it, or **fall short** of it.
;; Greek has a word for each: *parabolē* (application), *hyperbolē* (excess),
;; *elleipsis* (falling short). In Heiberg's I.12 the square on the ordinate
;; is a rectangle "ὑπερβάλλον", exceeding; in I.13 it is "ἐλλεῖπον", falling
;; short.
;;
;; | $c$ | the rectangle $PV \cdot VR$ on $PL$ | name |
;; |:---:|:---|:---|
;; | $c = 0$ | fits exactly: $y^2 = p x$ | parabola (I.11) |
;; | $c > 0$ | exceeds $VL$ by $LR = c\,x^2$ | hyperbola (I.12) |
;; | $c < 0$ | falls short of $VL$ by $LR$ | ellipse (I.13) |
;;
;; Heath's summary: "the parabola is the curve in which the rectangle which
;; is equal to $y^2$ is applied to $p$ and neither falls short of it nor
;; overlaps it, the ellipse and hyperbola are those in which the rectangle is
;; applied to $p$ but falls short of it, or overlaps it, respectively"
;; (p. lxxx).
;;
;; The panel draws it to scale in Heath's figure: the square on $QV$, the
;; rectangle $VL = PV \cdot PL$ under $PM$, and the rectangle $LR$ by which
;; $PV \cdot VR$ exceeds $VL$ (green) or falls short of it (red, dashed). Move
;; $V$ along $PM$; turn $e$ through $1$ and the name changes with the sign of
;; $c$. One sign more: the circle is the ellipse with $c = -1$, where $p = d$.

(areas/areas)

;; ## The foci (III.45-52)
;;
;; Apollonius has no word for a focus. In Book III he defines two points on
;; the axis by another application of areas: apply to the axis $AA'$ a
;; rectangle equal to a fourth of the figure $p \cdot AA'$, falling short (the
;; ellipse) or exceeding (the hyperbola) by a square. The points it marks,
;; $S$ and $S'$, are "the points arising out of the application." In letters,
;; with $a$ the semi-axis and $e$ the eccentricity:
;;
;; $$AS \cdot SA' = b^2 = (1 - e^2)\,a^2.$$
;;
;; From them he proves that the focal distances of any point $P$ make equal
;; angles with the tangent (III.48), and that "in an ellipse the sum, and in a
;; hyperbola the difference, of the focal distances of any point is equal to
;; the axis $AA'$" (III.51-52). The same sign that named the curves decides
;; which of the two holds.

(conics/focal)

(badges :apollonius/III.45)

(badges :apollonius/III.48)

(badges :apollonius/III.51-52)

;; III.48 is a mirror law. Light from one focus, reflected at the ellipse,
;; passes through the other. III.52 is a gardener's rule: pin a string of
;; length $AA'$ at $S$ and $S'$, keep it taut with a pencil, and the pencil
;; draws the ellipse.

;; ## Epilogue: Dandelin's spheres (1822)
;;
;; Apollonius reaches III.52 after a long chain of propositions about
;; tangents. In 1822 Germinal Pierre Dandelin, at Brussels, found a proof that
;; fits in one picture. Put two spheres inside the cone, one on each side of
;; the cutting plane, each touching the cone along a circle and the plane at a
;; single point. **Those points are the foci.**
;;
;; Take any point $Q$ of the section. $QF_1$ and $QG_1$ are tangents from $Q$
;; to the first sphere, with $G_1$ on its circle of contact, and two tangents
;; from a point to a sphere are equal. The same holds for the second sphere.
;; So $QF_1 + QF_2 = QG_1 + QG_2 = G_1G_2$, the stretch of the generator
;; between the two circles, and that stretch is the same for every $Q$.

(conics/dandelin)

(badges :apollonius/dandelin)

;; Emmy proves it in space: both spheres touch the plane, both tangent
;; pairs are equal, and the sum is the generator's segment.

;; ## All three at once
;;
;; Apollonius describes each section from its vertex $P$. Five centuries
;; later Pappus (*Collection* VII) gave one more description, from a focus
;; $F$ and a line, the **directrix**: the distance to $F$ stands in a fixed
;; ratio $e$ to the distance from the directrix. About the focus, with $\ell$
;; the semi-parameter, that is one curve,
;;
;; $$r = \frac{\ell}{1 + e\cos t},$$
;;
;; and it is Apollonius' symptoma with $p = 2\ell$ and $c = e^2 - 1$. The
;; sign of $c$ is the sign of $e - 1$: $e < 1$ ellipse, $e = 1$ parabola,
;; $e > 1$ hyperbola, whose far branch is the opposite section of I.14.
;;
;; Drag $\ell$ and $e$. The solid curve is the focal form. The dashed curve is
;; drawn separately from $y^2 = p x + c x^2$ about $P$, and it lies on the
;; solid one at every setting.

(playground/playground)

;; The curves waited eighteen centuries for physics.
;;
;; - **Galileo** (*Two New Sciences*, 1638) proved that a thrown body falls
;;   along Apollonius' parabola: the horizontal distance grows as the time and
;;   the fall as its square, so the square on one is applied exactly to the
;;   other, $y^2 = p\,x$.
;; - **Kepler** (*Astronomia Nova*, 1609) found that Mars moves on an
;;   Apollonian ellipse with the Sun at a focus, one of the "points arising
;;   out of the application." The [Kepler notebook](../kepler-laws/) follows
;;   the orbit; its $r = \ell / (1 + e\cos t)$ is the curve above.
;; - **Newton** (*Principia*, 1687, Book I, Proposition 11) proved the
;;   converse: a body that moves on an ellipse about a focus is pulled toward
;;   that focus by a force inversely as the square of the distance. His proof
;;   uses Apollonius' focal properties and the parameter $p$ directly.
;;
;; Backwards, the same picture closes the loop with Menaechmus. His two mean
;; proportionals are the meeting point of a parabola $y^2 = a\,x$ and a
;; second conic, the curves of the [doubling of the cube](../doubling-the-cube/).
;; Apollonius' achievement was to see them, and the ellipse, as three cuts of
;; one cone.
;;
;; Descartes (1637) turned the symptoma into the equation of a curve. The
;; next notebooks in the series take up that thread.

^{::clerk/no-cache true}
(page/prev-next "notebooks/apollonius_conics.clj")
