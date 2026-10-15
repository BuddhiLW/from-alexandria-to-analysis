;; # Euler: bridges, infinite polynomials, and the faces of solids
;;
;; Leonhard Euler was born in Basel in 1707. Johann Bernoulli, the best
;; analyst in Europe and Leibniz's ally against the Newtonians, taught him
;; on Saturday afternoons. In 1727 Euler left for the new Academy of
;; Sciences in St Petersburg, founded on Leibniz's advice to Peter the Great.
;; In 1741 Frederick the Great called him to the Berlin Academy, where he
;; stayed twenty-five years; in 1766 he went back to St Petersburg and died
;; there in 1783.
;;
;; He lost the sight of his right eye in 1738 and was almost totally blind
;; from 1771. His output did not fall: he dictated to his sons and
;; assistants, and about half of everything he published dates from the
;; blind years. The *Opera Omnia*, begun in 1911, runs to more than eighty
;; volumes. Enestroem's catalogue numbers his papers E1 to E866, and those
;; E-numbers are how we cite him here.
;;
;; Euler wrote in Latin. This page quotes his Latin verbatim from the first
;; printings and gives our own English beside it, marked as ours. Four
;; results, in the order he found them:
;;
;; 1. 1734/35, *De summis serierum reciprocarum* (E41): the sum of the
;;    reciprocals of the squares.
;; 2. 1736, *Solutio problematis ad geometriam situs pertinentis* (E53): the
;;    bridges of Koenigsberg.
;; 3. 1748, *Introductio in analysin infinitorum* (E101): $e^{ix} = \cos x
;;    + i \sin x$.
;; 4. 1750, *Elementa doctrinae solidorum* (E230, printed 1758): $V - E + F = 2$.
;;
;; Press ▶ to play a proof, or click a step to see its figure. The figures,
;; the data and the checks come from Alexandria's Euler shelf
;; (`alexandria.euler.series`, `alexandria.euler.graphs`,
;; `alexandria.euler.polyhedra`). Each badge names how the check was decided:
;; *proved* means Emmy simplified a difference to exactly 0; *numeric* means
;; a floating-point value was inside a stated bound.
^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns euler
  {:history/year 1748
   :history/title "Euler: the Basel problem, Koenigsberg, e^{ix}, and V - E + F = 2"
   :history/era "Enlightenment"
   :history/fields #{:analysis :trigonometry :topology :graph-theory}
   :nextjournal.clerk/visibility {:code :hide :result :show}
   :nextjournal.clerk/toc true}
  (:require [alexandria.euler.graphs :as graphs]
            [alexandria.euler.opera-view :as view]
            [alexandria.euler.polyhedra :as polyhedra]
            [alexandria.euler.series :as series]
            [alexandria.euler.widgets :as widgets]
            [clojure.string :as str]
            [history-of-math.page :as page]
            [history-of-math.lens :as lens]
            [history-of-math.widgets :as hom]
            [nextjournal.clerk :as clerk]
            [alexandria.euler.playgrounds :as playgrounds]))

^{::clerk/visibility {:code :hide :result :hide}}
(hom/install!)

^{::clerk/visibility {:result :hide}}
(defn latin
  "Euler's Latin passages of proposition id, then ours in English."
  [id]
  (let [{:keys [latin english]} (view/passages id)]
    (clerk/md
     (str (str/join "\n>\n" (map (fn [s l] (str "> *" s "* (" l ")")) (:passages latin) (:locus latin)))
          "\n\n— Euler, " (:source latin)
          "\n\n**In English (this series' rendering):**\n\n"
          (str/join "\n>\n" (map #(str "> " %) english))))))

;; ## The Basel problem (E41, 1734/35)
;;
;; In 1650 Pietro Mengoli asked for the exact value of
;;
;; $$1 + \frac14 + \frac19 + \frac1{16} + \cdots = \sum_{n=1}^{\infty} \frac{1}{n^2}.$$
;;
;; Leibniz had summed $\sum 1/(n(n+1)) = 1$ and the alternating series
;; $1 - \frac13 + \frac15 - \cdots = \frac{\pi}{4}$; the Bernoulli brothers
;; in Basel tried this one and failed, so it carries their city's name.
;; Euler computed the sum to twenty decimals, $1.6449340668\ldots$, and in
;; 1734 found what it is: $\pi^2/6$.
;;
;; ### The pattern: Viète's roots and coefficients
;;
;; Take a polynomial whose constant term is 1 and whose roots are
;; $r_1, \ldots, r_N$. It factors as
;;
;; $$P(x) = \left(1 - \frac{x}{r_1}\right)\left(1 - \frac{x}{r_2}\right)\cdots\left(1 - \frac{x}{r_N}\right).$$
;;
;; Multiply out and read off the coefficient of $x$: it is
;; $-\left(\frac1{r_1} + \cdots + \frac1{r_N}\right)$. The coefficient of
;; $x^2$ is the sum of the products $\frac1{r_i r_j}$ over pairs $i < j$.
;; These relations between roots and coefficients go back to Viète
;; (1591) and Newton's *Arithmetica universalis*. Euler states the first one
;; in §6 of E41: *ex natura autem et resolutione aequationum constat*, "from
;; the nature and resolution of equations it is known".
;;
;; ### Euler's bold step
;;
;; Now take $\frac{\sin x}{x} = 1 - \frac{x^2}{3!} + \frac{x^4}{5!} - \cdots$.
;; Its constant term is 1 and it vanishes at $x = \pm\pi, \pm 2\pi,
;; \pm 3\pi, \ldots$. Euler treats it as a polynomial of infinite degree and
;; factors it the same way, pairing $+n\pi$ with $-n\pi$:
;;
;; $$\frac{\sin x}{x} = \left(1 - \frac{x^2}{\pi^2}\right)\left(1 - \frac{x^2}{4\pi^2}\right)\left(1 - \frac{x^2}{9\pi^2}\right)\cdots$$
;;
;; Match the coefficients of $x^2$: on the left $-\frac16$, on the right
;; $-\left(\frac1{\pi^2} + \frac1{4\pi^2} + \frac1{9\pi^2} + \cdots\right)$. So
;;
;; $$\sum_{n=1}^{\infty}\frac{1}{n^2} = \frac{\pi^2}{6}.$$

(latin :euler/basel)

(view/basel)

;; Now try it yourself: drag $N$ along the terms and $x$ along
;; $\frac{\sin x}{x}$, change the exponent $s$, and watch the sums, the
;; product over the roots and the integral test answer.

(playgrounds/basel-playground)

;; Emmy checks the finite pattern: for the product of $N$ factors
;; $(1 - a_n u)$ with symbolic $a_n$, it takes the derivatives at $u = 0$
;; and simplifies the difference with the symmetric sums to 0, for each $N$
;; from 1 to 6. That is a proof for each truncation, not a proof about the
;; infinite product: Euler's step from finite to infinite degree waited for
;; Weierstrass's factorization theorem (1876). The partial sums are exact
;; rationals; what is still missing after $N$ terms lies below
;; $\int_N^\infty dx/x^2 = 1/N$, and the badge checks it against that bound.

(page/grade-badges (series/graded-basel) #{:grade/proved :grade/numeric})

;; ### The partial sums, live
;;
;; How close is $1 + \frac14 + \cdots + \frac1{n^2}$ to $\frac{\pi^2}{6}$
;; after $n$ terms? What is left is caught between two areas under
;; $1/x^2$: the area beyond $n + 1$ and the area beyond $n$, that is
;; between $\frac1{n+1}$ and $b = \frac1n$. The number of terms $n$ and the
;; bound $b$ are tied controls: move either and the other follows. The
;; blue staircase is the partial sums, the red line $\pi^2/6$, and the
;; violet gap what is still missing; the numbers beside it come from the
;; same raster kernel.

(widgets/basel)

(widgets/derivation :basel)

;; ### The same move, one coefficient on: $\sum 1/n^4$
;;
;; Nothing new is needed for the fourth powers. Write $a_n = 1/(n^2\pi^2)$.
;; The $x^4$ coefficient gives the sum over pairs, $e_2 = \sum_{i<j} a_i a_j
;; = \frac1{120}$. Squaring a sum gives every square once and every pair
;; twice, so
;;
;; $$\sum a_n^2 = e_1^2 - 2e_2 = \frac1{36} - \frac2{120} = \frac1{90},
;; \qquad \sum_{n=1}^\infty \frac1{n^4} = \frac{\pi^4}{90}.$$
;;
;; Euler writes this rule as $Q = P\alpha - 2\beta$ in §8 and goes on to the
;; sixth, eighth, … powers with the next coefficients. Every even power
;; comes out as a rational multiple of a power of $\pi$. The odd powers
;; resist: whether $\sum 1/n^3$ has a closed form is still open. Riemann,
;; at the end of this series, makes these sums one function, $\zeta(s)$.
;;
;; Euler also checked his method on a known case. With $\sin s = 1$ instead
;; of $\sin s = 0$ the same move gives Leibniz's series
;; $1 - \frac13 + \frac15 - \cdots = \frac{\pi}{4}$, which he calls a
;; *firmamentum*, a strong support, of the method (§10).
;;
;; ### Euler's argument, paragraph by paragraph
;;
;; The order in E41 is not the order above. Euler starts from the sine
;; (§3–7): the equation $0 = 1 - \frac{s}{y} + \frac{s^3}{1\cdot2\cdot3\,y}
;; - \cdots$ has as roots every arc $s$ whose sine is $y$. In §8 he states
;; a rule for the sums of powers of the roots: if their sum is $\alpha$,
;; the sum of their products in pairs $\beta$, in threes $\gamma$, …, then
;; $P = \alpha$, $Q = P\alpha - 2\beta$, $R = Q\alpha - P\beta + 3\gamma$,
;; and so on. In §10–12 he takes $y = 1$ and gets $P, Q, R, S, T = 1, 1,
;; \frac12, \frac13, \frac5{24}$: Leibniz's series, then
;; $1 + \frac19 + \frac1{25} + \cdots = \frac{\pi^2}{8}$, and from it
;; $\frac{\pi^2}{6}$. Only in §16–17 does he set $y = 0$ and divide by $s$:
;; that is the product for $\frac{\sin s}{s}$. §18 lists the sums of
;; $1/n^2$ up to $1/n^{12}$. The badges check §8 on symbolic roots, his
;; values for §10–12 and §18 as exact fractions, and his partial sums,
;; each computed by a raster loop kernel.

(page/grade-badges (series/graded-e41) #{:grade/proved :grade/numeric})

;; ## The bridges of Koenigsberg (E53, 1736)
;;
;; In Koenigsberg, Prussia, the river Pregel splits around the island of
;; the Kneiphof. Seven bridges joined the four parts of the city. Could
;; anyone walk across every bridge exactly once? Carl Ehler, mayor of
;; Danzig, put the question to Euler in 1736. Euler saw that it needed no
;; measurement at all, only *which* region each bridge joins. He called it a
;; problem of the *geometria situs*, the geometry of position that Leibniz
;; had asked for; it is the first theorem of graph theory, and of topology.

(latin :euler/koenigsberg)

;; Euler's figure has four regions, A to D, and seven bridges, a to g. We
;; draw it as he reduced it: shrink each region to a point and each bridge
;; to a line. A point is a *vertex*; a line is an *edge*; the number of
;; edges at a vertex is its *degree*. Two bridges join A and B, and two join
;; A and C, so the figure is a *multigraph*: more than one edge may join the
;; same two vertices.

(view/koenigsberg)

;; Now try it yourself: drag the regions, build or pull down bridges
;; between any two of them, and watch Euler's count of letters and his
;; rule decide whether the walk exists; when it does, it is drawn.

(playgrounds/koenigsberg-playground)

;; ### Euler's argument, paragraph by paragraph
;;
;; The player follows Euler's 21 numbered paragraphs (§) in order. In
;; short:
;;
;; - §4–5: a walk is recorded by the letters of the regions it passes
;;   through, so a walk over $n$ bridges has $n + 1$ letters.
;; - §6–7: the record never says which bridge was used, only which regions
;;   follow each other. AB must stand twice (bridges a and b), AC twice,
;;   AD, BD and CD once each.
;; - §8–9: a region with an odd number $d$ of bridges is written
;;   $(d+1)/2$ times, wherever the walk begins. At Koenigsberg that is
;;   $3 + 2 + 2 + 2 = 9$ letters for a walk that can only have 8.
;; - §11–13: with an even number $d$, the region is written $d/2$ times,
;;   or $d/2 + 1$ times if the walk starts there.
;; - §14: Euler's table: the regions, their bridges (even counts starred),
;;   and the letters each needs.
;; - §15: a second map, fifteen bridges among six regions A–F. The letters
;;   add up to $16 = 15 + 1$, and Euler prints a route:
;;   *EaFbBcFdAeFfCgAhCiDkAmEnApBoElD*.
;; - §16–17: counting bridges region by region counts every bridge twice,
;;   so the odd regions come in pairs.
;; - §18–19: if every count is even, a walk exists from anywhere (the seven
;;   bridges can each be crossed twice). With two odd counts it starts in
;;   one of them; with 4, 6, 8, … the letters exceed the walk by 1, 2, 3, ….
;; - §20: the rule. §21: how to find the walk; Euler says it is easy.
;;
;; The badges check each of these paragraphs on Euler's own data: his
;; route of §15 is the witness for the letter counts of §5, §8 and §11–12,
;; counted from the letters he printed. The last badge is the long way he
;; set aside in §3: every walk on the seven bridges tried, none crossing
;; more than six.
;;
;; ### Reading the original
;;
;; The Latin above is quoted from the proofread text on la.wikisource
;; (Euler's spelling kept: *vt*, *diuidi*); the English is the
;; public-domain translation on en.wikisource, *The Seven Bridges of
;; Königsberg*, section by section. The first printing (*Commentarii* 8,
;; 1741) has one plate with three figures. Figure 1 is the map of the city,
;; river and seven bridges that the player opens on. Figure 2 is a single
;; region A with its bridges (§8). Figure 3 is the map of §15, two islands,
;; four rivers and fifteen bridges, drawn here as a graph with Euler's
;; route walked across it.

(page/badge (= :none (graphs/euler-rule graphs/koenigsberg))
            (str "odd regions at Koenigsberg: " (str/join ", " (map name (graphs/odd-regions graphs/koenigsberg)))
                 ": no walk crosses every bridge once"))

(page/grade-badges (graphs/graded))

;; Euler did not show how to *find* the walk when the count allows one; he
;; says it is easy (§21). Carl Hierholzer proved that the condition is
;; sufficient in 1873.

;; ## *Introductio in analysin infinitorum* (E101, 1748)
;;
;; The *Introductio* is the book that made the *function* the subject of
;; analysis. Euler wrote it in Berlin; it was printed in Lausanne in 1748.
;; Newton and Leibniz had studied curves; Euler studies $e^x$, $\log x$,
;; $\sin x$ as functions given by infinite series and products, and he fixes
;; the notation we still use: $e$ for the base of natural logarithms, $\pi$,
;; $\sin$, $\cos$, $f(x)$. Chapter VIII, on the quantities that arise from
;; the circle, contains this:

(latin :euler/exponential)

;; ### Where $e$ comes from: chapter VII (§114–122)
;;
;; Chapter VII, *De quantitatibus exponentialibus ac logarithmis per series
;; explicatis*, builds the exponential series from nothing but an
;; infinitely small number. Euler begins (§114):
;;
;; > *Quia est $a^0 = 1$, atque crescente exponente ipsius $a$ simul valor
;; > potestatis augetur, si quidem $a$ est numerus unitate maior; sequitur
;; > si exponens infinite parum cyphram excedat, potestatem ipsam quoque
;; > infinite parum unitatem esse superaturam.*
;;
;; "Since $a^0 = 1$, and the power grows as the exponent grows when $a$ is
;; greater than one, it follows that if the exponent exceeds zero by an
;; infinitely small amount, the power too will exceed one by an infinitely
;; small amount" (our English). He writes $a^\omega = 1 + k\omega$, raises
;; both sides to the infinitely large power $j = z/\omega$, expands by the
;; binomial theorem, and gets $a^z = 1 + \frac{kz}{1} + \frac{k^2z^2}{1\cdot2}
;; + \cdots$. In §122 he picks the base with $k = 1$, sums
;; $1 + \frac11 + \frac1{1\cdot2} + \frac1{1\cdot2\cdot3} + \cdots$ to
;; twenty-three places, $2{,}71828182845904523536028$, and names it:
;; *ponamus autem brevitatis gratia pro numero hoc … constanter litteram
;; $e$*, "for brevity let us always write the letter $e$ for this number".
;; Step through the chapter; from §114 on, $k$ and the base $a$ are tied
;; controls ($k = \log a$, $a = e^k$, both solved by Emmy below).

(widgets/base-steps)

(widgets/base-derivation)

;; The same limit read the other way: $\left(1 + \frac1n\right)^n$ climbs
;; to $e$ as $n$ grows. How fast? Emmy expands $n\log(1 + \frac1n)$ in
;; $u = \frac1n$ and composes it with the exponential: the error is
;; $\varepsilon = e\left(\frac u2 - \frac{11u^2}{24} + \cdots\right)$, about
;; $e/2n$. The three quantities $n$, Euler's $\omega = 1/n$ and the error
;; $\varepsilon$ are tied controls: move any one and the other two are solved,
;; $\varepsilon \to n$ by the leading term, then Newton's steps with Emmy's
;; derivative.

(widgets/e-limit)

(widgets/derivation :e)

;; Here is the argument as series. Euler had shown in chapter VII that
;;
;; $$e^z = 1 + z + \frac{z^2}{2!} + \frac{z^3}{3!} + \cdots.$$
;;
;; Put $z = ix$, where $i = \sqrt{-1}$, so $i^2 = -1$. The powers of $i$
;; cycle: $1, i, -1, -i, 1, \ldots$. So the terms $\frac{(ix)^k}{k!}$ point
;; east, north, west, south in turn. Lay them end to end, each starting where
;; the last ended, and you get a spiral of segments. The east-west terms add
;; up to $1 - \frac{x^2}{2!} + \frac{x^4}{4!} - \cdots = \cos x$, the
;; north-south terms to $x - \frac{x^3}{3!} + \cdots = \sin x$: the spiral
;; ends on the unit circle at angle $x$. At $x = \pi$ it ends at $-1$:
;;
;; $$e^{i\pi} + 1 = 0.$$
;;
;; Euler himself writes the general formula; the special case at $\pi$ is
;; read off it later. Move the slider to change $x$.

(view/exponential)

;; Emmy proves the identity on truncations: for every order $N$ from 1 to
;; 16, $\sum_{k\le N}\frac{(ix)^k}{k!}$ minus the cosine and sine series
;; truncated at $N$ simplifies to exactly 0, with $i$ an exact complex
;; number. That is a statement about every truncation, the finite pattern
;; of the proof; the limit itself is the convergence Cauchy made rigorous in
;; 1821.

(page/grade-badges (series/graded-euler-formula))

;; ### $e^{ix}$ on the circle, from every side
;;
;; The truncations prove the pattern; Emmy also proves the identity whole,
;; in one line. Put $f(x) = e^{-ix}(\cos x + i\sin x)$. Its derivative
;; simplifies to 0, so $f$ is constant, and $f(0) = 1$. Below, $x$,
;; $\cos x$ and $\sin x$ are each a control: move the angle and the point
;; runs round the circle; move $\cos x$ (or $\sin x$) and the angle is
;; solved back by $\operatorname{atan2}$, the point staying on its own half
;; of the circle. The coloured segments are Euler's terms
;; $1, ix, \frac{(ix)^2}{2!}, \ldots$ laid end to end.

(widgets/cis)

(widgets/derivation :cis)

;; ## Solids: $V - E + F = 2$ (E230, E231; 1750, printed 1758)
;;
;; In a letter to Christian Goldbach of November 1750, Euler wrote that he
;; had found a property of all solids bounded by planes that he was amazed
;; nobody had noticed. Euclid's Book XIII and Kepler's *Harmonices mundi*
;; had studied the regular solids by their faces and angles; Euler counts
;; their parts. In his words, a solid has *anguli solidi* (solid angles,
;; our vertices, $S$), *acies* (edges, $A$) and *hedrae* (faces, $H$).

(latin :euler/polyhedra)

;; Euler names the parts in E230 §6, and in his Latin the edges get a name
;; for the first time:
;;
;; > *hos terminos, quoniam apud scriptores Stereometriae nullum nomen
;; > proprium reperio, acies vocabo* (E230 §6, *Novi Commentarii* 4)
;;
;; "since I find no proper name for these boundaries among the writers on
;; solid geometry, I shall call them *acies*" (our English). Step through
;; §6–7 on the cube, one kind of boundary at a time; drag to turn it.

(widgets/terms-steps)

;; Euler admits in E230 that he has no firm proof and offers induction over
;; the kinds of solid. In E231 (1751) he gives one by cutting off solid
;; angles one at a time; it has a gap, since cutting can leave something
;; that is not a solid. The proof animated here is Augustin-Louis Cauchy's
;; of 1813, a forward link in this series:
;;
;; 1. Look at the cube from just above the middle of one face, close
;;    enough that every other face is seen through it. The near face becomes
;;    the outer square and the far face a small square inside: the cube's
;;    *Schlegel diagram* (named after Victor Schlegel, 1883). Nothing is
;;    cut, so $V$, $E$, $F$ are unchanged.
;; 2. Remove the outer face: the flat net has $V - E + F = 1$.
;; 3. Draw diagonals until every face is a triangle. Each diagonal adds one
;;    edge and one face.
;; 4. Peel off boundary triangles one at a time. Each removal takes away one
;;    edge and one face, or two edges, one face and a vertex. When one
;;    triangle is left, $3 - 3 + 1 = 1$, and every step kept the sum.

(view/polyhedra)

;; Now try it yourself: on the map of the solid, drag the vertices, rub
;; out edges or draw Cauchy's diagonals, take the outer face away, and
;; watch $V - E + F$; pull a vertex across an edge or cut the map in two
;; and see which hypothesis the count needs.

(playgrounds/vef-playground)

;; The badges count $V$, $E$ and $F$ from the coordinates of each solid
;; alone: a face is a maximal set of vertices on a plane that leaves all
;; the others on one side, an edge two vertices shared by two faces. Then
;; $V - E + F - 2$ is an exact integer, and it is 0 for the five regular
;; solids and for every prism and pyramid on a 3- to 8-gon. The last badges
;; follow Cauchy's reduction on the cube.

(clerk/table
 {:head ["solid" "V" "E" "F" "V - E + F"]
  :rows (for [s polyhedra/solids :let [{:keys [V E F]} (polyhedra/counts s)]]
          [(:name s) V E F (+ (- V E) F)])})

(page/grade-badges (polyhedra/graded))

;; ### Cutting off solid angles: E231
;;
;; Euler's own proof in E231 runs the other way from Cauchy's: it takes
;; solid angles *away*. Cut a corner of degree $d$ off with a plane close
;; to it. The corner's vertex goes; $d$ new vertices appear, one on each
;; edge that met there; the $d$ edges are shortened, not lost, and $d$ new
;; edges bound the new face. So $S$ grows by $d - 1$, $A$ by $d$, $H$ by
;; 1, and $S - A + H$ does not move. Pick a solid below, cut off as many
;; solid angles as you like, and move the depth of the cut. The counts are
;; read off the cut solid as it stands; the drawing is one raster kernel
;; per solid. (Euler cuts until a pyramid is left; the gap Cauchy's
;; contemporaries found is that a deep cut can reach past a neighbouring
;; vertex, which is why the depth stops short of the midpoints here.)

(widgets/polyhedron)

;; Emmy proves the bookkeeping for any degrees $d_1, \ldots, d_k$:

(widgets/derivation :cut)

;; ## Where this goes
;;
;; Euler worked in the world Newton and Leibniz made. He used Leibniz's $d$
;; and $\int$, built on Johann Bernoulli's teaching, and turned the calculus
;; of curves into an analysis of functions. Joseph-Louis Lagrange, a
;; nineteen-year-old in Turin, wrote to him in 1755 with a method for
;; problems of maxima and minima; Euler recognised it as better than his
;; own, named it the *calculus of variations*, and in 1766 handed Lagrange
;; his chair in Berlin. Lagrange is the next era of this series.
;;
;; Euler's methods were bold: infinite polynomials factored like finite
;; ones, divergent series summed. They gave right answers and left the
;; question of why. Cauchy answered it with limits in his *Cours d'analyse*
;; (1821) and, in 1813, gave the proof of $V - E + F = 2$ shown above;
;; Weierstrass justified the product for $\sin x$; Riemann turned the sums of
;; E41 into $\zeta(s)$. The bridges became graph theory and, through
;; Poincaré's Euler characteristic, topology.

^{::clerk/no-cache true}
(page/prev-next "notebooks/euler.clj")

(lens/section "euler")
