;; # Cauchy, *Cours d'analyse*: the arithmetic of the infinitely small
;;
;; In 1821 a professor at the École polytechnique in Paris published the
;; algebraic part of his lectures, the *Cours d'analyse de l'École royale
;; polytechnique*. Augustin-Louis Cauchy was thirty-one. In 1823 he followed
;; it with a *Résumé* of his lectures on the infinitesimal calculus. In these
;; two books the calculus of Newton and Leibniz gets definitions: of a limit,
;; of a continuous function, of the sum of a series, of an integral. Each
;; definition says what to *compute* to check it, not what to imagine.
;;
;; The quotations below are Cauchy's French, verbatim from the first
;; editions (archive.org scans). The English beside each one is this
;; series' own rendering, marked *our translation*. Every check is computed
;; by Alexandria (`alexandria.cauchy.analysis`): the algebra by Emmy, a
;; computer algebra system, and every number by raster, which compiles
;; Emmy's expressions into native kernels. A badge reading *proved* means
;; Emmy reduced the claim to exactly 0 for all values of its letters, or an
;; exact computation in fractions decided it; *numeric* means a raster
;; computation agreed to the stated tolerance, used where no closed form
;; exists. The sections follow Cauchy's own order: the *Cours* (limit,
;; continuity, the root between two signs, series, the sum theorem), then
;; the *Résumé* (derivative, integral, fundamental theorem).
^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns cauchy
  {:history/year 1821
   :history/title "Cauchy, Cours d'analyse"
   :history/era "Rigour"
   :nextjournal.clerk/visibility {:code :hide :result :show}
   :nextjournal.clerk/toc true}
  (:require [alexandria.cauchy.analysis :as cauchy]
            [alexandria.cauchy.analysis-view :as view]
            [clojure.string :as str]
            [history-of-math.page :as page]
            [nextjournal.clerk :as clerk]))

^{::clerk/visibility {:result :hide}}
(defn quotes
  "The French passages of part id, each with our English and the page."
  [id]
  (clerk/md
   (str/join "\n\n"
             (for [{:keys [fr en source]} (:quotes (view/part id))]
               (str "> *" fr "*\n>\n> " en " *(our translation)*\n>\n> — " source)))))

^{::clerk/visibility {:result :hide}}
(def graded (cauchy/graded))

^{::clerk/visibility {:result :hide}}
(defn badges
  "One line per graded check: its grade, who computed it (Emmy for a
   symbolic or exact check, raster for a number) and its label."
  [part]
  (clerk/html
   (into [:div {:style {:display "flex" :flex-direction "column" :gap "0.3em"}}]
         (for [{:keys [label grade source]} (get graded part)
               :let [ok? (#{:grade/proved :grade/closed-form :grade/numeric} grade)
                     tone (if ok? "#2f8a3e" "#c0392b")]]
           [:div {:style {:display "inline-block" :padding "0.15em 0.8em" :border-radius "1em"
                          :font-family "ui-monospace, monospace" :font-size "0.85em"
                          :border (str "1px solid " tone) :color tone}}
            (str (name grade) (if (= source :raster) " by raster: " " by Emmy: ") label)]))))

;; ## Paris, 1821
;;
;; The École polytechnique was founded in 1794, during the Revolution, to
;; train engineers for the state. Lagrange, Monge and Fourier taught there.
;; Cauchy entered it as a student in 1805, became an engineer of bridges and
;; roads, and worked on the harbour at Cherbourg for Napoleon's fleet.
;;
;; Politics shaped his career. In 1815 the Bourbon king Louis XVIII came
;; back to the throne. In 1816 the restored monarchy purged the Académie
;; des sciences: Monge and Carnot, men of the Revolution and the Empire,
;; were expelled, and Cauchy, a devout Catholic and a royalist, was
;; appointed in their place. The same year he became professor of analysis
;; at the Polytechnique. His students found the lectures hard, and the
;; school's council complained that he spent the hours on a rigour the
;; engineers did not need. He answered with the *Cours*.
;;
;; The problem he faced was old. Newton reasoned with "first and last
;; ratios" of quantities that vanish (*Principia*, Book I, Lemma I, 1687);
;; Leibniz with infinitely small differentials. Both worked, and nobody
;; could say what they were. Berkeley mocked them in 1734 as "ghosts of
;; departed quantities". Lagrange, in his *Théorie des fonctions
;; analytiques* (1797), tried to do without the infinitely small: a
;; function is its power series, and the derivative is read off the
;; coefficients. Cauchy rejected that too, as the last section shows. His
;; way is to keep the infinitely small but define it: a *variable* that
;; becomes smaller than any given number.
;;
;; He was not alone. In Prague in 1817 the priest Bernard Bolzano published
;; a proof of the intermediate value theorem with the same definition of
;; continuity and the same criterion for convergence. It appeared as a
;; pamphlet in Prague and was not read in Paris.

;; ## The limit, and continuity as a game

(quotes :cauchy/continuity)

;; Read the first sentence as a game against a doubter. The doubter names a
;; distance, as small as he likes. The variable must end by staying within
;; that distance of its limit. An *infinitely small* is a variable whose
;; limit is 0: it ends below every number named. This is Newton's "ultimate
;; ratio" with the motion taken out. No quantity is ever infinitely small;
;; a variable *becomes* smaller than anything given.
;;
;; The third sentence defines continuity. Modern notation names the two
;; distances: $\varepsilon$ (epsilon) for the doubter's tolerance on the
;; value of the function, and $\delta$ (delta) for the answer, a tolerance
;; on the variable. $f$ is continuous at $a$ when for every
;; $\varepsilon > 0$ there is a $\delta > 0$ with
;;
;; $$|x - a| < \delta \implies |f(x) - f(a)| < \varepsilon.$$
;;
;; Cauchy does not write the $\delta$; Weierstrass's Berlin lectures of the
;; 1860s put it in. But the game is Cauchy's sentence: "an infinitely small
;; increment of the variable produces an infinitely small increment of the
;; function". Here it is played on $f(x) = x^2$ at $a = 1$. The yellow band
;; is $\varepsilon$ and the blue window is $\delta$. Inside the window the
;; graph must stay in the band.

(view/continuity)

;; For $x^2$ the largest window is $\delta = \sqrt{a^2 + \varepsilon} - a$:
;; on the right of $a$ the graph meets the edge of the band exactly there.
;; For $1/x$ the left side is the worse one, and
;; $\delta = \varepsilon a^2 / (1 + \varepsilon a)$. Emmy proves these for
;; every $a$ and $\varepsilon$. For $\sin x$ there is no such formula, so
;; bisection finds the largest working $\delta$.

(badges :continuity)

;; ## A root between two signs: chapter II and Note III
;;
;; Right after continuity, Cauchy states its first consequence (chapter
;; II, Theorem 4): a continuous function that passes from $f(x_0)$ to
;; $f(X)$ takes every value $b$ in between.

(quotes :cauchy/ivt)

;; In chapter II his proof is a picture: the curve $y = f(x)$ joins two
;; points on either side of the line $y = b$, so it must cross it. He adds
;; that Note III gives a "purely analytic" proof, one that also computes
;; the root. That proof is a search. Take $b = 0$, put $h = X - x_0$ and
;; choose a whole number $m > 1$. Divide $h$ into $m$ parts, evaluate $f$
;; at every division point, and keep the first two consecutive values of
;; contrary signs, $f(x_1)$ and $f(X')$. Then $X' - x_1 = h/m$. Repeat
;; inside $[x_1, X']$ to get $x_2, X''$ with $X'' - x_2 = h/m^2$, and so on.
;; The $x_n$ increase, the $X^{(n)}$ decrease, and the gap $h/m^n$ goes to
;; 0, so both have one limit $a$. Values of $f$ of both signs approach
;; $f(a)$, so $f(a) = 0$.
;;
;; With $m = 10$ each round fixes one more decimal. The player runs his
;; search on $x^3 - 2x - 5 = 0$ between 2 and 3, the equation Newton used
;; to show his method of approximation. Each round's eleven values come
;; from a compiled raster kernel of the polynomial; the bounds are exact
;; fractions. The window is stretched each round to the same width, so you
;; watch the tenths of a tenth.

(view/ivt)

(badges :ivt)

;; *Reading the original.* The proof is in Note III, *Sur la résolution
;; numérique des équations*, pages 460-462 of the 1821 edition. It has no
;; figure: Cauchy writes the two series $x_0, x_1, x_2, \ldots$ and
;; $X, X', X'', \ldots$ as lines of letters, and his Scholie 1 bounds the
;; error of the half-sum by $h/(2m^n)$. The moving figure draws those two
;; lines as nested brackets.

;; ## Series: when does an infinite sum have a sum?
;;
;; Eighteenth-century analysts summed series freely; Euler wrote
;; $1 - 1 + 1 - 1 + \cdots = \tfrac12$. In chapter VI of the *Cours* Cauchy
;; refuses this. A series has a sum only if its partial sums
;; $s_n = u_0 + u_1 + \cdots + u_{n-1}$ have a limit. Otherwise it "n'aura
;; plus de somme": it has no sum.

(quotes :cauchy/series)

;; The second passage is *Cauchy's criterion*: the series converges exactly
;; when the sums $u_n + u_{n+1} + \cdots + u_{n+p}$, whatever $p$, end by
;; being smaller than any number named. It tests convergence without
;; knowing the limit. Terms going to 0 are necessary but not enough. The
;; harmonic series $1 + \tfrac12 + \tfrac13 + \cdots$, which Cauchy treats
;; next, has terms going to 0 and no sum. The root test (his Theorem 1) and
;; the ratio test (Theorem 2) compare a series with a geometric progression.
;;
;; The player shows the partial sums of $\sum 1/n!$ climbing to
;; $e = 2.71828\ldots$, and the *tail band*: from a stage $n$ on, every
;; later sum stays in a band that narrows as $n$ grows.

(view/series)

(badges :series)

;; ### The root test, as Cauchy proves it: chapter VI, Theorem 1

(quotes :cauchy/root-test)

;; Let $k$ be the greatest limit of $(u_n)^{1/n}$. If $k < 1$, Cauchy puts
;; a number $U$ between $k$ and 1. The greatest values of $(u_n)^{1/n}$
;; cannot approach $k$ indefinitely without ending below $U$, so from some
;; $n$ on $(u_n)^{1/n} < U$, that is $u_n < U^n$. The terms of the series
;; end below those of the progression $1, U, U^2, \ldots$, which converges
;; because $U < 1$; so the series converges *à fortiori*. If $k > 1$ the
;; same argument with $1 < U < k$ gives infinitely many terms above $U^n$,
;; which grow without bound. Theorem 2, the ratio test, follows because
;; when $u_{n+1}/u_n$ has a limit, that limit is $k$.
;;
;; The player follows the proof on $u_n = n^2/2^n$: the roots
;; $(u_n)^{1/n} = n^{2/n}/2$ (a raster kernel) fall to $k = 1/2$; take
;; $U = 3/4$; the exact fractions show $u_n < U^n$ from $n = 13$ on.

(view/root-test)

(badges :root-test)

;; *Reading the original.* Chapter VI, section 2, pages 132-135 of the
;; 1821 *Cours*. He gives no figure and no numerical example for Theorem
;; 1; after Theorem 2 he applies the ratio test to
;; $1 + \frac{1}{1} + \frac{1}{1 \cdot 2} + \cdots$, the series of $e$ in
;; the player above.

;; ## The sum theorem, and Abel's exception
;;
;; On page 131 Cauchy proves that a convergent series of continuous
;; functions has a continuous sum. The argument: $s = s_n + r_n$, where the
;; finite sum $s_n$ is continuous and the remainder $r_n$ is small "if one
;; gives $n$ a very considerable value". Abel's answer comes after it.

(quotes :cauchy/sum-theorem)

;; The theorem as stated is false. Niels Henrik Abel, a Norwegian of
;; twenty-three, said so in 1826, in a footnote to his paper on the binomial
;; series in the first volume of Crelle's *Journal für die reine und
;; angewandte Mathematik*. His example is a Fourier series,
;;
;; $$\sin x - \tfrac12 \sin 2x + \tfrac13 \sin 3x - \cdots,$$
;;
;; whose sum is $x/2$ for $-\pi < x < \pi$. Every term is continuous, and
;; the series converges at every $x$. At $x = \pi$ every term is 0, so the
;; sum is 0, while just to the left it is close to $\pi/2$. The sum jumps.
;;
;; Watch the partial sums $S_n$ approach the sawtooth; the moving curve is
;; a compiled raster kernel. Near the jump they overshoot: the peak tends
;; to $\int_0^\pi \frac{\sin t}{t}\,dt = 1.8519\ldots$, not to
;; $\pi/2 = 1.5708$. Henry Wilbraham described this overshoot in 1848; it
;; is named after Josiah Willard Gibbs, who met it again in 1899.

(view/sum-theorem)

(badges :sum-theorem)

;; What is wrong with the proof? It picks one $n$ that makes the remainder
;; small at $x$, and uses it for the points near $x$ too. In Abel's series
;; no single $n$ serves: at $x = \pi - 1/n$ the error $|S_n(x) - x/2|$
;; stays near $0.62$ for every $n$. The missing idea is *uniform
;; convergence*: one $n$ for all $x$ at once. Philipp Ludwig von Seidel and
;; George Stokes isolated it in 1847, and Weierstrass made it standard. With
;; uniform convergence the theorem is true; Cauchy restated it that way in
;; 1853. The story continues in the notebook on Riemann and Weierstrass.
;;
;; The slider under the player sets $n$ for its last stage. Play that stage
;; and drag it from 10 to 48 terms: the red error bar at $x = \pi - 1/n$
;; keeps its length, about 0.6 every time.

;; ## The derivative, *Résumé* 1823, Lesson 3
;;
;; Two years after the *Cours*, the lectures on the calculus proper open
;; with the derivative, built from the limit and from continuity.

(quotes :cauchy/derivative)

;; Cauchy calls the increment of the variable $i$: $\Delta x = i$. Because
;; $f$ is continuous, both $\Delta y = f(x + i) - f(x)$ and $\Delta x = i$
;; are infinitely small, and their *rapport aux différences* may still
;; converge to a limit. For $f(x) = x^m$ he writes the ratio out,
;;
;; $$\frac{(x + i)^m - x^m}{i} = m x^{m-1} + \frac{m(m-1)}{1 \cdot 2} x^{m-2} i + \cdots + i^{m-1},$$
;;
;; and every term after the first carries $i$, so the limit is $m x^{m-1}$.
;; For $\sin x$ he writes the ratio as
;; $\frac{\sin \frac{i}{2}}{\frac{i}{2}} \cos\left(x + \frac{i}{2}\right)$,
;; whose limit is $\cos x$. The limit is a new function of $x$, the
;; *fonction dérivée* $f'(x)$. No infinitely small number appears: only
;; ratios computed at smaller and smaller $i$. The player shrinks $i$ for
;; $\sin x$ at $x = 1$; each value of $f$ and each ratio is a raster
;; kernel.

(view/derivative)

(badges :derivative)

;; *Reading the original.* Lesson 3, pages 9-10 of the 1823 *Résumé*.
;; Lagrange's *Théorie des fonctions analytiques* (1797) had defined
;; $f'(x)$ as the coefficient of $i$ in the expansion of $f(x + i)$; Cauchy
;; keeps Lagrange's notation $f'(x)$ and his letter $i$, and replaces the
;; expansion by a limit.

;; ## The definite integral, *Résumé* 1823
;;
;; For Leibniz the integral was a sum of infinitely many infinitely thin
;; strips. For Euler and Lagrange it was the inverse of the derivative.
;; Cauchy defines it as a limit of finite sums, for any continuous $f$, and
;; then proves that it inverts the derivative.

(quotes :cauchy/integral)

;; Divide $[x_0, X]$ by points $x_1, \ldots, x_{n-1}$ and form
;;
;; $$S = (x_1 - x_0) f(x_0) + (x_2 - x_1) f(x_1) + \cdots + (X - x_{n-1}) f(x_{n-1}).$$
;;
;; Each product is a rectangle standing on an element, as tall as $f$ at
;; its left end. Because $f$ is continuous, refining the division changes
;; $S$ less and less: $S$ has a limit, the integral
;; $\int_{x_0}^X f(x)\,dx$. Cauchy's tool is the *mean value*: a sum of
;; elements times values of $f$ equals the whole length times one value
;; $f(x_0 + \theta (X - x_0))$, with $\theta$ between 0 and 1. In Lesson 26
;; he lets the upper limit move and finds that the derivative of the
;; integral is $f$: the fundamental theorem of the calculus.

(view/integral)

;; For a polynomial the sums have closed forms. With $n$ equal elements and
;; $f(x) = x^2$,
;; $S = \frac{X^3 - x_0^3}{3} - \frac{(X - x_0)^2 (X + x_0)}{2n} + \frac{(X - x_0)^3}{6n^2}$,
;; so the error shrinks like $1/n$. Cauchy also divides geometrically,
;; $x_k = x_0 (1+\alpha)^k$ (his formula (12)), which gives
;; $\int x^a\,dx$ in one line. For $\sin$ and $\exp$ the checks are numeric.

(badges :integral)

;; ### Why the mode of division stops mattering: Lesson 21, equations (3) to (7)

(quotes :cauchy/refinement)

;; Cauchy proves the limit exists without knowing it. With one element,
;; $S = (X - x_0) f(x_0)$, equation (3). With the elements $x_1 - x_0$,
;; ..., $X - x_{n-1}$, $S$ is their sum times a mean of the values
;; $f(x_0), \ldots, f(x_{n-1})$, and a mean of values of a continuous
;; function is itself a value: $S = (X - x_0) f(x_0 + \theta (X - x_0))$,
;; $0 < \theta < 1$, equation (4). Now cut every element again. Equation
;; (4), applied element by element, turns each product
;; $(x_1 - x_0) f(x_0)$ into $(x_1 - x_0) f(x_0 + \theta_0 (x_1 - x_0))$,
;; and writing that value as $f(x_0) \pm \varepsilon_0$ gives equation (7):
;;
;; $$S' - S = \pm\varepsilon_0 (x_1 - x_0) \pm \varepsilon_1 (x_2 - x_1) \pm \cdots \pm \varepsilon_{n-1} (X - x_{n-1}).$$
;;
;; Each $\varepsilon_k$ is how much $f$ moves inside one element, small
;; when the elements are small because $f$ is continuous; so $S' - S$ is
;; $X - x_0$ times a small mean. Two different divisions are compared
;; through a third that uses all their points. The player halves the
;; elements of $1 + \sin x$ on $[0, 3]$; every sum is a raster loop
;; kernel, and the badge checks $|S' - S| \le (X - x_0)^2 / n$, the bound
;; equation (7) gives here since $|f'| \le 1$.

(view/refinement)

(badges :refinement)

;; ## The fundamental theorem: Lesson 26

(quotes :cauchy/fundamental)

;; Let the upper limit move: $\mathcal{F}(x) = \int_{x_0}^x f(x)\,dx$ is a
;; new function, equation (1). Its increment over $\alpha$ is the integral
;; from $x$ to $x + \alpha$, which by the mean value is
;; $\alpha f(x + \theta\alpha)$, equation (3). So $\mathcal{F}$ is
;; continuous, and dividing by $\alpha$ and passing to the limit gives
;; $\mathcal{F}'(x) = f(x)$, equation (4). In the player $\mathcal{F}$ is
;; raster quadrature of $1 + \sin t$, and $\theta$ for each $\alpha$ is
;; found by raster's Brent; it stays between 0 and 1 and tends to $1/2$.

(view/fundamental)

(badges :fundamental)

;; *Reading the original.* Lessons 21 and 26 are on pages 81-83 and
;; 100-101 of the 1823 *Résumé*. The book has no plates; the rectangles,
;; the strip of width $\alpha$ and the point $x + \theta\alpha$ are drawn
;; here from his equations (2), (4) and (3) of Lesson 26, in his letters.

;; ## Against Lagrange: the flat function
;;
;; In the preface of the *Résumé* Cauchy explains why he will not build the
;; calculus on Taylor's series, as "the illustrious author of the
;; *Mécanique analytique*", Lagrange, had done. At the end of Lesson 38 he
;; gives the reason in one example.

(quotes :cauchy/flat)

;; Take $f(x) = e^{-1/x^2}$ with $f(0) = 0$. Every derivative of $f$ is a
;; polynomial in $1/x$ times $e^{-1/x^2}$, and $e^{-1/x^2}$ goes to 0 faster
;; than any power of $1/x$ grows. So $f(0) = f'(0) = f''(0) = \cdots = 0$,
;; and the Taylor series of $f$ at 0 is $0 + 0x + 0x^2 + \cdots$. It
;; converges everywhere, to the wrong function. Zoom in on the origin and
;; magnify the height by a power: the graph only gets flatter.

(view/flat)

(badges :flat)

;; ## After Cauchy
;;
;; In 1830 the July Revolution drove out the Bourbons. Cauchy refused the
;; oath to the new king, Louis-Philippe, and went into exile. In Turin he
;; held a chair of higher physics, and from 1833 in Prague he tutored the
;; exiled heir, the Duke of Bordeaux. He came back to Paris in 1838, still
;; refusing the oath, and regained his teaching posts only after 1848. He
;; wrote about 800 papers, on complex functions, elasticity and groups of
;; permutations.
;;
;; His definitions left two questions open. What is the integral of a
;; function that is *not* continuous? Bernhard Riemann answered in 1854,
;; with the same sums and any choice of point in each element. And what do
;; the limits stand on, if the real numbers themselves are not defined?
;; Weierstrass, Dedekind and Cantor answered in the 1860s and 1870s. Both
;; stories follow in the next notebooks.

^{::clerk/no-cache true}
(page/prev-next "notebooks/cauchy.clj")
