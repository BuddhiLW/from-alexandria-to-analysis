;; # Riemann and Weierstrass: the integral, uniform convergence, and the monster
;;
;; *From Alexandria to Analysis. Era: Rigour, part 1.*
;;
;; In 1854 Bernhard Riemann asked what an integral is, and answered with a
;; definition that still stands. In 1872 Karl Weierstrass showed the Berlin
;; Academy a continuous curve that has no tangent anywhere. Between them,
;; and in Weierstrass' lectures, analysis learned to say exactly what
;; "infinitely small" had meant since Newton and Leibniz: a statement about
;; every $\varepsilon > 0$.
;;
;; German passages are quoted verbatim from the collected works. The English
;; beside them is this series' own rendering.
^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns riemann-weierstrass
  {:history/year 1854
   :history/title "Riemann and Weierstrass: the integral, uniform convergence, and the monster"
   :history/era "Rigour"
   :nextjournal.clerk/toc true
   :nextjournal.clerk/visibility {:code :hide :result :show}}
  (:require [alexandria.riemann.integral :as integral]
            [alexandria.riemann.integral-view :as integral-view]
            [alexandria.weierstrass.monster :as monster]
            [alexandria.weierstrass.monster-view :as monster-view]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [history-of-math.page :as page]
            [nextjournal.clerk :as clerk]))

^{::clerk/visibility {:result :hide}}
(defn badges
  "One pill per graded check: proved (Emmy or exact rationals), numeric
   (measured), or FAILS."
  [rows]
  (clerk/html
   (into [:div {:style {:display "flex" :flex-direction "column" :gap "0.3em" :margin "0.6em 0"}}]
         (for [{:keys [label grade]} rows
               :let [tone (case grade :grade/proved "#2f8a3e" :grade/numeric "#2b6cb0" "#c0392b")
                     word (case grade :grade/proved "proved" :grade/numeric "numeric" "FAILS")]]
           [:div {:style {:font-family "ui-monospace, monospace" :font-size "0.82em"}}
            [:span {:style {:display "inline-block" :min-width "5.5em" :padding "0.05em 0.6em"
                            :margin-right "0.6em" :border-radius "1em" :text-align "center"
                            :border (str "1px solid " tone) :color tone}} word]
            label]))))

^{::clerk/visibility {:result :hide}}
(defn quote-pair
  "A German passage and the series' English rendering of it."
  [de en source]
  (clerk/html
   [:div {:style {:display "grid" :grid-template-columns "repeat(auto-fit, minmax(260px, 1fr))"
                  :gap "1em" :margin "0.8em 0"}}
    [:blockquote {:style {:margin 0 :font-style "italic"}} de]
    [:blockquote {:style {:margin 0}} en
     [:div {:style {:font-size "0.8em" :opacity 0.7 :margin-top "0.4em"}}
      (str "— " source "; English: the series' own rendering")]]]))

^{::clerk/visibility {:result :hide}}
(defn shelf [id] (some #(when (= id (:id %)) %)
                       (:propositions (clojure.edn/read-string
                                       (slurp (clojure.java.io/resource
                                               (case (namespace id)
                                                 "riemann" "alexandria/riemann/trigonometrische_reihe.edn"
                                                 "weierstrass" "alexandria/weierstrass/monster.edn")))))))

^{::clerk/visibility {:result :hide}}
(defn grades-matching [rows re] (filter #(re-find re (:label %)) rows))

;; ## Göttingen, 1854
;;
;; Riemann was 27. To teach at a German university he needed a
;; *Habilitation*: a written thesis and a trial lecture. For the lecture he
;; offered the faculty three topics. Gauss, then 77, passed over the two
;; Riemann had prepared and chose the third, the foundations of geometry.
;; That lecture, *Über die Hypothesen, welche der Geometrie zu Grunde
;; liegen* (10 June 1854), is a story for a later notebook.
;;
;; The written thesis was on Fourier series: *Über die Darstellbarkeit einer
;; Function durch eine trigonometrische Reihe*, on when a function can be
;; written as $a_0/2 + \sum (a_n \cos nx + b_n \sin nx)$. Riemann did not
;; publish it. After his death in 1866 Richard Dedekind printed it (1868),
;; "in completely unchanged form".
;;
;; Fourier (1807–1822) had claimed that every function has such a series,
;; and computed its coefficients as integrals. Cauchy (1823) had defined the
;; integral of a *continuous* function as a limit of sums. Dirichlet (1829)
;; proved convergence for well-behaved functions and named a function that
;; no integral of his day could handle: 1 on the rationals, 0 on the
;; irrationals. Riemann needed integrals of functions far wilder than
;; Cauchy's, so he began by fixing what an integral is.

(quote-pair "Es war nöthig, ihr einen kurzen Aufsatz über den Begriff eines bestimmten Integrales und den Umfang seiner Gültigkeit voraufzuschicken."
            "It was necessary to put in front of it a short essay on the concept of a definite integral and the extent of its validity."
            "Riemann, Habilitationsschrift, introduction")

;; ## Section 4: what is the integral?
;;
;; Notation, as Riemann writes it. Cut the interval $[a,b]$ at points
;; $a < x_1 < \dots < x_{n-1} < b$. The lengths of the pieces are
;; $\delta_1 = x_1 - a$, $\delta_2 = x_2 - x_1$, …, $\delta_n = b - x_{n-1}$.
;; An $\varepsilon_i$ is a "proper fraction", a number between 0 and 1. It
;; picks one point in the $i$-th piece, $x_{i-1} + \varepsilon_i\delta_i$;
;; today this point is called the *tag*.

(let [{:keys [statement-de statement source]} (shelf :riemann/integral-4)]
  (quote-pair statement-de statement source))

;; In one formula:
;;
;; $$S = \delta_1 f(a + \varepsilon_1\delta_1) + \delta_2 f(x_1 + \varepsilon_2\delta_2) + \dots + \delta_n f(x_{n-1} + \varepsilon_n\delta_n).$$
;;
;; The integral is the number $A$ that $S$ approaches *however* the cuts
;; and the tags are chosen, as all the $\delta$ shrink. The word "however"
;; is the new idea. Cauchy took the left end of each piece; Riemann lets
;; the tag go anywhere and asks that it not matter.
;;
;; The player below uses $f(x) = x^2$ on $[0,1]$. Each rectangle has width
;; $\delta_i$ and the height of $f$ at its tag (the dot). The *upper sum*
;; $U$ takes the greatest value of $f$ in every piece, the *lower sum* $L$
;; the least. Every $S$ on the same cuts lies between them.

(integral-view/integral-player)

;; ## Section 5: which functions have an integral?
;;
;; The *oscillation* $D_i$ of $f$ on the $i$-th piece is its greatest value
;; there minus its least. Then $U - L = \delta_1 D_1 + \dots + \delta_n D_n$.
;; $S$ converges exactly when this sum goes to zero. Riemann turns that into
;; a test you can check: fix any $\sigma > 0$, and look at the pieces where
;; the oscillation exceeds $\sigma$. If their total length $s$ can be made as
;; small as you like, the function is integrable. The proof is one line,
;; $\sigma s \le \sum \delta_i D_i \le \Delta$, so $s \le \Delta/\sigma$; and
;; the converse is as short.

(let [{:keys [statement-de statement source]} (shelf :riemann/integral-5)]
  (quote-pair statement-de statement source))

(badges (concat (grades-matching (integral/graded) #"x\^2|increasing")
                (grades-matching (integral/graded) #"to \d+ intervals")))

;; Dirichlet's function fails the test: every piece, however small, has
;; oscillation 1. Riemann's integral cannot integrate it. Lebesgue's (1902)
;; can; see the end of this notebook.
;;
;; ## Section 6: a function with jumps in every interval
;;
;; To show how far the definition reaches, Riemann builds a function no one
;; had considered. Write $(x)$ for the signed distance from $x$ to the
;; nearest whole number: $(1.3) = 0.3$, $(1.7) = -0.3$, and
;; $(\tfrac12) = 0$, the mean of $\tfrac12$ and $-\tfrac12$.

(let [{:keys [statement-de statement source]} (shelf :riemann/integral-6)]
  (quote-pair statement-de statement source))

;; At $x = p/2n$ (with $p$ odd and prime to $n$) Riemann computes the two
;; one-sided limits,
;;
;; $$f(x+0) = f(x) - \frac{1}{2n^2}\Big(1 + \frac19 + \frac1{25} + \cdots\Big) = f(x) - \frac{\pi^2}{16n^2}, \qquad f(x-0) = f(x) + \frac{\pi^2}{16 n^2}.$$
;;
;; So the jump there is $\pi^2/8n^2$. Jumps sit at every fraction with an
;; even denominator, in every interval; but only finitely many exceed any
;; given size. That is exactly Section 5's test, and the function is
;; integrable. In the player, the graph is a raster kernel of Riemann's
;; partial sums; the last step draws the exact upper and lower sums of
;; $S_8$, computed in rational arithmetic.

(integral-view/pathological-player)

(badges (concat (grades-matching (integral/graded) #"pi\^2|\(x\)|S_8 is|S_8 at|S_12|jumps of")
                (integral/numeric-checks)))

;; ## Uniform convergence repairs Cauchy
;;
;; Cauchy's *Cours d'analyse* (1821) stated that a convergent series of
;; continuous functions has a continuous sum; see the
;; [Cauchy notebook](../cauchy/). In 1826 Niels Henrik Abel wrote in a
;; footnote in Crelle's Journal that the theorem "suffers exceptions", and
;; gave one: $\sin x - \tfrac12\sin 2x + \tfrac13\sin 3x - \cdots$ converges
;; everywhere, to $x/2$ on $(-\pi,\pi)$ and to $0$ at $\pi$.
;;
;; The missing idea is *uniform* convergence. Weierstrass wrote
;; "gleichmässig convergirt" already in 1841, as a schoolteacher in
;; Münster, in a paper not printed until 1894. From 1856 he taught in
;; Berlin, and his lecture courses, copied by students across Europe, made
;; the definition and the $\varepsilon$-$\delta$ style of argument standard.
;; His printed definition (1880):

(let [{:keys [statement-de statement source]} (shelf :weierstrass/uniform)]
  (quote-pair statement-de statement source))

;; The last sentence is the *M-test*: bound each term by a number $g_\nu$
;; that does not depend on $x$; if $\sum g_\nu$ is finite, the series
;; converges uniformly. The player draws a tube of half-width $\varepsilon$
;; around the limit. Uniform convergence means: from some $N$ on, *every*
;; partial sum lies inside the tube along its *whole* length. Abel's partial
;; sums always overshoot near $\pi$; the geometric series settles inside.

(monster-view/uniform-player)

(badges (grades-matching (monster/graded) #"M-test|geometric series|Abel"))

;; With uniform convergence, Cauchy's proof works: choose $N$ once for all
;; $x$, then $|f(x)-f(x_0)|$ splits into three pieces each below
;; $\varepsilon/3$.
;;
;; ## Berlin, 18 July 1872: the function without a derivative
;;
;; Weierstrass opened his address with what everyone believed.

(let [{:keys [statement-de statement source]} (shelf :weierstrass/belief)]
  (quote-pair statement-de statement source))

(let [{:keys [riemann-de riemann]} (monster-view/part :weierstrass/monster)]
  (quote-pair riemann-de riemann "Weierstrass, Werke II, p. 71"))

;; Riemann's example was too hard to prove, so Weierstrass built his own.
;; A warning on letters: Weierstrass writes $a$ for the odd integer and $b$
;; for the number below 1. Most modern books swap them.

(let [{:keys [statement-de statement source]} (shelf :weierstrass/nowhere-differentiable)]
  (quote-pair statement-de statement source))

;; The notebook takes $a = 13$, $b = \tfrac12$, so $ab = 6.5 > 1 +
;; 3\pi/2 \approx 5.71$. Each term is bounded by $b^n$, so by the M-test the
;; series converges uniformly and $f$ is continuous. The zoom shrinks the
;; window by a factor $a$ per stage and stretches the height by
;; $w^{\ln 2/\ln 13}$; a smooth curve would flatten into its tangent, this
;; one never does. Then come Weierstrass' points $x' < x_0 < x''$ and his
;; difference quotients, computed with exact rational angles, against his
;; bound $(ab)^m\big(\tfrac23 - \tfrac{\pi}{ab-1}\big)$.

(monster-view/monster-player)

(badges (grades-matching (monster/graded) #"cos A|first m terms|ab|x'|a odd|estimate"))

^{::clerk/visibility {:result :show}}
(clerk/table {:head ["m" "alpha_m" "left quotient" "right quotient" "Weierstrass' bound"]
              :rows (for [{:keys [m alpha left right bound]} (monster-view/rows)]
                      [m alpha (format "%.1f" left) (format "%.1f" right) (format "%.1f" bound)])})

;; ## Bolzano, earlier
;;
;; Bernard Bolzano, in Prague around 1830, had already constructed a
;; continuous function that is nowhere differentiable, by repeatedly
;; replacing line segments with zigzags, in the manuscript of his
;; *Functionenlehre*. He never published it. Martin Rychlík proved its
;; nowhere-differentiability in 1922, and the manuscript was printed in
;; 1930. Weierstrass did not know it.
;;
;; ## Reception: "monsters"
;;
;; Many mathematicians did not welcome these functions. Charles Hermite
;; wrote to Thomas Stieltjes on 20 May 1893:
;;
;; > *Je me détourne avec effroi et horreur de cette plaie lamentable des
;; > fonctions continues qui n'ont point de dérivées.*
;;
;; "I turn away with fright and horror from this lamentable plague of
;; continuous functions that have no derivatives." Such functions came to be
;; called *monsters*. Twentieth-century analysis showed they are the rule,
;; not the exception: in a precise sense, most continuous functions are
;; nowhere differentiable.
;;
;; ## Where this goes
;;
;; Before: Euler's "continuous" function was one given by a single formula
;; (see the Euler notebook); Cauchy (1821) defined continuity and the
;; integral of a continuous function by limits. Riemann's definition reaches
;; much further, and Weierstrass' series show that a single formula can be
;; continuous everywhere and smooth nowhere.
;;
;; After: both arguments lean on facts about the real numbers that no one
;; had yet proved, such as the existence of the bound a bounded set
;; approaches. Dedekind (1872) and Cantor (1872) constructed the reals to
;; supply them; see [Dedekind and Cantor](../dedekind-cantor/). Cantor came
;; to sets through Riemann's subject: the points where a trigonometric
;; series may fail. And Henri Lebesgue (1902) replaced Riemann's question
;; "how small are the pieces of the $x$-axis?" with "how large is the set
;; where $f$ takes given values?", which integrates Dirichlet's function and
;; lets limits pass under the integral sign.

^{::clerk/no-cache true}
(page/prev-next "notebooks/riemann_weierstrass.clj")
