;; # Dedekind, Cantor, Lebesgue: the line made of numbers
;;
;; *Rigour, part 2: from the cut of 1872 to the integral of 1902.*
;;
;; The calculus of Newton and Leibniz worked for two hundred years before
;; anyone could say what a real number is. Cauchy (1821) defined limits and
;; continuity with inequalities. Riemann (1854) and Weierstrass (in his
;; Berlin lectures of the 1860s) made the integral and the limit exact. But
;; every one of those arguments assumed a line with no holes in it, and
;; nobody had built that line out of anything simpler.
;;
;; This notebook follows three steps, in the order they were taken. In
;; 1872 Richard Dedekind built the real numbers from the rationals. In 1874
;; and 1891 Georg Cantor showed that infinite sets come in different sizes,
;; and that the line is bigger than any list. In 1902 Henri Lebesgue used
;; Cantor's lists to measure sets, and with measure he built a new integral.
;;
;; **Notation.** A *rational number* is a fraction $p/q$ of whole numbers,
;; $q \neq 0$. $\mathbb{Q}$ is the set of all of them. $\sqrt 2$ is the
;; positive number whose square is $2$. A *set* is a collection of things,
;; its *members*; $x \in A$ says that $x$ is a member of $A$.
;;
;; **Sources.** Dedekind is quoted from W. W. Beman's authorized English
;; translation, *Essays on the Theory of Numbers* (Open Court, 1901;
;; Project Gutenberg #21016). Cantor and Lebesgue have no public-domain
;; English translation. Their German and French are quoted verbatim from
;; the journals as printed (archive.org), each followed by **this series'
;; own English rendering**, marked as such.
;;
;; The badges show grades from `alexandria.grade`. *Proved* means that
;; Emmy simplified the difference of the two sides to exactly 0, or that
;; a finite check was decided in exact rational arithmetic.
^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns dedekind-cantor
  {:nextjournal.clerk/visibility {:code :hide :result :show}
   :nextjournal.clerk/toc true
   :history/year 1872
   :history/title "Dedekind, Cantor, Lebesgue: the line made of numbers"
   :history/era "Rigour"}
  (:require [alexandria.cantor.sets :as sets]
            [alexandria.cantor.sets-view :as sets-view]
            [alexandria.dedekind.cuts :as cuts]
            [alexandria.dedekind.cuts-view :as cuts-view]
            [alexandria.lebesgue.measure :as measure]
            [alexandria.lebesgue.measure-view :as measure-view]
            [clojure.string :as str]
            [history-of-math.page :as page]
            [nextjournal.clerk :as clerk]))

^{::clerk/visibility {:result :hide}}
(defn badges
  "One badge per graded check."
  [graded]
  (clerk/html
   (into [:div]
         (for [{:keys [label grade]} graded
               :let [ok? (#{:grade/proved :grade/numeric} grade)
                     tone (if ok? "#2f8a3e" "#c0392b")]]
           [:div {:style {:margin "0.2em 0"}}
            [:span {:style {:display "inline-block" :padding "0.1em 0.7em" :border-radius "1em"
                            :font-family "ui-monospace, monospace" :font-size "0.82em"
                            :border (str "1px solid " tone) :color tone}}
             (str (name grade) ": " label)]]))))

^{::clerk/visibility {:result :hide}}
(defn quote-block
  "Block quotation of passages, with a source line."
  [passages source]
  (clerk/md (str (str/join "\n>\n" (map #(str "> " %) passages)) "\n\n— " source)))

^{::clerk/visibility {:result :hide}}
(defn original-and-rendering
  "Original-language passages, each followed by the series' rendering."
  [passages source]
  (clerk/md
   (str (str/join "\n\n"
                  (for [{:keys [original rendering]} passages]
                    (str "> *" original "*\n>\n> " rendering " *(series' rendering)*")))
        "\n\n— " source)))

;; ## Dedekind in Zürich and Brunswick, 1858–1872
;;
;; Richard Dedekind (1831–1916) studied at Göttingen with Gauss. He later
;; heard Dirichlet there and became Riemann's friend. In 1858 he went to the
;; new Polytechnic in Zürich to teach the calculus. In 1862 he moved back to
;; Brunswick (Braunschweig), his home town, and taught at its Technical
;; High School for the rest of his life. He published his answer only in
;; 1872, in a short pamphlet, *Stetigkeit und irrationale Zahlen*
;; (*Continuity and Irrational Numbers*). His preface says why he wrote it:

(let [{:keys [passages source]} (cuts-view/passage :dedekind/preface)]
  (quote-block passages (str "Dedekind, tr. Beman (" source ")")))

;; The theorem he could not prove without "geometric evidences" is this:
;; a quantity that keeps growing but stays below a bound approaches a
;; limit. Cauchy, Riemann and Weierstrass all used it. Dedekind traced it
;; back to one property of the line, which he called *continuity*:
;;
;; > *If all points of the straight line fall into two classes such that
;; > every point of the first class lies to the left of every point of the
;; > second class, then there exists one and only one point which produces
;; > this division of all points into two classes, this severing of the
;; > straight line into two portions.*
;;
;; — Dedekind, tr. Beman, Section III
;;
;; The rationals do not have this property. Dedekind's argument shows why,
;; and then repairs it.

;; ### The cut of √2
;;
;; Dedekind's word is *Schnitt*, a **cut**. A cut $(A_1, A_2)$ splits all
;; the rationals into two classes $A_1$ and $A_2$, so that every member of
;; $A_1$ is less than every member of $A_2$. A rational $a$ makes a cut:
;; put everything below $a$ in $A_1$ and everything above it in $A_2$.
;; The question is whether every cut is made this way.
;;
;; For $\sqrt 2$ take
;;
;; $$A_1 = \{\, r \in \mathbb{Q} : r \le 0 \text{ or } r^2 < 2 \,\},\qquad
;;   A_2 = \{\, r \in \mathbb{Q} : r > 0 \text{ and } r^2 > 2 \,\}.$$
;;
;; Dedekind proves three things. No rational has square $2$, so every
;; rational is in one class or the other. $A_1$ has no greatest member and
;; $A_2$ has no least one. So no rational makes this cut: the rationals
;; have a **gap** there. He then *creates* a new number for the gap, and
;; calls it irrational.
;;
;; In the player below, the rationals with denominator up to 24 are drawn
;; on the line. The blue ones are in $A_1$, the yellow ones in $A_2$. Watch
;; the line magnify around $\sqrt 2$: the rationals stay dense, but none
;; ever lands on the mark.

(cuts-view/cut-sqrt-2)

;; Dedekind's proof that $A_1$ has no greatest member uses one formula.
;; For a positive rational $x$ and $D = 2$, put
;;
;; $$y = \frac{x(x^2 + 3D)}{3x^2 + D}.$$
;;
;; Then $y - x = \dfrac{2x(D - x^2)}{3x^2 + D}$ and
;; $y^2 - D = \dfrac{(x^2 - D)^3}{(3x^2 + D)^2}$. If $x^2 < D$, these say
;; that $y$ is larger than $x$ and still has $y^2 < D$. If $x^2 > D$, $y$ is
;; smaller than $x$ and still has $y^2 > D$. Starting at $1$, the map gives
;; $7/5$, then $1393/985$, closer and closer to $\sqrt 2$ from below.
;;
;; His proof that no rational squares to $D$ is a descent. From a pair
;; $(t, u)$ with $t^2 = Du^2$ it builds a pair with a smaller $u$ and the
;; same property, which positive whole numbers cannot do for ever. Emmy
;; proves all three of his identities:

(badges (cuts/graded))

;; ### The pattern: Eudoxus, 2200 years earlier
;;
;; Dedekind did not claim the idea was new. In 1887 he wrote that fixing an
;; irrational by all the rationals below it and above it was "already set
;; forth in the clearest possible way" by Euclid:

(let [{:keys [euclid euclid-source passage source]} (cuts-view/passage :dedekind/eudoxus)]
  (clerk/md (str "> *" euclid "*\n\n— " euclid-source
                 "\n\n> *" passage "*\n\n— Dedekind, tr. Beman (" source ")")))

;; Book V of the *Elements* is Eudoxus' theory, from about 370 BC, and it
;; is **the same test**. Eudoxus compares the diagonal $d$ of a square with
;; its side $s$ by asking, for each pair of whole numbers $m, n$, whether
;; $n$ diagonals exceed $m$ sides. Because $d^2 = 2s^2$,
;;
;; $$n\,d > m\,s \iff 2n^2 > m^2 \iff \frac{m}{n} \in A_1 .$$
;;
;; So Definition 5's answers, "exceed" or "fall short", sort every rational
;; $m/n$ into Dedekind's two classes. "Equal" never happens. For Eudoxus
;; two ratios are the same when they sort the rationals the same way, that
;; is, when they make the same cut. The badge *Elements V Def. 5 sorts every
;; m/n ... exactly as the cut of sqrt 2* above checks this for every
;; $m, n \le 60$.
;;
;; Dedekind added the last step. Eudoxus compared ratios of magnitudes,
;; line to line and area to area. Dedekind took the sorting of the
;; rationals itself as the number, with no geometry left in it. The
;; Eudoxus branch of the set-theory course, *Elements V: ratios without
;; numbers*, works the same test on the same table.

;; ### No gaps left
;;
;; Now repeat the construction one level up. Take all the real numbers,
;; the old rationals and the new cut-numbers, and split *them* into two
;; classes, every member of the first below every member of the second.
;; Dedekind's Section V, theorem iv, says that one real number makes this
;; split. The new line has no gaps: the cuts are **complete**. The proof
;; takes the cut of the rationals inside the given split; the number
;; $\alpha$ of that cut is the one. The last badge above shows a small
;; case. The cuts made by $1, 7/5, 1393/985, \ldots$ together have, as the
;; union of their lower classes, exactly the lower class of $\sqrt 2$.
;;
;; This completeness is what the calculus needed. In modern words, every
;; set of reals with an upper bound has a least upper bound.

;; ## Cantor at Halle, 1872–1891
;;
;; Georg Cantor (1845–1918) studied in Berlin under Weierstrass and
;; Kronecker. From 1869 to the end of his career he taught at Halle, a
;; smaller university. In March 1872, as Dedekind was writing his preface,
;; he received Cantor's paper on trigonometric series, which built the
;; reals from sequences of rationals; the preface thanks "the ingenious
;; author". The two met in Switzerland that year and began writing to each
;; other in 1873. In a letter of 29 November 1873 Cantor asked whether the
;; positive whole numbers can be matched one-to-one with the real numbers.
;; Dedekind could not say. He did show Cantor that the algebraic numbers
;; can be matched with them. Within weeks Cantor proved that the reals
;; cannot. He published both results in Crelle's *Journal*, volume 77, in
;; 1874.
;;
;; **Countable.** A set is *countable* when its members can be listed as a
;; first, a second, a third, and so on, so that each member has a definite
;; place in the list. Cantor calls such a list a *Reihe*, a sequence.

;; ### The rationals, counted
;;
;; The first surprise is that the rationals are countable, although between
;; any two of them lie infinitely many more. Write every fraction $p/q$ in
;; a table, row $q$, column $p$. Walk it one diagonal at a time,
;; $p + q = 2, 3, 4, \ldots$. Each diagonal is finite, so every cell is
;; reached after finitely many steps. Skip the cells not in lowest terms,
;; whose value was already counted.

(sets-view/zigzag)

;; ### The algebraic numbers, counted by height
;;
;; A real number is *algebraic* when it is a root of an equation with
;; whole-number coefficients, as $\sqrt 2$ is a root of $\omega^2 - 2 = 0$.
;; Cantor's idea is to give each algebraic number a whole number, its
;; **height**:

(let [{:keys [passages source]} (sets-view/entry :cantor/heights)]
  (original-and-rendering passages (str "Cantor, Crelle 77 (" source ")")))

;; Cantor writes $[a]$ for the absolute value $|a|$. Only finitely many
;; equations have a given height, so only finitely many numbers do. List
;; height 1, then height 2 in increasing order, then height 3, and so on,
;; and every algebraic number gets a place. The heights below are computed
;; exactly. The equations are normalized as Cantor requires. Irreducibility
;; is checked by the rational-root test, and for degree 4 by a search for
;; quadratic factors. The real roots are counted by Sturm sequences in
;; exact rationals.

(sets-view/heights)

(clerk/md
 (str "| height $N$ | real algebraic numbers | $\\varphi(N)$ |\n|---:|:---|---:|\n"
      (str/join "\n"
                (for [row (sets-view/heights-data)]
                  (str "| " (:N (first row)) " | " (str/join ", " (map :label row)) " | " (count row) " |")))))

;; Cantor gives $\varphi(1) = 1$, $\varphi(2) = 2$, $\varphi(3) = 4$. The
;; computation agrees, and the next value is $\varphi(4) = 12$. Height 4 is
;; where $\pm\sqrt 2$, $\pm 1/\sqrt 2$ and the golden ratio first appear.
;;
;; The same paper's §2 proves the other half. Given any list of reals and
;; any interval, take the first two members of the list inside the interval
;; as a new, smaller interval, and repeat. The nested intervals close on a
;; number that is not in the list. So no list holds every real number. The
;; algebraic numbers can be listed, so most real numbers are not algebraic:
;; they are *transcendental*. Liouville had found some in 1844. Cantor's
;; argument shows they are in every interval.

(badges (sets/graded))

;; ### The diagonal, 1891
;;
;; Seventeen years later, at the first meeting of the new German
;; Mathematical Society (DMV), Cantor, its first president, gave a shorter
;; proof. It uses no irrational numbers and no intervals, only two
;; symbols, $m$ and $w$:

(let [{:keys [passages source]} (sets-view/entry :cantor/diagonal)]
  (original-and-rendering passages (str "Cantor, Jahresbericht der DMV 1 (" source ")")))

;; The table below holds the first eight rationals of $[0, 1)$ from the
;; zig-zag, each written as a row of binary digits ($m = 0$, $w = 1$). Read
;; the diagonal and flip every digit. The new row differs from row $\mu$ at
;; place $\mu$, for every $\mu$.

(sets-view/diagonal)

;; In the same paper the same flip proves that every set has more subsets
;; than members, so Cantor's sizes of infinity have no largest one. The
;; set-theory course, after Pinter's *A Book of Set Theory*, continues this
;; thread in its chapters on cardinal numbers. There the diagonal becomes
;; Cantor's theorem, $|A| < |\mathcal P(A)|$.

;; ### Kronecker's opposition
;;
;; In Berlin not everyone accepted this. Leopold Kronecker (1823–1891),
;; Cantor's teacher, held that mathematics should be built from the whole
;; numbers by finite steps. Infinite sets, cuts and completed lists were
;; not mathematics to him. Heinrich Weber's obituary of Kronecker, printed
;; by the DMV in 1893, records his best-known line, spoken at the Berlin
;; meeting of naturalists in 1886:
;;
;; > *Die ganzen Zahlen hat der liebe Gott gemacht, alles andere ist
;; > Menschenwerk.*
;; >
;; > The whole numbers were made by the dear God; everything else is the
;; > work of man. *(series' rendering)*
;;
;; — H. Weber, "Leopold Kronecker", *Jahresbericht der DMV* 2 (1891–92),
;; p. 19
;;
;; Cantor held Kronecker responsible for delaying his 1878 paper at
;; Crelle's *Journal* and for keeping him out of Berlin. Dedekind answered
;; in 1887: every theorem of analysis can be stated about whole numbers,
;; but he saw "nothing meritorious" in that circumlocution. The reply that
;; stuck came from Hilbert, in "Über das Unendliche" (*Mathematische
;; Annalen* 95, 1926):
;;
;; > *Aus dem Paradies, das Cantor uns geschaffen, soll uns niemand
;; > vertreiben können.*
;; >
;; > No one shall drive us out of the paradise that Cantor has created for
;; > us. *(series' rendering)*

;; ## Lebesgue in Paris, 1902
;;
;; Riemann's integral (1854) slices the region under a graph into thin
;; vertical strips. Darboux (1875) gave each strip an upper and a lower
;; height. The function is integrable when the two sums meet as the strips
;; get thinner. By 1900 many functions failed this test, among them
;; derivatives of other functions. Henri Lebesgue (1875–1941), a student
;; of Émile Borel at the École Normale, wrote his thesis to fix it. It
;; appeared in the *Annali di Matematica* in 1902:

(let [{:keys [passages source]} (measure-view/entry :lebesgue/introduction)]
  (original-and-rendering passages (str "Lebesgue, *Intégrale, longueur, aire* (" source ")")))

;; ### Measure, and the rationals of measure zero
;;
;; Lebesgue first gives sets of points a size, their **measure**. It is the
;; length of an interval, and extends to much stranger sets.

(let [{:keys [passages source]} (measure-view/entry :lebesgue/measure-zero)]
  (original-and-rendering passages (str "Lebesgue 1902 (" source ")")))

;; The *outer measure* of a set $E$ is the smallest total length of
;; countably many intervals that cover $E$. Here Cantor's count pays off.
;; List the rationals of $[0, 1]$ as $r_1, r_2, r_3, \ldots$ and put $r_n$
;; inside an interval of length $\varepsilon/2^n$. The lengths add up to
;;
;; $$\frac{\varepsilon}{2} + \frac{\varepsilon}{4} + \frac{\varepsilon}{8} + \cdots
;;   = \frac{\varepsilon/2}{1 - 1/2} = \varepsilon .$$
;;
;; Every $\varepsilon > 0$ will do, so the rationals have measure $0$: a
;; dense set of points that takes up no length at all.

(measure-view/measure-zero)

;; ### Dirichlet's function
;;
;; Dirichlet, in 1829, gave a function that no integral of his day could
;; handle:
;;
;; $$\chi(x) = \begin{cases} 1 & x \text{ rational} \\ 0 & x \text{ irrational.} \end{cases}$$
;;
;; Riemann's vertical strips fail on it. Every strip contains a rational,
;; so its upper height is $1$ and the upper sum is $S = 1$. Every strip also
;; contains an irrational, so its lower height is $0$ and the lower sum is
;; $s = 0$. The two sums never meet, however thin the strips.
;;
;; Lebesgue slices the other way, horizontally, by the *values* of the
;; function. $\chi$ takes only two values: $1$ on the rationals, of measure
;; $0$, and $0$ on the irrationals, of measure $1$. His sum is
;;
;; $$1 \cdot 0 + 0 \cdot 1 = 0 .$$
;;
;; Lebesgue explained the difference with money. *(Paraphrase, marked as
;; such: the series found no public-domain text of his 1926 lecture "Sur le
;; développement de la notion d'intégrale".)* Riemann counts the coins in
;; the order they come out of the purse. Lebesgue first sorts the coins by
;; value, then multiplies each value by the number of coins that have it.
;; For a purse the totals agree. For $\chi$ only the second way gives an
;; answer.

(measure-view/dirichlet)

(badges (measure/graded))

;; ## Where this goes
;;
;; Within thirty years these three steps became the ground of analysis.
;; Textbooks still build the real line from Dedekind's cuts or Cantor's
;; sequences. Cantor's sets became the language of all of mathematics:
;; Russell's paradox (1901) showed that they needed axioms, and Zermelo
;; gave them in 1908. Lebesgue's measure became the integral of Fourier
;; analysis and of probability, which Kolmogorov built on measure in 1933.
;; The notebooks before this one follow Cauchy's limits and the integrals
;; and functions of Riemann and Weierstrass. The ones after it follow
;; analysis into the twentieth century.

^{::clerk/no-cache true}
(page/prev-next "notebooks/dedekind_cantor.clj")
