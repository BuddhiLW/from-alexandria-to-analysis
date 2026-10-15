;; # Cantor, the diagonal and the power set (1891)
;;
;; In 1874 Cantor proved that the real numbers cannot be listed, by nested
;; intervals (see [the Dedekind, Cantor, Lebesgue notebook](../dedekind-cantor/)).
;; Seventeen years later, in a four-page note for the new German
;; Mathematical Society (DMV), he gave a much simpler proof, independent of
;; the irrational numbers, and in its last lines he saw that the same
;; proof shows more: for every set there is a set with more elements. This
;; notebook reads that note, page by page, and then runs its two arguments.
;;
;; The 1874 notebook counted the rationals and the algebraic numbers. What
;; 1891 adds is (1) an argument with no intervals, no order and no
;; irrational numbers, only two letters; (2) a theorem about *every* set,
;; so that there is no largest infinity. The first is the diagonal
;; argument; the second is **Cantor's theorem**, $|A| < |\mathcal P(A)|$,
;; where $\mathcal P(A)$ is the set of all subsets of $A$.
;;
;; **Sources.** Georg Cantor, *Ueber eine elementare Frage der
;; Mannigfaltigkeitslehre*, Jahresbericht der Deutschen
;; Mathematiker-Vereinigung 1 (1890/91, published 1892), pp. 75-78
;; (public domain; scan at
;; [archive.org](https://archive.org/details/jahresberichtder1218deut)).
;; The German is quoted from the scan. The scan's OCR garbles the formula
;; lines, so the matrix $a_{\mu,\nu}$ below is retyped from the page image.
;; The English is **this series' own rendering**.
;;
;; **Notation.** A *sequence* of two characters is an infinite list
;; $(x_1, x_2, \dots)$, each place holding $m$ or $w$. The code below is
;; `alexandria.structure.cardinality`: an *enumeration* (a list with a
;; place for each element), the diagonal of a list of sequences, and the
;; power set of a finite set.
^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns set-theory-cantor-1891
  {:history/year 1891
   :history/title "Cantor, the diagonal argument and the power set"
   :history/era "Rigour"
   :history/fields #{:set-theory :logic-computing}
   :nextjournal.clerk/visibility {:code :hide :result :show}
   :nextjournal.clerk/toc true}
  (:require [alexandria.cantor.diagonal-widgets :as w]
            [alexandria.cantor.sets :as sets]
            [alexandria.structure.cardinality :as card]
            [history-of-math.lens :as lens]
            [history-of-math.page :as page]
            [history-of-math.widgets :as hom]
            [nextjournal.clerk :as clerk]))

^{::clerk/visibility {:code :hide :result :hide}}
(hom/install!)

;; ## The question, and the simpler proof (p. 75)
;;
;; The note opens by recalling what the paper of 1874 had shown:

(page/blockquote
 "In dem Aufsatze, betitelt: Ueber eine Eigenschaft des Inbegriffs aller reellen algebraischen Zahlen (Journ. für Math. Bd. 77, S. 258), findet sich wohl zum ersten Male ein Beweis für den Satz, dass es unendliche Mannigfaltigkeiten giebt, die sich nicht gegenseitig eindeutig auf die Gesamtheit aller endlichen ganzen Zahlen 1, 2, 3, ..., v, ... beziehen lassen, oder, wie ich mich auszudrücken pflege, die nicht die Mächtigkeit der Zahlenreihe 1, 2, 3, ..., v, ... haben."
 "Cantor, Jahresbericht der DMV 1 (1892), p. 75. Our translation: *In the paper entitled “On a property of the collection of all real algebraic numbers” (Journal für Math. vol. 77, p. 258) there is, probably for the first time, a proof of the theorem that there are infinite manifolds that cannot be related one-to-one to the totality of all finite whole numbers 1, 2, 3, ..., v, ..., or, as I am accustomed to say, that do not have the power (Mächtigkeit) of the number sequence 1, 2, 3, ..., v, ....*")

(page/blockquote
 "Es lässt sich aber von jenem Satze ein viel einfacherer Beweis liefern, der unabhängig von der Betrachtung der Irrationalzahlen ist."
 "Cantor, p. 75. Our translation: *But a much simpler proof of that theorem can be given, which is independent of the consideration of the irrational numbers.*")

;; *Mannigfaltigkeit* (manifold) and *Inbegriff* are Cantor's words for
;; what we call a set; *Mächtigkeit* (power) is what we call cardinality:
;; two sets have the same power when their elements can be matched one to
;; one. A set \"has the power of the number sequence\" when it is countable.
;;
;; ## The elements: sequences of m and w (pp. 75-76)

(page/blockquote
 "Sind nämlich m und w irgend zwei einander ausschliessende Charaktere, so betrachten wir einen Inbegriff M von Elementen: E = (x₁, x₂, ..., xᵥ, ...), welche von unendlich vielen Coordinaten x₁, x₂, ..., xᵥ, ... abhängen, wo jede dieser Coordinaten entweder m oder w ist. M sei die Gesamtheit aller Elemente E."
 "Cantor, pp. 75-76 (the formula line retyped from the page image). Our translation: *Let m and w be any two mutually exclusive characters, and consider a collection M of elements E = (x₁, x₂, ..., xᵥ, ...) that depend on infinitely many coordinates x₁, x₂, ..., xᵥ, ..., where each coordinate is either m or w. Let M be the totality of all elements E.*")

(page/blockquote
 "Zu den Elementen von M gehören beispielsweise die folgenden drei: E′ = (m, m, m, m, ...), E″ = (w, w, w, w, ...), E‴ = (m, w, m, w, ...)."
 "Cantor, p. 76 (formulas retyped from the page image). Our translation: *Among the elements of M are, for example, these three: E′ = (m, m, m, m, ...), E″ = (w, w, w, w, ...), E‴ = (m, w, m, w, ...).*")

;; In today's words, an element of $M$ is a function from the positive
;; integers to the two-element set $\{m, w\}$. Writing $w$ for "in" and
;; $m$ for "out", it is the same thing as a subset of $\mathbb N$: the
;; sequence $(m, w, m, w, \dots)$ is the set of even places. We use this
;; below.
;;
;; ## The theorem and its proof (p. 76)

(page/blockquote
 "Ich behaupte nun, dass eine solche Mannigfaltigkeit M nicht die Mächtigkeit der Reihe 1, 2, ..., v, ... hat. Dies geht aus folgendem Satze hervor: „Ist E₁, E₂, ..., Eᵥ, ... irgend eine einfach unendliche Reihe von Elementen der Mannigfaltigkeit M, so giebt es stets ein Element E₀ von M, welches mit keinem Eᵥ übereinstimmt.“"
 "Cantor, p. 76. Our translation: *I now assert that such a manifold M does not have the power of the sequence 1, 2, ..., v, .... This follows from the following theorem: “If E₁, E₂, ..., Eᵥ, ... is any simply infinite sequence of elements of the manifold M, then there is always an element E₀ of M that agrees with no Eᵥ.”*")

;; The proof writes the claimed list out as a matrix. Cantor's lines, which
;; the OCR cannot read, are (retyped from the page image):
;;
;; $$E_1 = (a_{1,1}, a_{1,2}, \dots, a_{1,\nu}, \dots)$$
;; $$E_2 = (a_{2,1}, a_{2,2}, \dots, a_{2,\nu}, \dots)$$
;; $$\vdots$$
;; $$E_\mu = (a_{\mu,1}, a_{\mu,2}, \dots, a_{\mu,\nu}, \dots)$$
;; $$\vdots$$

(page/blockquote
 "Hier sind die aμ,ν in bestimmter Weise m oder w. Es werde nun eine Reihe b₁, b₂, ..., bᵥ, ... so definirt, dass bᵥ auch nur gleich m oder w und von aᵥ,ᵥ verschieden sei. Ist also aᵥ,ᵥ = m, so ist bᵥ = w, und ist aᵥ,ᵥ = w, so ist bᵥ = m. Betrachten wir alsdann das Element E₀ = (b₁, b₂, b₃, ...) von M, so sieht man ohne weiteres, dass die Gleichung E₀ = Eμ für keinen positiven ganzzahligen Wert von μ erfüllt sein kann, da sonst für das betreffende μ und für alle ganzzahligen Werte von ν: bᵥ = aμ,ᵥ, also auch im besondern: bμ = aμ,μ wäre, was durch die Definition von bᵥ ausgeschlossen ist."
 "Cantor, p. 76 (the formulas retyped from the page image). Our translation: *Here the a_{μ,ν} are, in a definite way, m or w. Now define a sequence b₁, b₂, ..., bᵥ, ... so that bᵥ is also only m or w and differs from a_{ν,ν}. So if a_{ν,ν} = m then bᵥ = w, and if a_{ν,ν} = w then bᵥ = m. If we then consider the element E₀ = (b₁, b₂, b₃, ...) of M, one sees at once that the equation E₀ = E_μ cannot hold for any positive whole value of μ, since otherwise, for that μ and for all whole values of ν, bᵥ = a_{μ,ν}, hence in particular b_μ = a_{μ,μ}, which the definition of bᵥ excludes.*")

(page/blockquote
 "Aus diesem Satze folgt unmittelbar, dass die Gesamtheit aller Elemente von M sich nicht in die Reihenform: E₁, E₂, ..., Eᵥ, ... bringen lässt, da wir sonst vor dem Widerspruch stehen würden, dass ein Ding E₀ sowohl Element von M, wie auch nicht Element von M wäre."
 "Cantor, p. 76. Our translation: *From this theorem it follows at once that the totality of all elements of M cannot be brought into the sequence form E₁, E₂, ..., Eᵥ, ..., since otherwise we should face the contradiction that a thing E₀ would be both an element of M and not an element of M.*")

;; ### Try it: the claimed list
;;
;; The table starts with the first eight rationals of $[0, 1)$ from the
;; zig-zag of 1874, each written as eight binary digits ($m = 0$,
;; $w = 1$): a list someone might claim is complete. The shaded cells are
;; the diagonal $a_{\nu,\nu}$. The last row is $E_0$, the diagonal with
;; every letter flipped. Click any cell, or press the second button to
;; make every letter $m$: $E_0$ is then all $w$ and still differs from
;; every row, at the place that row's number names. Nothing you click can
;; make $E_0$ equal a row.

(w/table-widget)

;; The widget has a finite table, eight rows of eight places, so it shows
;; the *mechanism*: row $\mu$ and $E_0$ disagree at place $\mu$. Cantor's
;; claim concerns infinitely many rows, each infinitely long; the argument
;; is the same, since $E_0$ is defined place by place from the diagonal.
;; The check below runs it in the library, on the 12-row table of
;; `alexandria.cantor.sets` and on a list of 50 sequences given by a
;; function:

(let [rows (mapv :row (sets/binary-rows 12))
      E (card/indexed-list (fn [i] (nth rows i)) 12)
      E-fn (card/indexed-list (fn [i] (fn [k] (if (zero? (mod (+ i k) 3)) :w :m))) 50)]
  (page/badge (and (card/in-no-place? E card/mw-flip 12) (card/in-no-place? E-fn card/mw-flip 50))
              "the diagonal of a 12-row table and of 50 computed sequences is found in none of the rows"))

;; ## The step to every set (pp. 76-77)
;;
;; Cantor does not stop at sequences. He remarks that his proof is not
;; about sequences at all:

(page/blockquote
 "Dieser Beweis erscheint nicht nur wegen seiner grossen Einfachheit, sondern namentlich auch aus dem Grunde bemerkenswert, weil das darin befolgte Princip sich ohne weiteres auf den allgemeinen Satz ausdehnen lässt, dass die Mächtigkeiten wohldefinirter Mannigfaltigkeiten kein Maximum haben oder, was dasselbe ist, dass jeder gegebenen Mannigfaltigkeit L eine andere M an die Seite gestellt werden kann, welche von stärkerer Mächtigkeit ist als L."
 "Cantor, pp. 76-77. Our translation: *This proof seems remarkable not only for its great simplicity but especially because the principle followed in it extends at once to the general theorem that the powers of well-defined manifolds have no maximum, or, what is the same, that for every given manifold L another one M can be set beside it that has a stronger power than L.*")

;; His example is the interval $L$ of real numbers $z$ with
;; $0 \le z \le 1$, and for $M$ he takes all functions $f(x)$ on that
;; interval with values $0$ or $1$:

(page/blockquote
 "Man verstehe unter M den Inbegriff aller eindeutigen Functionen f(x), welche nur die beiden Werte 0 oder 1 annehmen, während x alle reellen Werte, die ≥ 0 und ≤ 1 sind, durchläuft."
 "Cantor, p. 77. Our translation: *Let M be the collection of all single-valued functions f(x) that take only the two values 0 or 1, while x runs through all real values that are ≥ 0 and ≤ 1.*")

;; $M$ is not smaller than $L$, he says, because it has a part of the same
;; power as $L$: the functions that are 1 at one single point $x_0$ and 0
;; everywhere else. Then comes the diagonal again, with a function
;; $\varphi(x, z)$ in place of the matrix $a_{\mu,\nu}$:

(page/blockquote
 "Es hat aber auch M nicht gleiche Mächtigkeit mit L, da sich sonst die Mannigfaltigkeit M in gegenseitig eindeutige Beziehung zu der Veränderlichen z bringen liesse, und es könnte M in der Form einer eindeutigen Function der beiden Veränderlichen x und z: φ(x, z) gedacht werden, so dass durch jede Specialisirung von z ein Element f(x) = φ(x, z) von M erhalten wird und auch umgekehrt jedes Element f(x) von M aus φ(x, z) durch eine einzige bestimmte Specialisirung von z hervorgeht. Dies führt aber zu einem Widerspruch. Denn versteht man unter g(x) diejenige eindeutige Function von x, welche nur die Werte 0 oder 1 annimmt und für jeden Wert von x von φ(x, x) verschieden ist, so ist einerseits g(x) ein Element von M, andererseits kann g(x) durch keine Specialisirung z = z₀ aus φ(x, z) hervorgehen, weil φ(z₀, z₀) von g(z₀) verschieden ist."
 "Cantor, p. 77 (the symbol φ and the subscripts read from the page image). Our translation: *But M also does not have the same power as L, for otherwise M could be brought into one-to-one relation with the variable z, and M could be thought of as a single-valued function of the two variables x and z, φ(x, z), such that every specialisation of z yields an element f(x) = φ(x, z) of M, and conversely every element f(x) of M arises from φ(x, z) by one definite specialisation of z. But this leads to a contradiction. For if g(x) denotes the single-valued function of x that takes only the values 0 or 1 and differs from φ(x, x) for every value of x, then on the one hand g(x) is an element of M, on the other hand g(x) can arise from φ(x, z) by no specialisation z = z₀, because φ(z₀, z₀) differs from g(z₀).*")

(page/blockquote
 "Ist somit die Mächtigkeit von M weder kleiner noch gleich derjenigen von L, so folgt, dass sie grösser ist als die Mächtigkeit von L."
 "Cantor, p. 77. Our translation: *If therefore the power of M is neither smaller than nor equal to that of L, it follows that it is greater than the power of L.*")

;; ## Cantor's theorem, in today's form
;;
;; A function $f(x)$ with values $0$ or $1$ on $L$ is the same as a subset
;; of $L$: the set of places where it is 1. So $M = \mathcal P(L)$, and a
;; one-to-one matching of $L$ with $M$ would give a map
;; $f : L \to \mathcal P(L)$ onto $\mathcal P(L)$. Cantor's $g$ is the set
;;
;; $$D = \{\, x \in L : x \notin f(x) \,\}.$$
;;
;; For each $x$, $D$ and $f(x)$ disagree about whether $x$ belongs to them,
;; so $D \neq f(x)$: $D$ is a subset that no $f(x)$ equals, whatever $f$
;; is. The sequence case of the previous section is $L = \mathbb N$: with
;; $f(n) = \{k : E_n(k) = w\}$ the set $D$ is the set of places where the
;; diagonal $E_0$ is $w$ (the library tests this identity on a prefix,
;; `card/characteristic-set`). One proof, written twice.
;;
;; ### Try it: every map on a set with up to three elements
;;
;; Choose $n \le 3$ and, for each $x \in X = \{0, \dots, n-1\}$, the
;; subset $f(x)$ by ticking boxes. The page computes $D$ and shows that it
;; is no $f(x)$; the shaded cells, where the row asks "is $x$ in $f(x)$?",
;; are the diagonal. The button runs **all** maps $X \to \mathcal P(X)$ for
;; $n = 1, 2, 3$ (there are $(2^n)^n = 2, 16, 512$ of them, computed by
;; `card/cantor-report`) and reports in how many $D$ is missed:

(w/theorem-widget)

;; For these finite sets the theorem is easy to believe: $\mathcal P(X)$
;; has $2^n$ elements and a map from $n$ elements reaches at most $n$ of
;; them. What Cantor's proof shows is that **the same $D$ works without
;; counting**, so it still works when $X$ is infinite and counting no
;; longer separates the sets. The check confirms
;; $|\mathcal P(X)| = 2^{|X|} > |X|$ and the count of missed maps:

(page/badge (every? (fn [{:keys [n size power-size maps missed]}]
                      (and (= maps missed) (= size n) (= power-size (bit-shift-left 1 n))
                           (< size power-size)))
                    (map card/cantor-report [0 1 2 3]))
            "for n = 0, 1, 2, 3: |P(X)| = 2^n > n, and D is missed by all 1 + 2 + 16 + 512 maps")

;; ## No largest infinity (pp. 77-78)
;;
;; Cantor ends by pointing back to his *Grundlagen einer allgemeinen
;; Mannigfaltigkeitslehre* (1883), where he had shown by other means that
;; the powers have no maximum, and forward:

(page/blockquote
 "Die „Mächtigkeiten“ repräsentiren die einzige und notwendige Verallgemeinerung der endlichen „Cardinalzahlen“, sie sind nichts anderes als die actual-unendlich-grossen Cardinalzahlen, und es kommt ihnen dieselbe Realität und Bestimmtheit zu, wie jenen; nur dass die gesetzmässigen Beziehungen unter ihnen, die auf sie bezügliche „Zahlentheorie“ zum Teil eine andersartige ist, wie im Gebiete des Endlichen. Die weitere Erschliessung dieses Feldes ist Aufgabe der Zukunft."
 "Cantor, pp. 77-78. Our translation: *The “powers” represent the only and necessary generalisation of the finite “cardinal numbers”; they are nothing other than the actually-infinitely-large cardinal numbers, and the same reality and definiteness belong to them as to those; only that the lawful relations among them, the “number theory” belonging to them, is in part of a different kind than in the realm of the finite. The further opening-up of this field is a task of the future.*")

;; Applying the theorem again and again to $\mathbb N$ gives
;; $|\mathbb N| < |\mathcal P(\mathbb N)| < |\mathcal P(\mathcal P(\mathbb N))| < \cdots$,
;; an unending ladder: there is no largest infinity.
;;
;; ## What 1891 adds to 1874, and where it went
;;
;; | | 1874 | 1891 |
;; |:---|:---|:---|
;; | objects | real numbers in an interval | sequences of two letters, then any set |
;; | tool | nested intervals, the order of the line | one flipped diagonal; no order |
;; | result | the reals are not a list | no set matches its power set |
;; | needs | completeness of the line | nothing about the elements |
;;
;; Because the 1891 proof uses no property of the elements, it travels.
;; Russell's paradox (1901) is $D$ for the map $f(x) = x$ on the set of
;; all sets. Gödel's sentence and Turing's halting problem are built by
;; flipping a diagonal of provability or of halting
;; ([the Gödel and Turing notebook](../goedel-turing/)). The structure in
;; `alexandria.structure.cardinality` is that shared core: an enumeration,
;; a flip, and the diagonal it yields.
;;
;; What Cantor could not decide, whether a set lies strictly between
;; $\mathbb N$ and $\mathcal P(\mathbb N)$ in size, became the continuum
;; hypothesis, Hilbert's first problem of 1900.

^{::clerk/no-cache true}
(page/prev-next "notebooks/set_theory_cantor_1891.clj")

(lens/section "set-theory-cantor-1891")
