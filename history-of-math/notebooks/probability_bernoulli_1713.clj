;; # Jacob Bernoulli, 1713: the first law of large numbers
;;
;; Jacob Bernoulli worked on the *Ars conjectandi* for twenty years and died
;; in Basel in 1705 without printing it. His nephew Nicolaus published it in
;; 1713 (Basel, Thurneysen). Parts I to III reprint Huygens' tract on games of
;; chance with Jacob's commentary, the theory of permutations and
;; combinations, and twenty-four problems on dice. Part IV, *Usus et
;; applicatio praecedentis doctrinae in civilibus, moralibus et oeconomicis*,
;; carries the doctrine over to civil, moral and economic matters, where nobody
;; can count the cases beforehand as one counts the faces of a die. Bernoulli's
;; answer is to count them afterwards, by trials, and the chapter that ends the
;; book proves that this can be made as sure as one pleases.
;;
;; The Latin is quoted from the printed text of 1713 (the Smithsonian scan on
;; the Internet Archive, [jacobibernoulli00bern](https://archive.org/details/jacobibernoulli00bern)),
;; read against its OCR; the long *s* is written *s*, abbreviations are
;; expanded, and a page link follows each quotation. The English beside the
;; Latin is this series' own translation. Page numbers are the printed ones.
;;
;; Notation used below. A trial has *fertile* cases (those in which the event
;; happens) and *sterile* ones; Bernoulli writes $r$ and $s$ for their numbers
;; and $t = r + s$, so the probability of the event is $p = r/t$. In $n$ trials
;; the event happens $S_n$ times, and $S_n/n$ is the observed frequency. The
;; figures and checks come from `alexandria.structure.probability`, a finite
;; probability space (a *measure* of total mass 1) with exact rational masses,
;; of which the binomial distribution of $S_n$ is the $n$-fold product of a
;; single trial; no random numbers are drawn there.
^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns probability-bernoulli-1713
  {:history/year 1713
   :history/title "Jacob Bernoulli, Ars conjectandi IV: the first law of large numbers (1713)"
   :history/era "Early calculus"
   :history/fields #{:probability :graph-theory}
   :nextjournal.clerk/visibility {:code :hide :result :show}
   :nextjournal.clerk/toc true}
  (:require [alexandria.bernoulli.widgets :as w]
            [history-of-math.lens :as lens]
            [history-of-math.page :as page]
            [history-of-math.widgets :as hom]
            [nextjournal.clerk :as clerk]))

^{::clerk/visibility {:code :hide :result :hide}}
(hom/install!)

;; ## 1. What can be called certain
;;
;; Chapter I of Part IV is a vocabulary. Certainty is a whole, a probability
;; is a part of it, and Bernoulli names the cases near the ends:

(page/blockquote
 "Moraliter certum est, cujus probabilitas fere aequatur integrae certitudini, sic ut defectus sentiri non possit."
 "Bernoulli, *Ars conjectandi* (Basel 1713), Pars IV, cap. I, p. 211 ([scan](https://archive.org/details/jacobibernoulli00bern/page/n264/mode/1up)). Translation: *Morally certain is that whose probability nearly equals whole certainty, so that the shortfall cannot be felt.* Whatever has only the probability that is the shortfall of the morally certain is *morally impossible*. In the examples that follow, the odds are one thousand to one.")

;; ## 2. The question
;;
;; For a die the cases are known, so the chances are. For the weather, the
;; plague or a man's life they are not, and Bernoulli's proposal, in chapter IV,
;; is to learn them from experience, as everyone does. He asks whether that
;; can ever be made certain, or whether it is hopeless like an asymptote that
;; is approached and never reached:

(page/blockquote
 "Inquirendum nimirum restat, an aucto sic observationum numero ita continuo augeatur probabilitas assequendae genuinae rationis inter numeros casuum, quibus eventus aliquis contingere et quibus non contingere potest, ut probabilitas haec tandem datum quemvis certitudinis gradum superet."
 "Bernoulli, *Ars conjectandi*, Pars IV, cap. IV, p. 225 ([scan](https://archive.org/details/jacobibernoulli00bern/page/n278/mode/1up)). Translation: *It remains to ask whether, as the number of observations grows, the probability of reaching the true ratio between the numbers of cases in which an event can happen and can not happen goes on increasing, until at last it exceeds any given degree of certainty.*")

;; In modern terms: for each tolerance $\varepsilon > 0$, does
;; $P(|S_n/n - p| \le \varepsilon)$ tend to 1, and how fast? He says that the
;; answer is not obvious, and that he has found a proof.
;;
;; ## 3. The theorem, as he states it
;;
;; Chapter V proves lemmas on the terms of the binomial $(r+s)^{nt}$ (the
;; largest term, and how fast the terms fall away from it), and then states
;; what the lemmas are for. With the fertile and sterile cases in the ratio
;; $r : s$ he keeps the observed ratio between $(r-1)/t$ and $(r+1)/t$, that is,
;; within $1/t$ of $p$, and asks for as many trials as make it $c$ times more
;; likely to fall inside than outside:

(page/blockquote
 "Ostendendum est, tot posse capi experimenta, ut datis quotlibet (puta c) vicibus verisimilius evadat, numerum fertilium observationum intra hos limites quam extra casurum esse."
 "Bernoulli, *Ars conjectandi*, Pars IV, cap. V, main proposition, p. 236 ([scan](https://archive.org/details/jacobibernoulli00bern/page/n289/mode/1up)). Translation: *It must be shown that so many trials can be taken that it becomes more likely, by as many times as one pleases (say $c$), that the number of fertile observations falls within these limits than outside them.* The limits are $(r+1)/t$ and $(r-1)/t$ of the number of trials.")

;; Then the proof runs through the terms of the binomial. In the language of
;; the structure below: the number of fertile observations is a random variable
;; on the $n$-fold product of the trial, its masses are the binomial terms
;; $\binom{n}{k} p^k (1-p)^{n-k}$, and the event "within the limits" is a
;; sum of consecutive terms. Bernoulli compares that sum with the rest term by
;; term, the largest term to the one at the edge, and so does not need to add
;; them. Our widget does add them, exactly, with integers.
;;
;; ## 4. The numbers
;;
;; The specific case he works is $r = 30$, $s = 20$, so $t = 50$, $p = 3/5$,
;; limits $31/50$ and $29/50$, and $c = 1000$. The lemmas give two
;; trial counts (one for each side of the band), and the larger is his answer:

(page/blockquote
 "Unde per ibi demonstrata infertur, quod institutis 25550 experimentis multo plus millies verisimilius sit, rationem quam numerus fertilium observationum obtinebit ad numerum omnium, [...] cadere intra hos limites quam extra."
 "Bernoulli, *Ars conjectandi*, Pars IV, cap. V, p. 238 ([scan](https://archive.org/details/jacobibernoulli00bern/page/n291/mode/1up)); the limits, here elided, are $31/50$ and $29/50$. Translation: *From what is shown there it follows that with 25550 trials it is much more than a thousand times more likely that the ratio of fertile observations to all will fall within these limits than outside them.* For $c = 10000$ he states 31258 trials, and for $c = 100000$ the scan reads 36966; each factor of ten costs 5708 more.")

;; `alexandria.structure.probability/bernoulli-n` carries out his arithmetic:
;; $m$ is the first integer with $\bigl(\tfrac{r+1}{r}\bigr)^m \ge c(s-1)$
;; (and the mirror for the other side), and the trials are $n_1 = m t +
;; \tfrac{s t (m-1)}{r+1}$ at least; for $r=30$, $s=20$, $c=1000$ that is
;; $m = 301$ and 24728 on one side and $m = 211$ and **25550** on the other.
;; Our function returns 25550 for $c = 1000$ and 31258 for $c = 10000$; for
;; $c = 100000$ it returns 36965, one trial under the 36966 of the scan, a
;; difference of rounding in the logarithms, or of a misread digit.
;;
;; This is the first place in print where a frequency is tied to a probability
;; by a theorem with a number in it. It is also very wasteful. Bernoulli
;; compares the largest term with the edge ones and discards everything in
;; between; the exact probability that comes from summing all the terms is far
;; closer to 1 at 25550 than 1000 to 1, and it passes 1000 to 1 well before.
;; Pick the ratio, a scale (this multiplies $r$ and $s$, so makes the band
;; $1/t$ narrower), and the odds; move $n$ and read where the exact line
;; crosses the green line of the odds, against Bernoulli's $n$ and against
;; Chebyshev's lower bound $1 - p(1-p)/(n\varepsilon^2)$ (Chebyshev came in
;; 1867, and gives a bound that holds for every distribution, not just the
;; binomial). The histogram is the exact distribution of $S_n/n$, the band
;; $p \pm 1/t$ shaded, narrowing round $p$ as $n$ grows; the circle is one
;; draw of a seeded pseudo-random generator, a *sample*, there to show that
;; a run falls in the band, and not a proof of anything.

(w/law-widget)

;; Three things to try. At scale 2 and ratio 3 : 2 the band is $\pm 1/10$ and
;; one hundred trials are already inside it with high probability. At scale 10
;; (Bernoulli's $r = 30$, $s = 20$ is ratio 3 : 2 at scale 10) the band is
;; $\pm 1/50$ and the curve has to wait until $n$ is in the thousands. And for
;; any case the blue line keeps rising to 1, which is the content of the
;; theorem, while the sentence under the slider says in odds what that means.
;;
;; ## 5. Moral certainty and the fate of the world
;;
;; Bernoulli sees at once what the theorem says about nature. If the trials
;; could go on for ever, probability would end in certainty, and in the most
;; fortuitous things we should recognise a law:

(page/blockquote
 "omnia in mundo certis rationibus et constanti vicissitudinis lege contingere deprehenderentur"
 "Bernoulli, *Ars conjectandi*, Pars IV, cap. V, p. 239 ([scan](https://archive.org/details/jacobibernoulli00bern/page/n292/mode/1up)). Translation: *everything in the world would be found to happen by fixed ratios and by a constant law of change.* He goes on to the \"necessity\" and \"fatality\" that this makes us admit even in the most casual events, and wonders whether Plato had it in mind in his doctrine of the return of all things after countless ages. There the book breaks off, unfinished; Bernoulli had meant to apply it to civil and economic questions.")

;; ## What the idea became
;;
;; Bernoulli's theorem is the *weak law of large numbers* for coin-tossing:
;; $P(|S_n/n - p| \le \varepsilon) \to 1$ for every $\varepsilon > 0$. The name
;; *loi des grands nombres* is Poisson's, of 1835. De Moivre in 1733 sharpened
;; the estimate by approximating the binomial with the curve now called normal,
;; so that the wasteful $n$ of Bernoulli could be cut to what the exact sum
;; shows. Bayes (1763) and Laplace (1812) turned the direction round and asked
;; what the trials say about $p$. Chebyshev (1867) gave the bound for any
;; variable with a variance, and Borel (1909) and Cantelli the *strong* law,
;; that $S_n/n \to p$ for almost every infinite sequence of trials, which needs
;; a probability on infinite sequences, that is, a measure. That is the step
;; this series follows, through Jordan's content (1892) and Lebesgue's
;; integral (1904), to Kolmogorov's axioms (1933), in which a probability
;; space is a measure of total mass 1. The protocol in
;; `alexandria.structure.probability` is that, restricted to a finite set; a
;; Lebesgue measure on an interval will mount the same protocol (outcomes,
;; the measure of an event, expectation) and the laws it is tested against: the
;; whole has mass 1, masses add on disjoint events, expectation is linear.

^{::clerk/no-cache true}
(page/prev-next "notebooks/probability_bernoulli_1713.clj")

(lens/section "probability-bernoulli-1713")
