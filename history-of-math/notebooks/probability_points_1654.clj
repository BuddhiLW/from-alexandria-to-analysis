;; # Pascal and Fermat, 1654: the problem of points
;;
;; In the summer of 1654 the Chevalier de Méré, a gambler and a man of the
;; salons, put two questions to Blaise Pascal in Paris. One was about dice.
;; The other was older: two players have each staked the same sum on a game
;; of several rounds, and they stop before the end. One of them lacks a
;; points to win, the other b. How should they divide the money on the
;; table? Luca Pacioli had answered it wrongly in 1494, and Cardano and
;; Tartaglia had argued about it since. Pascal wrote to Pierre de Fermat, a
;; magistrate in Toulouse, and between July and October the two of them
;; settled it by two different methods. Their letters are the start of the
;; calculus of probability.
;;
;; Fermat counted: he listed every way the remaining rounds could fall and
;; divided the stakes as the number of ways each player wins. Pascal
;; reasoned backward from the end of the game, one round at a time. Pascal
;; then found the counts already written in a table of numbers he had been
;; studying, the *triangle arithmétique*, printed after his death in 1665.
;; Three years after the letters, Christiaan Huygens, who had heard of the
;; question in Paris but not of its answer, wrote the first printed treatise
;; on chance, *De ratiociniis in ludo aleae* (1657), and turned the split
;; into a general rule: the value of an expectation.
;;
;; Pascal's French is quoted from the *Œuvres de Fermat*, vol. II,
;; *Correspondance* (ed. P. Tannery and C. Henry, Paris 1894), letters LXX
;; and LXXII, and from the *Traité du triangle arithmétique* (Paris,
;; Desprez, 1665). Huygens is quoted in his Latin of 1657 (in Schooten's
;; *Exercitationes mathematicae*, Leiden) and in the English of 1714 (*The
;; Value of all Chances in Games of Fortune*, London). The English beside
;; the French and Latin is this series' own translation. Spelling is
;; modernised where the print uses the long s and u for v.
;;
;; The figures and checks come from Alexandria's shelf
;; `alexandria.pascal.points`: *proved* means Emmy simplified a difference
;; to exactly 0, *numeric* that numbers from raster were within the bound.
^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns probability-points-1654
  {:history/year 1654
   :history/title "Pascal and Fermat, the problem of points (1654), with Huygens' expectation (1657)"
   :history/era "Scientific Revolution"
   :history/fields #{:probability :graph-theory}
   :nextjournal.clerk/visibility {:code :hide :result :show}
   :nextjournal.clerk/toc true}
  (:require [alexandria.pascal.points :as pp]
            [alexandria.pascal.widgets :as w]
            [history-of-math.lens :as lens]
            [history-of-math.page :as page]
            [history-of-math.widgets :as hom]
            [nextjournal.clerk :as clerk]))

^{::clerk/visibility {:code :hide :result :hide}}
(hom/install!)

;; ## 1. The problem of points
;;
;; Pascal's letter of 29 July 1654 begins with the simplest case. Two
;; players play for three points and have each put 32 pistoles in the game;
;; the first has two points, the other one.

(page/blockquote
 "Posons que le premier en ait deux et l'autre une; ils jouent maintenant une partie, dont le sort est tel que, si le premier la gagne, il gagne tout l'argent qui est au jeu, savoir 64 pistoles; si l'autre la gagne, ils sont deux parties à deux parties, et par conséquent, s'ils veulent se séparer, il faut qu'ils retirent chacun leur mise, savoir chacun 32 pistoles."
 "Pascal to Fermat, 29 July 1654, *Œuvres de Fermat* II, letter LXX, p. 290. Translation: *Suppose the first has two points and the other one; they now play a round whose fate is such that if the first wins it he wins all the money in the game, that is 64 pistoles; if the other wins it they are two points to two, and so, if they wish to part, each must take back his stake, that is 32 pistoles each.*")

;; So the first player is sure of 32, and the other 32 are an even chance:

(page/blockquote
 "Je suis sûr d'avoir 32 pistoles, car la perte même me les donne; mais pour les 32 autres, peut-être je les aurai, peut-être vous les aurez, le hasard est égal. Partageons donc ces 32 pistoles par la moitié et me donnez, outre cela, mes 32 qui me sont sûres."
 "Pascal, letter LXX, p. 291. Translation: *I am sure of 32 pistoles, for even losing gives me them; but as for the other 32, perhaps I shall have them, perhaps you will, the chance is equal. Let us then divide these 32 pistoles in half, and give me besides my 32 that are sure.* He has 48, the other 16.")

;; In modern terms: if E(a, b) is the fraction of the 2s on the table that
;; belongs to the player who lacks a points while his opponent lacks b, the
;; split is 2s E(a, b) to 2s (1 − E(a, b)). Choose a, b and the stake; the
;; bar splits live. The share is tied to the stake: type a share and the
;; stake that produces it is solved back.

(w/stake-tie w/game)

;; ## 2. Fermat's method: count the outcomes
;;
;; Fermat's letter is lost, but Pascal restates its method in his reply of
;; 24 August, on the case where the first player lacks two points and the
;; second three:

(page/blockquote
 "Il est aisé de supputer que ce sera en quatre parties, d'où vous concluez qu'il faut voir combien quatre parties se combinent entre deux joueurs et voir combien il y a de combinaisons pour faire gagner le premier et combien pour le second et partager l'argent suivant cette proportion."
 "Pascal to Fermat, 24 August 1654, *Œuvres de Fermat* II, letter LXXII, p. 301. Translation: *It is easy to reckon that it will be in four rounds, from which you conclude that one must see how four rounds combine between two players, and how many combinations make the first win and how many the second, and share the money in that proportion.*")

;; He makes the rounds into dice with two faces, a for the first player and
;; b for the second, and lists the sixteen ways four of them can land:

(page/blockquote
 "toutes les faces qui ont deux a le font gagner : donc il en a 11 pour lui; et parce qu'il y manque trois parties au second, toutes les faces où il y a trois b le peuvent faire gagner : donc il y en a 5. Donc il faut qu'ils partagent la somme comme 11 à 5."
 "Pascal, letter LXXII, p. 302. Translation: *every face with two a's makes him win: so he has 11 for him; and because the second lacks three points, every face with three b's can make him win: so there are 5. So they must share the sum as 11 to 5.*")

;; The rows below are the outcomes for your a and b, one raster kernel call
;; per row, lit by the winner. Roberval objected that the game would often
;; end before all n rounds were played; Pascal answered that playing on
;; changes nothing, since the winner is already decided in every row.

(w/fermat-table w/game)

;; ## 3. Pascal's method: from the end of the game backward
;;
;; Pascal preferred his own method, "plus courte et plus nette". His *Traité*
;; states the principle on which it rests:

(page/blockquote
 "Si deux joueurs jouent à un jeu de pur hasard, à condition que si le premier gagne il lui reviendra une certaine somme, et s'il perd il lui en reviendra une moindre, s'ils veulent se séparer sans jouer, et prendre chacun ce qui leur appartient, le parti est, que le premier prenne ce qui lui revient en cas de perte, et de plus la moitié de l'excès, dont ce qui lui reviendrait en cas de gain surpasse ce qui lui revient en cas de perte."
 "Pascal, *Usage du triangle arithmétique pour déterminer les partis*, Corollaire premier, *Traité du triangle arithmétique* (1665). Translation: *If two players play a game of pure chance on condition that if the first wins a certain sum comes to him and if he loses a smaller one, and they wish to part without playing and each take what belongs to him, the split is that the first takes what comes to him in case of loss, and besides half the excess by which what would come to him in case of gain exceeds what comes to him in case of loss.*")

;; Each position (i, j) leads, after one round, to (i − 1, j) or to
;; (i, j − 1), so its value is the mean of those two. The edges are known:
;; a player who lacks no points has won (1), one whose opponent lacks none
;; has lost (0). Step through the grid in Pascal's order, the positions one
;; round from the end first; click any cell to see the two it depends on.

(w/pascal-grid w/game)

;; Emmy checks the step and solves the recursion along the first two rows:

(w/derivation)

;; ## 4. The arithmetic triangle
;;
;; The same numbers stand in a table Pascal built for other purposes. Its
;; definition, one sentence at a time:

(w/stepped-triangle)

;; Then the rule of the *partis*:

(page/blockquote
 "Soit prise dans le triangle la base dans laquelle il y a autant de cellules qu'il manque de parties aux deux ensemble. Ensuite soient prises dans cette base autant de cellules continues à commencer par la première, qu'il manque de parties au premier joueur, et qu'on prenne la somme de leurs nombres. Donc il reste autant de cellules, qu'il manque de parties à l'autre."
 "Pascal, *Usage du triangle arithmétique pour déterminer les partis*, *Traité* (1665). Translation: *Take in the triangle the base that has as many cells as the points the two lack together. Then take in this base as many consecutive cells, starting from the first, as the points the first player lacks, and take the sum of their numbers. There remain as many cells as the points the other lacks.* Each player receives the sum of the cells counted for the other.")

;; The cell at place k of the base of n + 1 cells counts the outcomes of n
;; rounds in which the first player wins exactly k of them, which is why
;; the triangle and Fermat's count agree, and Pascal's recursion is the
;; triangle's own rule of addition. The widget runs all three methods on
;; raster for your a and b:

(w/triangle-row w/game)

;; And for every game in the widgets' range, by raster on the server (the
;; first player's fraction):

(w/agreement-table)

(page/grade-badges (pp/graded) #{:grade/proved :grade/numeric})

;; ## 5. Huygens, 1657: the value of a chance
;;
;; Huygens takes as his foundation that a chance is worth what would buy it
;; in a fair game:

(page/blockquote
 "Hoc autem utrobique utar fundamento: nimirum, in alea, tanti aestimandam esse cujusque sortem seu expectationem ad aliquid obtinendum, quantum si habeat, possit denuo ad similem sortem sive expectationem pervenire, aequa conditione certans."
 "Huygens, *De ratiociniis in ludo aleae*, in Schooten, *Exercitationes mathematicae* (Leiden 1657), pp. 520-521, before Prop. I. Translation: *I shall use this foundation throughout: that in games of chance each one's lot or expectation of obtaining something is to be valued at the sum which, if he had it, would let him reach the same lot or expectation again, playing on fair terms.*")

(page/blockquote
 "If I expect a or b, and have an equal Chance of gaining either of them, my Expectation is worth (a + b)/2."
 "Huygens, Prop. I, in the English of 1714, *The Value of all Chances in Games of Fortune* (London); the fraction set in modern type.")

(page/blockquote
 "If the number of Chances I have to gain a, be p, and the number of Chances I have to gain b, be q, supposing the Chances equal; my Expectation will be worth (ap + bq)/(p + q)."
 "Huygens, Prop. III, ibid.")

;; The price is tied to the chances both ways: move a prize or a number of
;; cases and the price follows; type a price and the q that makes it fair
;; is solved back. The preset "problem of points" restates Pascal's 2
;; against 3 as an expectation: the whole 64 in 11 cases of 16, nothing in
;; 5, worth 44.

(w/huygens)

(w/derivation-huygens)

;; Huygens then treats the problem of points itself (Propositions IV to IX)
;; by Pascal's backward method, and in his Proposition XIV gives the first
;; problem of chance that needs an equation. Jacob Bernoulli reprinted the
;; whole tract with his commentary as Part I of the *Ars conjectandi*
;; (1713), where the expectation of Huygens meets the law of large numbers.

^{::clerk/no-cache true}
(page/prev-next "notebooks/probability_points_1654.clj")

(lens/section "probability-points-1654")
