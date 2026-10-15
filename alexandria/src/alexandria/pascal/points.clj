(ns alexandria.pascal.points
  "The problem of points (Pascal and Fermat, letters of 1654; Pascal, Traite
   du triangle arithmetique, 1665; Huygens, De ratiociniis in ludo aleae,
   1657).

   Two players stake s each; the game is interrupted when the first lacks a
   points and the second b. Each round is a fair coin. The fair split gives
   the first player the fraction E(a, b) of the 2s on the table, found three
   ways:

     Fermat    play out the n = a + b - 1 rounds that decide the game in
               every case, list the 2^n equally likely outcomes, count those
               the first player wins (Pascal's letter of 24 Aug. 1654
               reports Fermat's method): `outcome`, one kernel call per row.
     Pascal    work backward: E(0, b) = 1, E(a, 0) = 0 and
               E(a, b) = (E(a - 1, b) + E(a, b - 1)) / 2, the mean of the two
               positions the next round leads to (letter of 29 July 1654):
               `mean`, one kernel call per cell of the grid.
     triangle  the base of the arithmetic triangle with a + b cells: the
               first player gets the sum of its last b cells over the sum
               of the base (Usage du triangle arithmetique pour determiner
               les partis, 1665): `triangle`.

   Huygens' Propositions I-III: a chance worth a in p cases and b in q
   equally likely cases is worth x = (p a + q b) / (p + q): `huygens-tie`.

   Every kernel is Emmy arithmetic, branch-free and small (at most six
   arguments, no floor: the bits of an outcome come from the sign of a
   sine, indicators from |x| / x at half-integer offsets, so integer
   arguments never sit on a jump), so alexandria.raster on the JVM and the
   :raster backend of emmy-viewers compile the same functions. Every number
   the notebook shows comes from them. The widgets' range is
   1 <= a, b <= `max-missing` (at most 9 rounds, 512 outcomes)."
  (:require [alexandria.grade :as grade]
            [alexandria.raster :as raster]
            [emmy.env :as e]
            [hive-dsl.result :as r]))

(def max-missing "The widgets' range: 1 <= a, b <= this." 5)
(def max-rounds "Rounds that decide every game in range: 2 max-missing - 1." (dec (* 2 max-missing)))

;; ---------------------------------------------------------------------------
;; Branch-free indicators (exact on integers)

(defn- step-up
  "1 where x > 0, 0 where x < 0."
  [x]
  (e/* 1/2 (e/+ 1 (e/divide x (e/abs x)))))

(defn ge "[x >= t] for integers x, t." [x t] (step-up (e/+ (e/- x t) 1/2)))
(defn lt "[x < t] for integers x, t." [x t] (e/- 1 (ge x t)))

(defn rounds "n = a + b - 1: the rounds that decide the game in every case." [a b]
  (e/- (e/+ a b) 1))

;; ---------------------------------------------------------------------------
;; Fermat: the enumeration

(defn bit
  "Round j of outcome k (k = 0 .. 2^n - 1 read in binary, round 0 the
   lowest bit): 1 when the first player wins it. Branch-free: the sign of
   sin(pi (k + 1/2) / 2^j)."
  [k j]
  (step-up (e/- 0 (e/sin (e/* (/ Math/PI (bit-shift-left 1 j)) (e/+ k 1/2))))))

(defn outcome
  "Outcome k of the n = a + b - 1 rounds: [winner wins-a bit0 .. bit8],
   winner 1 when the first player wins at least a of the n rounds (then the
   second wins fewer than b), else 0."
  [a b k]
  (let [n (rounds a b)
        bs (mapv #(bit k %) (range max-rounds))
        wa (apply e/+ (for [j (range max-rounds)] (e/* (nth bs j) (lt j n))))]
    (into [(ge wa a) wa] bs)))

(defn outcome-letters
  "Outcome k of n rounds as Fermat's letters, a for the first player, b for
   the second (first round first)."
  [k n]
  (apply str (for [j (range n)] (if (bit-test k j) "a" "b"))))

(defn outcome-count
  "[n 2^n]: the rounds n = a + b - 1 and the number of their outcomes, 2^n
   as exp(n ln 2) (raster has no power of a variable exponent; the page
   rounds it)."
  [a b]
  [(rounds a b) (e/exp (e/* (rounds a b) (Math/log 2)))])

(defn split
  "The split from a count: f outcomes for the first player, g for the
   second, stake s each: [fraction share-first share-second total], the
   first player's fraction f / (f + g) of the 2s on the table."
  [f g s]
  (let [total (e/+ f g)
        frac (e/divide f total)]
    [frac (e/* 2 s frac) (e/* 2 s (e/divide g total)) total]))

;; ---------------------------------------------------------------------------
;; Pascal: the recursion

(defn mean
  "Pascal's step: a position whose next round leads to u if won and v if
   lost is worth [(u + v) / 2, (u - v) / 2]: the sure part v plus half the
   uncertain part u - v (Corollaire premier), the uncertain half second."
  [u v]
  [(e/* 1/2 (e/+ u v)) (e/* 1/2 (e/- u v))])

(def cells
  "The grid's cells [i j], i the first player's missing points, in Pascal's
   order: by i + j (the positions one round from the end first), then i."
  (vec (sort-by (fn [[i j]] [(+ i j) i])
                (for [i (range 1 (inc max-missing)) j (range 1 (inc max-missing))] [i j]))))

;; ---------------------------------------------------------------------------
;; The arithmetic triangle

(defn cell
  "The cell of Pascal's triangle in base n at place k (k = 0 the first):
   C(n, k) = n (n - 1) .. (n - k + 1) / k!, 0 beyond the base."
  [n k]
  (reduce e/* 1 (for [i (range k)] (e/divide (e/- n i) (inc i)))))

(defn triangle-cell
  "The cell in parallel rank i, perpendicular rank j (Pascal's square
   layout, 0-based): C(i + j, i) = prod over t = 1 .. i of (j + t) / t,
   branch-free for i up to max-rounds (each factor switched on by [i >= t])."
  [i j]
  (reduce e/* 1 (for [t (range 1 (inc max-rounds))]
                  (e/+ 1 (e/* (ge i t) (e/- (e/divide (e/+ j t) t) 1))))))

(defn base
  "The base of the arithmetic triangle with a + b cells: [cell0 .. cell9]
   (n = a + b - 1; a falling factorial C(n, k) is 0 for k > n, so the base
   needs no mask)."
  [a b]
  (let [n (rounds a b)] (mapv #(cell n %) (range (inc max-rounds)))))

(defn triangle
  "Pascal's rule of the partis for a, b: [sum-first sum-second], the first
   player taking the last b cells of the base (places k >= a), the second
   the first a."
  [a b]
  (let [n (rounds a b)
        cs (mapv #(cell n %) (range (inc max-rounds)))]
    [(apply e/+ (map-indexed (fn [k c] (e/* (ge k a) c)) cs))
     (apply e/+ (map-indexed (fn [k c] (e/* (lt k a) c)) cs))]))

;; ---------------------------------------------------------------------------
;; Section 1: the stake, tied

(def tied "The stake tie: each player's stake s and the first player's share." [:s :share])
(def ranges {:s [1 100 1] :share [0.5 200 0.5]})
(def labels {:s "s, each player's stake" :share "first player's share"})

(defn- clamp [x lo hi]
  (let [at-least (fn [x lo] (e/* 1/2 (e/+ x lo (e/abs (e/- x lo)))))
        at-most (fn [x hi] (e/* 1/2 (e/- (e/+ x hi) (e/abs (e/- x hi)))))]
    (at-most (at-least x lo) hi)))

(defn split-tie
  "The tie of section 1 at fraction E = E(a, b) (a kernel parameter, from
   the triangle kernel): state [s share], one block per source. s moved:
   share = 2 s E. share moved: s = share / (2 E), clamped to [1, 100], and
   the share re-solved from it."
  [E s share]
  (let [s' (clamp (e/divide share (e/* 2 E)) 1 100)]
    [s (e/* 2 s E) s' (e/* 2 s' E)]))

;; ---------------------------------------------------------------------------
;; Huygens: the value of a chance

(def huygens-keys [:a :b :p :q :x])
(def huygens-ranges {:a [0 200 1] :b [0 200 1] :p [0.1 300 0.1] :q [0.1 300 0.1] :x [0 200 0.01]})
(def huygens-labels {:a "a, the first prize" :b "b, the second prize" :p "p, cases giving a"
                     :q "q, cases giving b" :x "x, the fair price"})

(defn expectation "Huygens, Prop. III: (p a + q b) / (p + q)." [a b p q]
  (e/divide (e/+ (e/* p a) (e/* q b)) (e/+ p q)))

(defn chances-for-price
  "The inverse of Prop. III: the q that makes x the price, p fixed:
   q = p (a - x) / (x - b)."
  [a b p x]
  (e/divide (e/* p (e/- a x)) (e/- x b)))

(def huygens-eps "Keeps the price strictly between the prizes (origin guard)." 1e-3)

(defn huygens-solve
  "The inverse kernel of the Huygens tie, [a b p x] -> [q]: the price x read
   as the fraction t = (x - b) / (a - b) of the way from b to a (clamped to
   [0.002, 0.998], the origin guard; a = b is guarded by 1e-9), then the
   inverse of Prop. III, q = p (1 - t) / t, clamped to [0.1, 300]. The page
   re-prices x from this q with `expectation`."
  [a b p x]
  (let [t (clamp (e/divide (e/- x b) (e/+ (e/- a b) 1e-9)) 0.002 0.998)]
    [(clamp (e/divide (e/* p (e/- 1 t)) t) 0.1 300)]))

(defn huygens-tie
  "The tie on the JVM, through raster: the state after source key src moved
   in state m ({:a :b :p :q :x}). x moved: q from `huygens-solve`, then x
   re-priced; anything else: x re-priced."
  [src {:keys [a b p q] :as m}]
  (let [rv (fn [f & xs] (binding [raster/*simplify?* false] (apply raster/value f xs)))
        q' (if (= src :x) (first (rv huygens-solve a b p (:x m))) q)]
    (assoc m :q q' :x (rv expectation a b p q'))))

(def stake-keys "The stake widget's tied controls." [:a :b :s :share])

(def stake-ranges {:a [1 max-missing 1] :b [1 max-missing 1] :s [1 100 1] :share [0.5 200 0.5]})

(def stake-labels {:a "a, points the first lacks" :b "b, points the second lacks"
                   :s "s, each one's stake" :share "share of the first"})

(defn stake-tie
  "The browser tie of section 1, state [a b s share], one block per source
   (order of `stake-keys`): E = E(a, b) by the triangle's rule; a, b or s
   moved: share = 2 s E; share moved: s = share / (2 E) (clamped to
   [1, 100]), share re-solved."
  [a b s share]
  (let [[sa sb] (triangle a b)
        [_ fwd s' sh'] (split-tie (e/divide sa (e/+ sa sb)) s share)]
    [a b s fwd a b s fwd a b s fwd a b s' sh']))

(defn stake-readout
  "Section 1's numbers for a, b, s: [fraction share-first share-second
   outcomes sum-first sum-second rounds]."
  [a b s]
  (let [[sa sb] (triangle a b)
        [frac s1 s2 total] (split sa sb s)]
    [frac s1 s2 total sa sb (rounds a b)]))

(defn huygens-blocks
  "The browser tie of section 5, state [a b p q x], one block per source
   (order of `huygens-keys`): a, b, p or q moved re-prices x by Prop. III;
   x moved solves q (`huygens-solve`) and re-prices x from it."
  [a b p q x]
  (let [xf (expectation a b p q)
        [q'] (huygens-solve a b p x)
        xs (expectation a b p q')]
    (vec (concat [a b p q xf] [a b p q xf] [a b p q xf] [a b p q xf] [a b p q' xs]))))

;; ---------------------------------------------------------------------------
;; JVM evaluation through raster

(defn- rv [f & xs] (binding [raster/*simplify?* false] (apply raster/value f xs)))

(defn fermat-at
  "Fermat's count on raster, one outcome kernel call per outcome:
   {:n :favourable :total :rows [[letters winner] ...]}."
  [a b]
  (let [n (+ a b -1)
        rows (vec (for [k (range (bit-shift-left 1 n))]
                    (let [o (rv outcome a b k)]
                      [(apply str (for [j (range n)] (if (== 1.0 (o (+ 2 j))) "a" "b"))) (o 0)])))]
    {:n n :favourable (reduce + (map second rows)) :total (count rows) :rows rows}))

(defn triangle-at "The triangle's rule on raster: {:cells :sum-a :sum-b :total}." [a b]
  (let [[sa sb] (rv triangle a b)]
    {:cells (rv base a b) :sum-a sa :sum-b sb :total (+ sa sb)}))

(defn pascal-grid-at
  "Pascal's grid on raster: {[i j] E}, each cell by the mean kernel from its
   two dependencies, in the order of `cells`."
  []
  (reduce (fn [g [i j]]
            (let [E (fn [i j] (cond (zero? i) 1.0 (zero? j) 0.0 :else (g [i j])))]
              (assoc g [i j] (first (rv mean (E (dec i) j) (E i (dec j)))))))
          {} cells))

(defn fraction-at "E(a, b) from the triangle kernel." [a b]
  (let [{:keys [sum-a total]} (triangle-at a b)] (/ sum-a total)))

(defn agreement
  "For every a, b in range: {[a b] [fermat pascal triangle]} fractions, by raster."
  []
  (let [grid (pascal-grid-at)]
    (into (sorted-map)
          (for [a (range 1 (inc max-missing)) b (range 1 (inc max-missing))
                :let [{:keys [favourable total]} (fermat-at a b)
                      {:keys [sum-a] tt :total} (triangle-at a b)]]
            [[a b] [(/ favourable total) (grid [a b]) (/ sum-a tt)]]))))

(def agreement-cache (delay (agreement)))

;; ---------------------------------------------------------------------------
;; Emmy: derivations

(defn lemma-difference
  "Pascal's Corollaire premier and second: the loss A plus half the excess B
   is half the sum of the two cases, A + B/2 - (A + (A + B))/2."
  []
  (e/- (e/+ 'A (e/* 1/2 'B)) (e/* 1/2 (e/+ 'A (e/+ 'A 'B)))))

(defn E1
  "E(1, b) = 1 - 2^-b, written in u = 2^-b (one round earlier, b - 1, is
   2u): Emmy's simplifier then decides the recursion exactly."
  [b u] (e/- 1 u))
(defn E2 "E(2, b) = 1 - (b + 2) / 2^(b + 1), in u = 2^-b." [b u]
  (e/- 1 (e/* 1/2 (e/+ b 2) u)))

(defn recursion-difference-1
  "E(1, b) - (E(0, b) + E(1, b - 1)) / 2 with the closed form: 0 for every b."
  []
  (e/- (E1 'b 'u) (e/* 1/2 (e/+ 1 (E1 (e/- 'b 1) (e/* 2 'u))))))

(defn recursion-difference-2
  "E(2, b) - (E(1, b) + E(2, b - 1)) / 2: 0 for every b."
  []
  (e/- (E2 'b 'u) (e/* 1/2 (e/+ (E1 'b 'u) (E2 (e/- 'b 1) (e/* 2 'u))))))

(defn huygens-prop1
  "Huygens' Prop. I as Emmy solves it: staking x against x where the winner
   pays the loser a, I win 2x - a; set 2x - a = b. Returns [x residual]."
  []
  (let [x (e/* 1/2 (e/+ 'a 'b))]
    [x (e/- (e/- (e/* 2 x) 'a) 'b)]))

(defn huygens-round-trip
  "Price then inverse: chances-for-price at the price of (p, q) minus q."
  []
  (e/- (chances-for-price 'a 'b 'p (expectation 'a 'b 'p 'q)) 'q))

(defn huygens-points
  "The problem of points as an expectation: 2s in the 11 of Fermat's 16
   cases and 0 in 5 (a = 2, b = 3) are worth 11 s / 8."
  []
  (e/- (expectation (e/* 2 's) 0 11 5) (e/* 11/8 's)))

(defn- variant [res] (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails))

(defn graded
  "[{:label :grade}] for the shelf."
  []
  (let [agree @agreement-cache
        exact rationalize
        diffs (mapcat (fn [[f p t]] [(- (exact f) (exact p)) (- (exact p) (exact t))]) (vals agree))
        {:keys [favourable total]} (fermat-at 2 3)
        [_ share] (rv split favourable (- total favourable) 32)
        tie-gaps (for [[[a b] [_ E _]] agree s [1 32 77]
                       :let [[_ share] (rv split-tie E s 0)
                             [_ _ s' _] (rv split-tie E 1 share)]]
                   (- s' s))
        huy-gaps (for [[a b p q] [[3 7 1 1] [10 0 11 5] [64 0 3 1] [5 80 2.5 7]]
                       :let [m (huygens-tie :q {:a a :b b :p p :q q :x 0})
                             q' (:q (huygens-tie :x (assoc m :q 1)))]]
                   (- q' q))]
    [{:label "Pascal's lemma: the sure part plus half the uncertain part is half the sum, A + B/2 = (A + (A + B))/2"
      :grade (variant (grade/grade :symbolic [(lemma-difference)] 0))}
     {:label "E(1, b) = 1 − 2^−b satisfies Pascal's rule E(1, b) = ½(1 + E(1, b − 1)) for every b"
      :grade (variant (grade/grade :symbolic [(recursion-difference-1)] 0))}
     {:label "E(2, b) = 1 − (b + 2)/2^(b+1) satisfies Pascal's rule E(2, b) = ½(E(1, b) + E(2, b − 1)) for every b"
      :grade (variant (grade/grade :symbolic [(recursion-difference-2)] 0))}
     {:label (str "Fermat's enumeration, Pascal's recursion and the arithmetic triangle give the same split in all "
                  (count agree) " games with a, b ≤ " max-missing " (exhaustive check, every number from raster, compared exactly)")
      :grade (variant (grade/grade :symbolic diffs 0))}
     {:label "Pascal's example of 24 Aug. 1654 (2 points against 3, 32 pistoles each): 11 outcomes of 16, 44 pistoles of 64 (raster)"
      :grade (variant (grade/grade :symbolic [(- (exact favourable) 11) (- (exact total) 16) (- (exact share) 44)] 0))}
     {:label "the stake tie: the share from s, then s from the share, is the identity in every game in range (raster)"
      :grade (variant (grade/grade :numeric tie-gaps 1e-9))}
     {:label "Huygens, Prop. I: x = (a + b)/2 solves 2x − a = b"
      :grade (variant (grade/grade :symbolic [(second (huygens-prop1))] 0))}
     {:label "Huygens, Prop. III and its inverse: q(x(p, q)) = q (Emmy, symbolic)"
      :grade (variant (grade/grade :symbolic [(huygens-round-trip)] 0))}
     {:label "the Huygens tie: price from q, then q from the price, is the identity (raster, four cases)"
      :grade (variant (grade/grade :numeric huy-gaps 1e-6))}
     {:label "the problem of points as an expectation: 11 cases worth 2s and 5 worth nothing are worth 11s/8 (Huygens, Prop. III)"
      :grade (variant (grade/grade :symbolic [(huygens-points)] 0))}]))
