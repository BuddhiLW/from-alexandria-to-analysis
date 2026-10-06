(ns alexandria.dedekind.cuts
  "Dedekind, Stetigkeit und irrationale Zahlen (1872): the cut of sqrt D in
   the rationals, in Dedekind's own arithmetic (Beman 1901, Sections IV-V).

     cut         the classes A1, A2 of sqrt D as predicates on exact rationals
     descent     Dedekind's proof that no rational squares to D: the pair
                 (t, u) goes to (D u - l t, t - l u), u strictly smaller
     y-map       y = x (x^2 + 3D) / (3x^2 + D), which beats every candidate
                 for a greatest member of A1 or a least of A2
     eudoxus     Elements V Def. 5 on diagonal : side, the same sorting of
                 the rationals 2200 years earlier
     cut of cuts the union of the lower classes of an increasing family of
                 cuts is again the lower class of a cut (Section V, iv)
     figures     the rationals on the line, zoomed about sqrt 2
     identities  Dedekind's three equalities, graded by Emmy

   Reuse searched: Emmy, desargues and alexandria have no cut or Dedekind
   type (carto search cut/dedekind/real over each: none). set-theory.eudoxus
   (the Eudoxus V branch notebook) computes the same equimultiple test, but
   alexandria cannot depend on a consumer, so the test lives here and that
   branch can import it. Clojure's exact ratios are the rationals; Emmy
   grades the identities."
  (:require [alexandria.grade :as grade]
            [alexandria.proofs :as proofs]
            [emmy.env :as e]
            [hive-dsl.result :as r]))

;; ---------------------------------------------------------------------------
;; The cut of sqrt D (Section IV)

(defn A2?
  "r is in the upper class of sqrt D: a positive rational whose square
   exceeds D."
  [D r]
  (and (pos? r) (> (* r r) D)))

(defn A1?
  "r is in the lower class of sqrt D: every rational not in A2, that is
   r <= 0 or r^2 < D (equality never happens, see `descent`)."
  [D r]
  (not (A2? D r)))

(defn lambda-of
  "Dedekind's lambda: the integer with lambda^2 < D < (lambda + 1)^2."
  [D]
  (long (Math/floor (Math/sqrt (double D)))))

(defn descent-step
  "Dedekind's step on a pair (t, u): (D u - lambda t, t - lambda u). It
   multiplies t^2 - D u^2 by lambda^2 - D."
  [D [t u]]
  (let [l (lambda-of D)]
    [(- (* D u) (* l t)) (- t (* l u))]))

(defn pell-value [D [t u]] (- (* t t) (* D u u)))

(defn descent
  "The pairs descent-step makes from (t, u) while u stays positive. On a
   pair with t^2 - D u^2 = 0 the u would fall forever, which positive
   integers cannot do: Dedekind's contradiction. Shown on the near misses
   t^2 - 2u^2 = +-1 (99/70, 41/29, ...)."
  [D tu]
  (->> (iterate (partial descent-step D) tu)
       (take-while (fn [[_ u]] (pos? u)))
       vec))

(defn y-map
  "Dedekind's y = x (x^2 + 3D) / (3x^2 + D)."
  [D x]
  (/ (* x (+ (* x x) (* 3 D))) (+ (* 3 x x) D)))

(defn beaten-by-y
  "[x y] pairs: from x, the y-map gives a rational of the same class nearer
   to sqrt D, n times."
  [D x n]
  (mapv vec (take n (partition 2 1 (iterate (partial y-map D) x)))))

;; ---------------------------------------------------------------------------
;; Eudoxus' pattern: Elements V Def. 5 on the diagonal and side

(defn eudoxus-verdict
  "Compare n copies of the diagonal with m copies of the side of a square
   (Elements V Def. 5), exactly: n d > m s iff 2 n^2 > m^2, as d^2 = 2 s^2.
   :exceeds, :equal or :falls-short."
  [m n]
  (let [lhs (* 2 n n) rhs (* m m)]
    (cond (> lhs rhs) :exceeds (= lhs rhs) :equal :else :falls-short)))

(defn eudoxus-is-the-cut?
  "For every positive m/n with m, n <= k: Eudoxus' verdict 'n diagonals
   exceed m sides' holds exactly when m/n is in Dedekind's A1 for D = 2,
   and 'equal' never happens."
  [k]
  (every? (fn [[m n]]
            (let [v (eudoxus-verdict m n)]
              (and (not= :equal v)
                   (= (= :exceeds v) (A1? 2 (/ m n))))))
          (for [m (range 1 (inc k)) n (range 1 (inc k))] [m n])))

;; ---------------------------------------------------------------------------
;; Section V, iv: a cut of cuts is produced by a cut

(defn rationals-in
  "The rationals m/n in [lo, hi] with 1 <= n <= max-n, distinct, sorted."
  [lo hi max-n]
  (->> (for [n (range 1 (inc max-n))
             m (range (long (Math/ceil (* lo n))) (inc (long (Math/floor (* hi n)))))]
         (/ m n))
       distinct sort vec))

(defn union-of-cuts
  "The lower class of the union of the cuts produced by the rationals as
   (every r <= some a in as), as a predicate."
  [as]
  (fn [r] (boolean (some #(<= r %) as))))

(defn cut-of-cuts?
  "The cuts produced by the y-map approximants of sqrt 2 below it (1, 7/5,
   1393/985, ...): on every rational in [0, 2] with denominator <= max-n the
   union of their lower classes agrees with A1 of sqrt 2. The union is the
   lower class of one cut, and it is the cut of sqrt 2."
  [max-n]
  (let [as (vec (take 4 (iterate (partial y-map 2) 1)))
        below? (union-of-cuts as)]
    (every? (fn [r] (= (below? r) (A1? 2 r))) (rationals-in 0 2 max-n))))

;; ---------------------------------------------------------------------------
;; Figures

(defn- step
  "1 when x > 0, 0 when x < 0, branch-free."
  [x]
  (e// (e/+ 1 (e// x (e/+ (e/abs x) 1e-12))) 2))

(defn on-the-line
  "The rational m/n (state [m n]) on the line magnified by zoom about the
   centre c: A1 lifted above the line, A2 dropped below it, by lift."
  [c zoom lift]
  (fn [[m n]]
    (let [side (e/- (e/* 2 (step (e/- (e/* 2 n n) (e/* m m)))) 1)]
      [(e/* zoom (e/- (e// m n) c)) (e/* lift side)])))

(def figures
  "Every moving figure, by name: {:f :params :state}."
  {:line {:f on-the-line :params [1.4142135623730951 1 0.08] :state [1 1]}})

;; ---------------------------------------------------------------------------
;; Identities, graded

(def identities
  "Dedekind's equalities (Section IV) as [label difference] pairs, each zero."
  (let [x 'x D 'D l 'lambda t 't u 'u
        den (e/+ (e/* 3 (e/square x)) D)
        y (e// (e/* x (e/+ (e/square x) (e/* 3 D))) den)]
    [["y - x = 2x(D - x^2) / (3x^2 + D)"
      (e/- (e/- y x) (e// (e/* 2 x (e/- D (e/square x))) den))]
     ["y^2 - D = (x^2 - D)^3 / (3x^2 + D)^2"
      (e/- (e/- (e/square y) D) (e// (e/expt (e/- (e/square x) D) 3) (e/square den)))]
     ["t'^2 - D u'^2 = (lambda^2 - D)(t^2 - D u^2), with t' = D u - lambda t, u' = t - lambda u"
      (e/- (e/- (e/square (e/- (e/* D u) (e/* l t))) (e/* D (e/square (e/- t (e/* l u)))))
           (e/* (e/- (e/square l) D) (e/- (e/square t) (e/* D (e/square u)))))]]))

(defn- variant [res] (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails))

(defn graded
  "[{:label :grade}]: the identities by Emmy, then the exact finite checks
   (decided in exact rationals, so proved or failed, never approximate)."
  []
  (into (mapv (fn [[label diff]] {:label label :grade (variant (grade/grade :symbolic [diff]))})
              identities)
        (map (fn [[label ok?]] {:label label :grade (if ok? :grade/proved :grade/fails)}))
        [["Elements V Def. 5 sorts every m/n (m, n <= 60) exactly as the cut of sqrt 2"
          (eudoxus-is-the-cut? 60)]
         ["the descent from 99/70: 99/70, 41/29, 17/12, 7/5, 3/2, 1/1, u falling, t^2 - 2u^2 = +-1 throughout"
          (let [ds (descent 2 [99 70])]
            (and (= [[99 70] [41 29] [17 12] [7 5] [3 2] [1 1]] ds)
                 (every? #(= 1 (abs (pell-value 2 %))) ds)))]
         ["from 1 in A1 the y-map climbs 1 < 7/5 < 1393/985 < ..., each still in A1"
          (every? (fn [[a b]] (and (< a b) (A1? 2 b))) (beaten-by-y 2 1 3))]
         ["from 3/2 in A2 the y-map falls 3/2 > 99/70 > ..., each still in A2"
          (every? (fn [[a b]] (and (> a b) (A2? 2 b))) (beaten-by-y 2 3/2 3))]
         ["the union of the cuts of 1, 7/5, 1393/985, ... is the cut of sqrt 2 on every m/n in [0, 2], n <= 40"
          (cut-of-cuts? 40)]]))

;; ---------------------------------------------------------------------------
;; The proof as data

(def proofs-resource "alexandria/dedekind/stetigkeit.proofs.edn")

(defn proof [] (proofs/read-proofs proofs-resource))
