(ns alexandria.eudoxus.proportion
  "Eudoxus' theory of proportion (Euclid, Elements V, Defs. 4-7) and his
   method of exhaustion (Elements XII.2). Ported from set-theory.eudoxus.

     equimultiple-order  how m copies of A compare with n copies of B
     same-ratio?         Def. 5 over a finite set of multipliers, exact
     greater-ratio?      Def. 7: the witness m, n, or nil
     diagonal-cut-side   where m/n falls against diagonal : side (2 n^2 vs m^2)
     cut-sample          the rationals m/n, n <= limit, on either side
     descent             the parity descent for sqrt 2, as Emmy identities
     polygon-area        the inscribed regular 2^k-gon of XII.2, an Emmy
                         function; its numbers come from raster (media)
     graded              the identities, graded by alexandria.grade"
  (:require [alexandria.grade :as grade]
            [alexandria.proofs :as proofs]
            [emmy.env :as e]
            [hive-dsl.result :as r]
            [alexandria.raster :as raster]))

(defn sign
  "Which way x compares with y: :less, :equal or :greater."
  [x y]
  (cond (< x y) :less (> x y) :greater :else :equal))

(defn equimultiple-order
  "m A against n B."
  [m A n B]
  (sign (* m A) (* n B)))

(defn same-ratio?
  "Def. 5 over the multiplier pairs [[m n] ...]: A : B = C : D when m A and
   m C alike exceed, equal or fall short of n B and n D."
  [A B C D multipliers]
  (every? (fn [[m n]] (= (equimultiple-order m A n B) (equimultiple-order m C n D)))
          multipliers))

(defn greater-ratio?
  "Def. 7: {:m :n} with m A greater than n B while m C is not greater than
   n D, or nil."
  [A B C D multipliers]
  (some (fn [[m n]]
          (when (and (= :greater (equimultiple-order m A n B))
                     (not= :greater (equimultiple-order m C n D)))
            {:m m :n n}))
        multipliers))

(defn diagonal-cut-side
  "Where m/n falls against diagonal : side of a square, decided exactly:
   n diagonal > m side exactly when 2 n^2 > m^2 (I.47 squares both).
   :lower, :upper, or :equal (which no whole numbers reach)."
  [m n]
  (case (sign (* 2 n n) (* m m)) :greater :lower :less :upper :equal :equal))

(defn cut-sample
  "Every rational m/n in lowest terms with 1 <= n <= limit and 0 < m/n < 2,
   sorted, each with its :side."
  [limit]
  (->> (for [n (range 1 (inc limit)) m (range 1 (* 2 n)) :when (= 1 (.gcd (biginteger m) (biginteger n)))]
         {:m m :n n :side (diagonal-cut-side m n)})
       (sort-by (fn [{:keys [m n]}] (/ m n)))
       vec))

(defn pairs
  "Multiplier pairs [m n], 1 <= m, n <= k."
  [k]
  (for [m (range 1 (inc k)) n (range 1 (inc k))] [m n]))

(def descent
  "The parity descent for the side and diagonal, [label difference] pairs,
   each zero, symbolic in r and q. If p^2 = 2 q^2 then p is even, p = 2 r;
   then q^2 = 2 r^2, so q is even too, and p/q was not in lowest terms."
  (let [r 'r q 'q k 'k]
    [["p = 2r: (2r)^2 = 2 q^2 is q^2 = 2 r^2"
      (e/- (e/- (e/square (e/* 2 r)) (e/* 2 (e/square q))) (e/* -2 (e/- (e/square q) (e/* 2 (e/square r)))))]
     ["an odd square is odd: (2k + 1)^2 = 2 (2k^2 + 2k) + 1"
      (e/- (e/square (e/+ (e/* 2 k) 1)) (e/+ (e/* 2 (e/+ (e/* 2 (e/square k)) (e/* 2 k))) 1))]
     ["I.47 on the square: diagonal^2 = side^2 + side^2 = 2 side^2"
      (e/- (e/+ (e/square 's) (e/square 's)) (e/* 2 (e/square 's)))]]))

(defn polygon-area
  "Area of the regular polygon of 2^k sides inscribed in the circle of
   radius r (Elements XII.2): 2^(k-1) r^2 sin(2 pi / 2^k)."
  [k r]
  (e/* (e/expt 2 (e/- k 1)) (e/square r) (e/sin (e// (e/* 2 'pi) (e/expt 2 k)))))

(def exhaustion
  "XII.1-2 as identities: doubling the sides from 2^k to 2^(k+1) adds
   2^k triangles, each half its rectangle; and the area scales with the
   square on the diameter."
  (let [k 'k r 'r d 'd t (e// 'pi (e/expt 2 'k))]
    [["the 2^(k+1)-gon's area, written by the half angle: 2^k r^2 sin(pi/2^(k-1)) / 2 = 2^k r^2 sin(t) cos(t), t = pi/2^k"
      (e/- (e// (e/* (e/expt 2 k) (e/square r) (e/* 2 (e/sin t) (e/cos t))) 2)
           (e/* (e/expt 2 k) (e/square r) (e/sin t) (e/cos t)))]
     ["XII.1: polygons on diameters d and 2d: area scales as d^2"
      (e/- (e// (polygon-area k (e/* 2 d)) (polygon-area k d)) 4)]]))

(defn doubling
  "Vertex m (0 <= m < 2^(k+1)) of the inscribed polygon of radius r while
   the 2^k-gon becomes the 2^(k+1)-gon (XII.2): even m are the old
   vertices, odd m rise from the middle of a side (t = 0) to the middle of
   its arc (t = 1). An Emmy function, compiled to a raster kernel by media."
  [k t r]
  (fn [[m]]
    (let [n (e/expt 2 k)
          odd (e// (e/- 1 (e/cos (e/* Math/PI m))) 2)
          rho (e/* r (e/+ (e/- 1 odd) (e/* odd (e/+ (e/* (e/- 1 t) (e/cos (e// Math/PI n))) t))))
          theta (e// (e/* Math/PI m) n)]
      [(e/* rho (e/cos theta)) (e/* rho (e/sin theta))])))

(def figures
  "{name {:f :params :state}} for alexandria.medium.kernel."
  {:doubling {:f doubling :params [2 0 1] :state [0]}})

(defn circle-share
  "The share of the circle the inscribed n-gon covers, (n/2) sin(2 pi / n) / pi,
   an Emmy function of the number of sides n."
  [n]
  (e// (e/* (e// n 2) (e/sin (e// (* 2 Math/PI) n))) Math/PI))

(defn circle-shares
  "{k share} for the 2^k-gons, k = 2..kmax, every share by alexandria.raster."
  [kmax]
  (zipmap (range 2 (inc kmax))
          (raster/sample circle-share (mapv (fn [k] [(bit-shift-left 1 k)]) (range 2 (inc kmax))))))

(defn graded
  "[{:label :grade}] of the descent and exhaustion identities, and the
   exact Def. 5 and Def. 7 examples."
  []
  (let [g (fn [diff] (let [res (grade/grade :symbolic [diff])]
                       (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails)))
        exact (fn [label ok?] {:label label :grade (if ok? :grade/proved :grade/fails)})]
    (-> (mapv (fn [[label diff]] {:label label :grade (g diff)}) (concat descent exhaustion))
        (conj (exact "Def. 5: 2 : 3 and 4 : 6 order every equimultiple alike (m, n <= 12)"
                     (same-ratio? 2 3 4 6 (pairs 12)))
              (exact "Def. 7: 3 : 2 is greater than 4 : 3 (a witness m, n)"
                     (boolean (greater-ratio? 3 2 4 3 (pairs 12))))
              (exact "no m/n with n <= 60 lands on diagonal : side"
                     (not-any? #(= :equal (:side %)) (cut-sample 60)))))))

(def proofs-resource "alexandria/eudoxus/elements_5.proofs.edn")

(defn proof [] (proofs/read-proofs proofs-resource))
