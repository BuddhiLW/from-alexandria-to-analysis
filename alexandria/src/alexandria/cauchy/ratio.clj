(ns alexandria.cauchy.ratio
  "Cours d'analyse 1821, ch. VI sec. 2, Theorems I and II side by side: the
   root (u_n)^(1/n) and the ratio u_(n+1)/u_n of a series of positive
   terms, and Cauchy's U.

   Five series the reader can pick, each by the Emmy expression of
   log u_n (`series`). Every number of the table (`table`) comes out of a
   raster kernel: u_n, the root and the ratio as exp of raster logs;
   log n! as raster loop sums of log k. The ratio's closed form is Emmy's
   (`ratio-expr`, simplified), and its limit k is graded against the
   raster value at large n.

   Cauchy's argument ties two numbers. For k < 1 choose U between k and 1;
   then from some N on every root is below U (u_n < U^n). Forward: U ->
   N, the first n from which the roots stay below U. Inverse: N -> U, the
   greatest root from N on (the smallest U that works from N): Cauchy's
   'limit of the greatest values' read at N. For k > 1 the same with 1 < U
   < k and the roots above U. Both maps read the raster table, which runs
   to n = `horizon`; forward then inverse lands on the greatest root from
   N, at or below the U chosen, and inverse then forward returns N or an
   earlier n (`grades`)."
  (:require [alexandria.grade :as grade]
            [alexandria.raster :as raster]
            [alexandria.raster.series :as rs]
            [emmy.env :as e]
            [hive-dsl.result :as r]))

(def horizon "The table runs to n = horizon (the tie reads it)." 400)
(def shown "The slider of n runs to this n." 60)

(def series
  "id -> {:label :tex :poly :geo :log :k :cauchy?}: u_n = poly(n) geo^n
   (Emmy), except Cauchy's 1/n!; :log (fn [n lf] log u_n), lf = log n!;
   :k the common limit of root and ratio."
  {:e {:label "1/n!" :tex "u_n = \\frac{1}{n!}" :k 0
       :log (fn [_ lf] (e/- lf)) :cauchy? true}
   :quad {:label "n²/2ⁿ" :tex "u_n = \\frac{n^2}{2^n}" :k 1/2
          :poly e/square :geo 1/2
          :log (fn [n _] (e/- (e/* 2 (e/log n)) (e/* n (e/log 2))))}
   :basel {:label "1/n²" :tex "u_n = \\frac{1}{n^2}" :k 1
           :poly (fn [n] (e/divide 1 (e/square n))) :geo 1 :log (fn [n _] (e/* -2 (e/log n)))}
   :harmonic {:label "1/n" :tex "u_n = \\frac{1}{n}" :k 1
              :poly (fn [n] (e/divide 1 n)) :geo 1 :log (fn [n _] (e/- (e/log n)))}
   :growth {:label "2ⁿ/n²" :tex "u_n = \\frac{2^n}{n^2}" :k 2
            :poly (fn [n] (e/divide 1 (e/square n))) :geo 2
            :log (fn [n _] (e/- (e/* n (e/log 2)) (e/* 2 (e/log n))))}})

(def order [:e :quad :basel :harmonic :growth])

(def sums-known
  "The sum where it is known in closed form (the readout compares)."
  {:e Math/E :quad 6.0 :basel (/ (* Math/PI Math/PI) 6)})

(defn verdict
  "What Theorem I (or II) says for k."
  [k]
  (cond (< k 1) :converges (> k 1) :diverges :else :open))

;; ---------------------------------------------------------------------------
;; Emmy: the ratio in closed form

(defn ratio-expr
  "u_(n+1)/u_n for series id at n (a symbol or a number), simplified by
   Emmy: poly(n+1)/poly(n) times geo, since geo^(n+1)/geo^n = geo. For
   1/n!, Cauchy's line 1.2.3...n / 1.2.3...n(n+1) = 1/(n+1)."
  [id n]
  (if (= id :e)
    (e/divide 1 (e/+ n 1))
    (let [{:keys [poly geo]} (series id)]
      (e/simplify (e/* geo (e/divide (poly (e/+ n 1)) (poly n)))))))

;; ---------------------------------------------------------------------------
;; The table, by raster

(def ^:private log-factorials
  (delay
    (let [ls (raster/sample e/log (mapv vector (range 1 (+ horizon 2))))]
      (vec (reductions + 0.0 ls)))))

(defn log-factorial "log n! (raster logs, summed)." [n] (nth @log-factorials n))

(defn- log-u
  "log u_n for series id at n: one raster kernel of the Emmy expression."
  [id n]
  (raster/value (fn [n lf] ((:log (series id)) n lf)) n (log-factorial n)))

(defn table
  "[{:n :u :root :ratio :s}] for n = 1 .. horizon: u_n, (u_n)^(1/n),
   u_(n+1)/u_n and the partial sum s_n = u_1 + ... + u_n (1/n! from n = 0:
   s_n = 1 + u_1 + ... + u_n). Every u, root and ratio is exp of a raster
   log."
  [id]
  (let [ls (mapv #(log-u id %) (range 1 (+ horizon 2)))
        ex (fn [x] (raster/value e/exp x))
        us (mapv ex ls)
        s0 (if (= id :e) 1.0 0.0)]
    (mapv (fn [i]
            (let [n (inc i)]
              {:n n :u (nth us i)
               :root (ex (/ (nth ls i) n))
               :ratio (ex (- (nth ls (inc i)) (nth ls i)))}))
          (range horizon))))

(defn with-sums [rows id]
  (let [s0 (if (= id :e) 1.0 0.0)]
    (mapv #(assoc %1 :s %2) rows (rest (reductions + s0 (map :u rows))))))

(def tables
  "Every series' table, with partial sums, computed once."
  (delay (into {} (for [id order] [id (with-sums (table id) id)]))))

;; ---------------------------------------------------------------------------
;; Cauchy's U and N, tied

(defn side
  "+1 when k < 1 (roots end below U), -1 when k > 1 (roots end above U),
   nil when k = 1 (no U)."
  [k]
  (case (verdict k) :converges 1 :diverges -1 nil))

(defn greatest-from
  "Inverse map: the greatest root from n = N on (the least, for k > 1):
   the U that just works from N."
  [rows k N]
  (let [rs (map :root (drop (dec N) rows))]
    (if (= 1 (side k)) (reduce max rs) (reduce min rs))))

(defn first-from
  "Forward map: the first N from which every root of the table is at or
   below U (at or above U for k > 1); nil when none. At a U strictly
   between k and 1 this is Cauchy's (u_n)^(1/n) < U; at the U of the
   inverse map it returns N itself or an earlier n."
  [rows k U]
  (let [ok? (if (= 1 (side k)) #(<= % U) #(>= % U))
        roots (mapv :root rows)]
    (loop [n (count roots) found nil]
      (if (or (zero? n) (not (ok? (nth roots (dec n)))))
        found
        (recur (dec n) n)))))

(defn u-range
  "The U slider's range: (k, 1) or (1, k), kept off both ends."
  [k]
  (case (side k) 1 [(+ (double k) 0.02) 0.98] -1 [1.02 (- (double k) 0.02)] nil))

;; ---------------------------------------------------------------------------
;; Grades

(defn- variant [res] (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails))
(defn- proved [label diff] {:label label :grade (variant (grade/grade :symbolic [diff]))})
(defn- numeric [label diff tol]
  {:label label :grade (variant (grade/grade :numeric [diff] tol)) :source raster/source})
(defn- decided [label ok?] {:label label :grade (if ok? :grade/proved :grade/fails)})

(defn grades
  "Theorems I and II on the five series: Emmy's ratios in closed form
   (proved), their values at n = 10^6 by raster against k, the root's
   limit from the raster table, the partial sums, and the U <-> N tie."
  []
  (let [t @tables
        closed {:quad (e/divide (e/square (e/+ 'n 1)) (e/* 2 (e/square 'n)))
                :basel (e/divide (e/square 'n) (e/square (e/+ 'n 1)))
                :harmonic (e/divide 'n (e/+ 'n 1))
                :growth (e/divide (e/* 2 (e/square 'n)) (e/square (e/+ 'n 1)))}
        big 1e6]
    (vec
     (concat
      [(proved "Cauchy's example 1/n!: u(n+1)/u(n) = 1.2.3...n / 1.2.3...n(n+1) = 1/(n+1), checked at n = 9"
               (e/- (e/divide (e/divide 1 (e/factorial 10)) (e/divide 1 (e/factorial 9))) (ratio-expr :e 9)))]
      (for [[id c] closed]
        (proved (str (get-in series [id :label]) ": Emmy's ratio u(n+1)/u(n) is " (pr-str (e/freeze c)))
                (e/- (ratio-expr id 'n) c)))
      (for [id order]
        (numeric (str (get-in series [id :label]) ": the ratio at n = 10^6 (raster kernel of Emmy's ratio) is within 1e-5 of k = " (get-in series [id :k]))
                 (- (raster/value (fn [n] (ratio-expr id n)) big) (double (get-in series [id :k]))) 1e-5))
      [(numeric "n²/2ⁿ: the root (u_n)^(1/n) from the raster table at n = 400 is within 0.02 of k = 1/2: root and ratio share their limit"
                (- (:root (peek (:quad t))) 0.5) 0.02)
       (numeric "1/n!: the partial sums of the table reach e (raster exp)"
                (- (:s (nth (:e t) 30)) (raster/value e/exp 1)) 1e-12)
       (numeric "n²/2ⁿ: the partial sums reach 6"
                (- (:s (nth (:quad t) 200)) 6.0) 1e-9)]
      (for [[id U] [[:quad 3/4] [:e 1/2] [:growth 3/2]]
            :let [rows (t id) k (get-in series [id :k])
                  N (first-from rows k U)
                  U' (greatest-from rows k N)]]
        (decided (str (get-in series [id :label]) ": U = " U " gives N = " N "; the extreme root from N on is "
                      (format "%.4f" U') ", on the right side of U, and gives back N")
                 (and N (if (= 1 (side k)) (< U' U) (> U' U)) (<= (first-from rows k U') N))))))))
