(ns alexandria.delian.philo
  "Philo of Byzantium's catapult rule (Belopoeica 51.23-29, Schöne 1893),
   derived by Emmy and graded.

     rule     a stone-thrower for a shot of W minae: reduce W to drachmae
              (100 W), take the cube side, add a tenth; that many dactyls
              is the diameter of the spring hole, the calibre:
                d = (11/10) (100 W)^(1/3).
              Ten minae give 1000 drachmae, cube side 10, calibre 11.
     scaling  for a shot k times heavier the calibre ratio is the cube
              root of k: d2/d1 = k^(1/3). Emmy divides the rule at k W by
              the rule at W and simplifies.
     means    Philo builds d2 from d1 by two mean proportionals: d1 : d2 =
              d2 : x = x : k d1. Emmy solves the first proportion for x
              (linear), the second for k d1 (linear), and is left with
              k = (d2/d1)^3: the Delian problem with ratio k.
     mesolabe Eratosthenes' frames give it with AE = k DΘ: the slide is
              s = k^(-1/3), and GH / DΘ = 1/s = d2/d1.

   Tie {k r}, r = d2/d1: r = k^(1/3), k = r^3; `grades` proves every step
   and both round trips :grade/proved."
  (:require [alexandria.delian.tie :as t]
            [alexandria.grade :as grade]
            [emmy.env :as e]
            [hive-dsl.result :as r]))

;; ---------------------------------------------------------------------------
;; The rule and the tie

(def tenth "1 + 1/10, the tenth Philo adds to the cube side." (e// 11 10))

(defn calibre
  "Philo's rule: the spring-hole diameter in dactyls for a shot of w minae,
   (11/10) (100 w)^(1/3)."
  [w] (e/* tenth (t/cbrt (e/* 100 w))))

(defn r-of-k "The calibre ratio for a shot k times heavier: k^(1/3)." [k] (t/cbrt k))
(defn k-of-r "The weight ratio for a calibre ratio r: r^3." [r] (e/cube r))
(defn s-of-r "The mesolabe's slide with AE = k DΘ: s = 1/r." [r] (e// 1 r))
(defn r-of-s "The calibre ratio GH/DΘ from the slide: 1/s." [s] (e// 1 s))

;; ---------------------------------------------------------------------------
;; Derivation

(def derivation
  "Every step as Emmy expressions, on symbols W k d1 d2 (all positive)."
  (delay
    (let [ratio (e/simplify (e// (calibre (e/* 'k 'W)) (calibre 'W)))
          x (e/solve-linear 'd1 (e/square 'd2))
          kd1 (e/solve-linear 'd2 (e/square x))
          k (e/simplify (e// kd1 'd1))]
      {:ratio ratio :x x :kd1 (e/simplify kd1) :k k})))

;; ---------------------------------------------------------------------------
;; Grades

(defn- variant [res] (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails))
(defn- proved [label diff] {:label label :grade (variant (grade/grade :symbolic [diff]))})
(defn- pos "A positive quantity exp(u) (see alexandria.delian.tie)." [u] (e/exp u))

(defn grades
  "[{:label :grade}] for Philo's rule, the two means and the tie."
  []
  (let [d @derivation
        k (pos 'u) rr (pos 'v) w (pos 'w) d1 (pos 'p) s (pos 'z)]
    [(proved "ten minae: 1000 drachmae, cube side 10, calibre 11 dactyls"
             (e/- (calibre 10) 11))
     (proved "the rule at k W over the rule at W is k^(1/3)"
             (e/- (e// (calibre (e/* k w)) (calibre w)) (r-of-k k)))
     (proved "d1 : d2 = d2 : x gives x = d2²/d1 (solve-linear)"
             (e/- (e/* 'd1 (:x d)) (e/square 'd2)))
     (proved "d2 : x = x : k d1 gives k d1 = d2³/d1² (solve-linear)"
             (e/- (e/* 'd2 (:kd1 d)) (e/square (:x d))))
     (proved "so k = (d2/d1)³: two means between d1 and k d1 multiply the cube by k"
             (e/- (:k d) (e/cube (e// 'd2 'd1))))
     (proved "k → r → k is the identity" (e/- (k-of-r (r-of-k k)) k))
     (proved "r → k → r is the identity" (e/- (r-of-k (k-of-r rr)) rr))
     (proved "the mesolabe with AE = k DΘ slides to s = k^(-1/3): DΘ = AE s³"
             (e/- (t/b-of-s (e/* k d1) (s-of-r (r-of-k k))) d1))
     (proved "its second mean GH is the new calibre d1 k^(1/3)"
             (e/- (t/mean-2 (e/* k d1) (s-of-r (r-of-k k))) (e/* d1 (r-of-k k))))
     (proved "s → r → s is the identity" (e/- (s-of-r (r-of-s s)) s))]))

;; ---------------------------------------------------------------------------
;; TeX for the page

(defn- eq [lhs rhs] (e/literal-number (list '= (e/freeze lhs) (e/freeze rhs))))

(defn equations
  "[{:label :eqs}]: the derivation as Emmy equations."
  []
  (let [d @derivation]
    [{:label "Philo's rule: W minae are 100 W drachmae; the calibre d in dactyls"
      :eqs [(eq 'd (list '* (list '/ 11 10) (list 'expt (list '* 100 'W) (list '/ 1 3))))]}
     {:label "a shot k times heavier: the rule at kW over the rule at W, simplified by Emmy"
      :eqs [(eq (list '/ 'd2 'd1) (:ratio d))]}
     {:label "two means between d1 and k d1: d1 : d2 = d2 : x is linear in x"
      :eqs [(eq 'x (:x d))]}
     {:label "and d2 : x = x : k d1 is linear in k d1"
      :eqs [(eq (list '* 'k 'd1) (:kd1 d)) (eq 'k (:k d))]}]))
