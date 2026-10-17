(ns alexandria.structure.lagrangian-test
  (:require [alexandria.structure.lagrangian :as lg]
            [alexandria.structure.lagrangian-contract :as c :refer [deflagrangian-contract]]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [emmy.env :as e]))

;; ---------------------------------------------------------------------------
;; Instances of the contract

(def coupled
  "Two masses on springs, coupled by c x y: a 2-degree-of-freedom L."
  (fn [[_ [x y] [vx vy]]]
    (e/- (e/* 1/2 'm (e/+ (e/square vx) (e/square vy)))
         (e/+ (e/* 1/2 'k (e/+ (e/square x) (e/square y))) (e/* 'c x y)))))

(def particle-in-field
  "A particle in the plane in a potential V(x, y), V a literal function."
  (lg/T-minus-V (fn [[_ _ v]] (e/* 1/2 'm (e/square v)))
                (fn [[_ [x y] _]] ((e/literal-function 'V '(-> (X Real Real) Real)) x y))))

(deflagrangian-contract free-particle {:L (lg/free-particle 'm) :n 1})
(deflagrangian-contract harmonic {:L (lg/harmonic 'm 'k) :n 1})
(deflagrangian-contract coupled-oscillators {:L coupled :n 2})
(deflagrangian-contract field {:L particle-in-field :n 2})

;; ---------------------------------------------------------------------------
;; The named results the wave brief asks for

(deftest euler-lagrange-of-the-free-particle
  (testing "m q'' = 0"
    (e/with-literal-functions [q]
      (is (c/proved? [(e/- ((lg/euler-lagrange (lg/free-particle 'm) q) 't)
                           (e/* 'm (((e/expt e/D 2) q) 't)))])))))

(deftest euler-lagrange-of-the-harmonic-oscillator
  (testing "m q'' + k q = 0, and cos(sqrt(k/m) t) solves it"
    (e/with-literal-functions [q]
      (is (c/proved? [(e/- ((lg/euler-lagrange (lg/harmonic 'm 'k) q) 't)
                           (e/+ (e/* 'm (((e/expt e/D 2) q) 't)) (e/* 'k (q 't))))])))
    (is (c/proved? [((lg/euler-lagrange (lg/harmonic 'm 'k)
                                        (fn [t] (e/* 'A (e/cos (e/* (e/sqrt (e// 'k 'm)) t)))))
                     't)]))))

(deftest hamilton-agrees-with-lagrange-in-two-degrees
  (testing "the Hamiltonian of the coupled oscillators, and its equations along (q, p = m q')"
    (is (c/proved? [(e/- ((lg/legendre coupled) (lg/H-state 2))
                         (e/+ (e// (e/+ (e/square 'p_0) (e/square 'p_1)) (e/* 2 'm))
                              (e/* 1/2 'k (e/+ (e/square 'q_0) (e/square 'q_1)))
                              (e/* 'c 'q_0 'q_1)))]))))

(deftest a-time-dependent-L-does-not-conserve-energy
  (testing "L = m v^2/2 - k(t) q^2/2: the energy rate is -dL/dt, not 0"
    (let [Lt (fn [[t q v]] (e/- (e/* 1/2 'm (e/square v)) (e/* 1/2 ((e/literal-function 'k) t) (e/square q))))]
      (e/with-literal-functions [q]
        (is (not (c/proved? [((lg/energy-rate Lt q) 't)])))
        (is (c/proved? [(e/+ ((lg/energy-rate Lt q) 't)
                             (e/* -1/2 ((e/D (e/literal-function 'k)) 't) (e/square (q 't))))]))))))

(deftest cyclic-coordinates
  (testing "the angle of a central force is ignorable, the radius is not"
    (let [Lc (lg/central-polar 'm (e/literal-function 'U))]
      (is (lg/cyclic? Lc 2 1))
      (is (not (lg/cyclic? Lc 2 0))))))

(deftest poisson-brackets
  (testing "{q, p} = 1 and {H, H} = 0"
    (is (= 1 (e/simplify ((lg/poisson lg/coordinate lg/conjugate) (lg/H-state 1)))))
    (let [Hh (lg/legendre (lg/harmonic 'm 'k))]
      (is (c/proved? [((lg/poisson Hh Hh) (lg/H-state 1))])))))

(deftest a-point-transformation-keeps-the-equations
  (testing "the free particle in polar coordinates: the angle is cyclic"
    (let [polar (fn [[_ [r th] _]] (e/up (e/* r (e/cos th)) (e/* r (e/sin th))))]
      (is (lg/cyclic? (lg/transform (lg/free-particle 'm) polar) 2 1)))))

;; ---------------------------------------------------------------------------
;; The action is stationary on the motion (numeric, generated)

(defspec the-action-is-stationary-on-the-motion 25
  (prop/for-all [w (gen/fmap #(+ 0.5 (/ % 20.0)) (gen/choose 0 40))
                 a (gen/fmap #(/ % 10.0) (gen/choose -10 10))
                 k (gen/choose 1 3)]
    ;; the oscillator m = 1, stiffness w^2, between t = 0 and a time short
    ;; of half a period (so the stationary point is a minimum): the true
    ;; motion sin(w t) varied by a sin(k pi t / T)
    (let [T (/ (* 0.9 Math/PI) w)
          q (fn [t] (e/sin (e/* w t)))
          eta (fn [t] (e/sin (e/* k Math/PI (e// t T))))
          S (lg/varied-action (lg/harmonic 1 (* w w)) q eta 0 T)
          h 1e-3
          slope (/ (- (S h) (S (- h))) (* 2 h))]
      (and (< (Math/abs (double slope)) 1e-6)
           (>= (+ (S a) 1e-9) (S 0))))))
