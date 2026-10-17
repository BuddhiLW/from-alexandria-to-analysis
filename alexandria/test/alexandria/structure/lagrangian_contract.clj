(ns alexandria.structure.lagrangian-contract
  "The conformance suite of a Lagrangian system:
   (deflagrangian-contract name {:L L :n n :H-matches-EL? bool}) checks,
   symbolically through alexandria.structure.lagrangian, that

     legendre-twice     Legendre's transformation applied twice gives L back
     canonical          Hamilton's equations along (q, dL/dq') are the
                        Euler-Lagrange equations of q
     energy             for a time-independent L, E = q' dL/dq' - L has
                        dE/dt = q' . (Euler-Lagrange) on every path, so it
                        is constant on every motion
     first-variation    d/da L(q + a eta) = d/dt(eta dL/dq') - eta EL

   and, numerically for generated parameters, that the action of the true
   motion is stationary: its varied action has zero slope at a = 0."
  (:require [alexandria.structure.lagrangian :as lg]
            [clojure.test :refer [deftest is testing]]
            [emmy.env :as e]))

(defn proved? [diffs] (= :grade/proved (lg/grade :symbolic diffs)))

(defn paths
  "Literal paths for n degrees of freedom: a scalar for n = 1, else an up
   tuple of literal functions."
  [n prefix]
  (if (= 1 n)
    (e/literal-function (symbol prefix))
    (apply e/up (map #(e/literal-function (symbol (str prefix "_" %))) (range n)))))

(defmacro deflagrangian-contract
  [name {:keys [L n]}]
  (let [t #(symbol (str name "-" %))]
    `(do
       (deftest ~(t "legendre-twice")
         (is (proved? [(lg/legendre-twice ~L (lg/local ~n))])))
       (deftest ~(t "canonical-equals-lagrangian")
         (is (proved? [((lg/canonical-agreement ~L (paths ~n "q")) 't)])))
       (deftest ~(t "energy-is-conserved")
         (is (proved? [((lg/energy-rate ~L (paths ~n "q")) 't)])))
       (deftest ~(t "first-variation")
         (is (proved? [((lg/first-variation-integrand ~L (paths ~n "q") (paths ~n "eta")) 't)]))))))
