(ns alexandria.structure.boolring-contract
  "The conformance suite for a Boolean ring, on top of the ring contract:
   (defboolring-contract name R element-gen) runs defring-contract and adds
   Boole's law of signs (Laws of Thought II. 9, III. 1): x x = x, x (1 - x)
   = 0, and, as a consequence, x + x = 0."
  (:require [alexandria.structure.boolring :as b]
            [alexandria.structure.ring :as ring]
            [alexandria.structure.ring-contract :refer [defring-contract]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]))

(defmacro defidempotent-contract
  "x x = x, x (1 - x) = 0 and x + x = 0 for elements from element-gen."
  [name R element-gen & [{:keys [num-tests] :or {num-tests 60}}]]
  (let [n #(symbol (str name "-" %))]
    `(do
       (defspec ~(n "idempotent") ~num-tests
         (prop/for-all [a# ~element-gen] (b/idempotent? ~R a#)))
       (defspec ~(n "contradiction") ~num-tests
         (prop/for-all [a# ~element-gen]
           (ring/same? ~R (ring/mul ~R a# (b/complement-of ~R a#)) (ring/zero ~R))))
       (defspec ~(n "characteristic-two") ~num-tests
         (prop/for-all [a# ~element-gen]
           (ring/same? ~R (ring/add ~R a# a#) (ring/zero ~R)))))))

(defmacro defboolring-contract
  "The ring laws plus idempotence."
  [name R element-gen & [opts]]
  `(do (defring-contract ~name ~R ~element-gen ~@(when opts [opts]))
       (defidempotent-contract ~name ~R ~element-gen ~@(when opts [opts]))))
