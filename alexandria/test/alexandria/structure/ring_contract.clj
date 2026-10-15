(ns alexandria.structure.ring-contract
  "The conformance suite every alexandria.structure.ring instance passes:
   (defring-contract name R element-gen) defines the commutative ring laws
   as test.check properties, and checks that the additive group passes the
   group laws; (defcharacter-contract name G chi element-gen) that a
   character is multiplicative."
  (:require [alexandria.structure.group :as g]
            [alexandria.structure.group-contract :refer [defgroup-contract]]
            [alexandria.structure.ring :as ring]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.generators :as gen]))

(defmacro defring-contract
  "Properties for the commutative ring R over elements from element-gen;
   its additive group passes the group contract."
  [name R element-gen & [{:keys [num-tests] :or {num-tests 60}}]]
  (let [n #(symbol (str name "-" %))]
    `(do
       (defspec ~(n "add-associative") ~num-tests
         (prop/for-all [a# ~element-gen b# ~element-gen c# ~element-gen]
           (ring/same? ~R (ring/add ~R (ring/add ~R a# b#) c#) (ring/add ~R a# (ring/add ~R b# c#)))))
       (defspec ~(n "add-commutative") ~num-tests
         (prop/for-all [a# ~element-gen b# ~element-gen]
           (ring/same? ~R (ring/add ~R a# b#) (ring/add ~R b# a#))))
       (defspec ~(n "zero-and-negatives") ~num-tests
         (prop/for-all [a# ~element-gen]
           (and (ring/same? ~R (ring/add ~R a# (ring/zero ~R)) a#)
                (ring/same? ~R (ring/add ~R a# (ring/neg ~R a#)) (ring/zero ~R)))))
       (defspec ~(n "mul-associative") ~num-tests
         (prop/for-all [a# ~element-gen b# ~element-gen c# ~element-gen]
           (ring/same? ~R (ring/mul ~R (ring/mul ~R a# b#) c#) (ring/mul ~R a# (ring/mul ~R b# c#)))))
       (defspec ~(n "mul-commutative-with-one") ~num-tests
         (prop/for-all [a# ~element-gen b# ~element-gen]
           (and (ring/same? ~R (ring/mul ~R a# b#) (ring/mul ~R b# a#))
                (ring/same? ~R (ring/mul ~R a# (ring/one ~R)) a#))))
       (defspec ~(n "distributive") ~num-tests
         (prop/for-all [a# ~element-gen b# ~element-gen c# ~element-gen]
           (ring/same? ~R (ring/mul ~R a# (ring/add ~R b# c#))
                       (ring/add ~R (ring/mul ~R a# b#) (ring/mul ~R a# c#)))))
       (defgroup-contract ~(n "additive") (ring/additive ~R) ~element-gen (gen/elements (ring/elements ~R)) =))))

(defmacro defcharacter-contract
  "chi(a b) = chi(a) chi(b) for elements of G from element-gen."
  [name G chi element-gen & [{:keys [num-tests] :or {num-tests 60}}]]
  `(defspec ~(symbol (str name "-multiplicative")) ~num-tests
     (prop/for-all [a# ~element-gen b# ~element-gen]
       (= (~chi (g/compose ~G a# b#)) (* (~chi a#) (~chi b#))))))
