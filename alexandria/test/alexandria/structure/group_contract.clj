(ns alexandria.structure.group-contract
  "The conformance suite every alexandria.structure.group instance passes:
   (defgroup-contract name G element-gen point-gen same-point?) defines the
   group and action laws as test.check properties."
  (:require [alexandria.structure.group :as g]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(defmacro defgroup-contract
  "Properties for group G over elements from element-gen; when point-gen
   is given, the action laws too, points compared by same-point?."
  [name G element-gen point-gen same-point? & [{:keys [num-tests] :or {num-tests 60}}]]
  (let [n #(symbol (str name "-" %))]
    `(do
       (defspec ~(n "associative") ~num-tests
         (prop/for-all [a# ~element-gen b# ~element-gen c# ~element-gen]
           (g/same? ~G (g/compose ~G (g/compose ~G a# b#) c#)
                    (g/compose ~G a# (g/compose ~G b# c#)))))
       (defspec ~(n "unit") ~num-tests
         (prop/for-all [a# ~element-gen]
           (and (g/same? ~G (g/compose ~G (g/unit ~G) a#) a#)
                (g/same? ~G (g/compose ~G a# (g/unit ~G)) a#))))
       (defspec ~(n "inverse") ~num-tests
         (prop/for-all [a# ~element-gen]
           (and (g/same? ~G (g/compose ~G a# (g/inverse ~G a#)) (g/unit ~G))
                (g/same? ~G (g/compose ~G (g/inverse ~G a#) a#) (g/unit ~G)))))
       ~@(when point-gen
           [`(defspec ~(n "action-unit") ~num-tests
               (prop/for-all [x# ~point-gen]
                 (~same-point? (g/act ~G (g/unit ~G) x#) x#)))
            `(defspec ~(n "action-compatible") ~num-tests
               (prop/for-all [a# ~element-gen b# ~element-gen x# ~point-gen]
                 (~same-point? (g/act ~G (g/compose ~G a# b#) x#)
                  (g/act ~G a# (g/act ~G b# x#)))))]))))

(def unit-interval
  "Doubles in [-1 1] at a 1/1000 grain."
  (gen/fmap #(/ % 1000.0) (gen/choose -1000 1000)))

(defn scaled [lo hi] (gen/fmap #(+ lo (* (- hi lo) (/ (+ % 1.0) 2.0))) unit-interval))

(def plane-point (gen/tuple (scaled -2 2) (scaled -2 2)))
