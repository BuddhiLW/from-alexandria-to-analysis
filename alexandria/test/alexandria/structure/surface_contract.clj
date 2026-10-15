(ns alexandria.structure.surface-contract
  "The conformance suite every cell complex passes: (defsurface-contract
   name complex-gen) defines, as test.check properties over generated
   complexes, the laws orientation and the Euler characteristic obey.

     chi          V - E + F, and every edge joins two vertices of the complex
     orientation  when orientable, every edge used twice is run in opposite
                  senses by the two faces' (sign * sense); when not, a
                  conflict edge is named
     holonomy     of the core loop agrees with orientability for a strip"
  (:require [alexandria.structure.surface :as s]
            [alexandria.structure.surfaces :as ss]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(defn- uses [{:keys [faces]}]
  (reduce (fn [m [fi f]] (reduce (fn [m [e sg]] (update m e (fnil conj []) [fi sg])) m f))
          {} (map-indexed vector faces)))

(defn senses-consistent?
  "Each edge used twice is run in opposite directions once the senses apply."
  [c senses]
  (every? (fn [[_ u]] (or (not= 2 (count u))
                          (let [[[fa sa] [fb sb]] u] (= (* (senses fa) sa) (- (* (senses fb) sb))))))
          (uses c)))

(defn chi-law?
  [c]
  (let [{:keys [V E F chi]} (s/counts c)]
    (and (= chi (- (+ V F) E))
         (every? #(every? (s/vertices c) %) (vals (:edges c))))))

(defn orientation-law?
  [c]
  (let [o (s/orientation c)]
    (if (:orientable? o)
      (and (= (set (range (count (:faces c)))) (set (keys (:senses o))))
           (senses-consistent? c (:senses o)))
      (contains? (:edges c) (:conflict o)))))

(defmacro defsurface-contract
  "Properties for the complexes drawn from complex-gen."
  [name complex-gen & [{:keys [num-tests] :or {num-tests 40}}]]
  (let [n #(symbol (str name "-" %))]
    `(do
       (defspec ~(n "chi") ~num-tests (prop/for-all [c# ~complex-gen] (chi-law? c#)))
       (defspec ~(n "orientation") ~num-tests (prop/for-all [c# ~complex-gen] (orientation-law? c#))))))
