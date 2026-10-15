(ns alexandria.structure.tie-contract
  "The conformance suite of a tie: (deftie-contract name tie) checks, on
   states generated inside the ranges, that
     - the source keeps its own value (when inside its range),
     - solving again from any key of a solved state returns that state,
     - the tie kernel is the blocks of `solve`, side by side,
     - the kernel compiles through alexandria.raster to the same numbers."
  (:require [alexandria.raster :as raster]
            [alexandria.structure.tie :as tie]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(defn state-gen
  "States with every key inside its range."
  [t]
  (apply gen/tuple (for [k (:keys t) :let [[lo hi] ((:ranges t) k)]]
                     (gen/fmap #(+ lo (* (- hi lo) (/ % 1000.0))) (gen/choose 1 999)))))

(defn close? [a b tol] (<= (Math/abs (- (double a) (double b))) (* tol (max 1.0 (Math/abs (double a))))))

(defmacro deftie-contract
  [name t & [{:keys [num-tests tol] :or {num-tests 60 tol 1e-9}}]]
  (let [n #(symbol (str name "-" %))]
    `(do
       (defspec ~(n "source-kept") ~num-tests
         (prop/for-all [st# (state-gen ~t) i# (gen/choose 0 (dec (count (:keys ~t))))]
           (let [k# (nth (:keys ~t) i#)]
             (close? (nth (tie/solve ~t k# st#) i#) (nth st# i#) ~tol))))
       (defspec ~(n "round-trip") ~num-tests
         (prop/for-all [st# (state-gen ~t) i# (gen/choose 0 (dec (count (:keys ~t))))]
           (let [s0# (mapv double (tie/solve ~t (nth (:keys ~t) i#) st#))]
             (every? (fn [k#] (every? true? (map #(close? %1 %2 ~tol) s0# (tie/solve ~t k# s0#))))
                     (:keys ~t)))))
       (defspec ~(n "kernel-is-blocks") ~num-tests
         (prop/for-all [st# (state-gen ~t)]
           (let [out# ((apply (tie/kernel ~t) st#) nil)]
             (every? (fn [[k# v#]] (every? true? (map #(close? %1 %2 1e-12) v# (tie/solve ~t k# st#))))
                     (tie/blocks ~t out#)))))
       (defspec ~(n "raster-agrees") 10
         (prop/for-all [st# (state-gen ~t)]
           (let [jvm# ((apply (tie/kernel ~t) st#) nil)
                 ras# (binding [raster/*simplify?* false]
                        (apply raster/value (fn [& xs#] ((apply (tie/kernel ~t) xs#) nil)) st#))]
             (every? true? (map #(close? %1 %2 1e-9) jvm# ras#))))))))
