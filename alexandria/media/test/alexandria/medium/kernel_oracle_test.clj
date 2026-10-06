(ns alexandria.medium.kernel-oracle-test
  "The oracle itself: a kernel whose wasm module is over the 4 KB sync limit
   of emmy.viewer.raster glue compiles asynchronously, and
   kernel-oracle/deviation must wait for it instead of failing on a null
   batch."
  (:require [alexandria.medium.kernel :as kernel]
            [alexandria.medium.kernel-oracle :as ko]
            [clojure.test :refer [deftest is testing]]
            [emmy.env :as e]))

(def ^:private terms 80)

(def large-figure
  "x(t) = sum_{k=1}^{80} cos(k t)/k^2 written out term by term (no loop), so
   the module is far over the sync limit; y = t."
  {:f (fn [a]
        (fn [[t]]
          [(reduce e/+ (for [k (range 1 (inc terms))]
                         (e/* a (e// (e/cos (e/* k t)) (* k k)))))
           t]))
   :params [1]
   :state [0]
   :opts {:simplify? false}})

(def small-figure
  {:f (fn [a] (fn [[t]] [(e/* a (e/cos t)) (e/* a (e/sin t))]))
   :params [2]
   :state [0]})

(deftest large-kernel-waits-for-async-module
  (testing "the glue is large enough to take the async path"
    (is (> (count (:glue (kernel/kernel large-figure))) 8192)))
  (when-let [{:keys [wasm max-error nan-states]}
             (ko/deviation large-figure [[0.0] [0.1] [0.7] [1.3] [2.9]])]
    (is wasm "the wasm module loaded")
    (is (empty? nan-states))
    (is (< max-error 1e-4) (str "max error " max-error))))

(deftest small-kernel-still-runs-synchronously
  (when-let [{:keys [wasm max-error]} (ko/deviation small-figure [[0.0] [1.0] [2.5]])]
    (is wasm)
    (is (< max-error 1e-4))))
