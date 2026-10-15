(ns alexandria.delian.playgrounds-view-test
  (:require [alexandria.delian.playgrounds :as pg]
            [alexandria.delian.playgrounds-view :as view]
            [alexandria.medium.kernel-oracle :as ko]
            [clojure.test :refer [deftest is testing]]))

(deftest the-playgrounds-ship-raster-kernels
  (doseq [[nm v] {:means (view/means-value) :conchoid (view/conchoid-value) :cissoid (view/cissoid-value)}]
    (testing (name nm)
      (is (= 2 (count (:kernels v))))
      (doseq [[k kv] (:kernels v)]
        (is (string? (:glue kv)) (str k))
        (is (seq (:fallback kv)) (str k)))
      (is (contains? (:presets v) (first (keys (:presets v))))))))

(deftest the-wasm-kernels-match-emmy
  (doseq [[k params state] [[:fig [1 2 2 1] [0]] [:curves [1 2] [1.5]]
                            [:cfig [1 2 60 2.2] [0]] [:conchoid [1 2] [1.0]]
                            [:sfig [1.2 1] [0]] [:cissoid [1.2] [1.0]]]]
    (when-let [{:keys [max-error]} (ko/deviation (get pg/kernels k) params [state])]
      (is (< max-error 1e-3) (str k " " max-error)))))

(deftest the-archytas-playground-mounts
  (testing "the view returns the MathBox-on-raster form with its presets and live case"
    (let [s (pr-str (view/archytas))]
      (is (re-find #"delian-archytas" s))
      (doseq [k (keys pg/archytas-presets)]
        (is (re-find (re-pattern (str "\"" (name k) "\"")) s) (str k)))
      (is (re-find #"data-live-case" s))))
  (testing "the Archytas kernel on wasm matches Emmy at every preset"
    (doseq [{:keys [ad ab u]} (vals pg/archytas-presets)]
      (when-let [{:keys [max-error]} (ko/deviation (:afig pg/kernels) [ad ab u] [[0]])]
        (is (< max-error 1e-3) (str [ad ab u] " " max-error))))))
