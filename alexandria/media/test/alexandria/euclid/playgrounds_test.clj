(ns alexandria.euclid.playgrounds-test
  (:require [alexandria.euclid.i47-playground :as ip]
            [alexandria.euclid.playgrounds-view :as view]
            [alexandria.medium.kernel-oracle :as ko]
            [clojure.test :refer [deftest is testing]]))

(deftest the-playgrounds-ship-raster-kernels
  (testing "I.47: the free-vertex figure compiles on :raster to glue and a fallback"
    (let [{:keys [kernel start presets]} (view/i47-value)]
      (is (string? (:glue kernel)))
      (is (seq (:fallback kernel)))
      (is (= ip/start start))
      (is (contains? presets :euclid)))))

(deftest the-wasm-kernels-match-emmy
  (doseq [s (vals ip/presets)]
    (when-let [{:keys [max-error]} (ko/deviation ip/kernel s [[0]])]
      (is (< max-error 1e-3) (str s " " max-error)))))
