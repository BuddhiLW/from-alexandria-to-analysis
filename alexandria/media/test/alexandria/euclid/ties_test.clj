(ns alexandria.euclid.ties-test
  (:require [alexandria.euclid.elements :as el]
            [alexandria.euclid.ties :as ties]
            [alexandria.euclid.ties-view :as view]
            [alexandria.medium.figure :as figure]
            [alexandria.medium.kernel-oracle :as ko]
            [alexandria.proofs :as proofs]
            [clojure.test :refer [deftest is testing]]))

(deftest the-widgets-ship-raster-kernels
  (testing "every kernel compiles on :raster to glue and an Emmy :js fallback"
    (doseq [[k v] (merge (:kernels (view/pythagoras-value)) (:kernels (view/application-value)))]
      (is (string? (:glue v)) (str k))
      (is (seq (:fallback v)) (str k))))
  (testing "the value carries what the render-fn reads"
    (is (= [:a :b :c :A] (:tied (view/pythagoras-value))))
    (is (= #{:parabola :hyperbola :ellipse} (set (keys (:names (view/application-value))))))))

(deftest the-tie-blocks-read-back
  (let [fig (figure/->FnFigure (:f (:tie ties/figures)))
        r (first (figure/points fig [7 4 3 90 1 3] [[0]]))
        {:keys [a b c A]} (ties/block [:a :b :c :A] {:a 0 :B 16} r :a)]
    (is (= [7.0 4.0 90.0] [a b A]))
    (is (< (Math/abs (- 49 (+ 16 (* c c)))) 1e-9)))
  (let [fig (figure/->FnFigure (:f (:atie ties/figures)))
        r (first (figure/points fig [10 21 9 -1] [[0]]))]
    (is (< (Math/abs (- 3 (:y (ties/block [:a :S :y :k] {:S 4} r :S)))) 1e-9))))

(deftest the-wasm-kernels-match-emmy
  (doseq [[k params] {:tie [7 4 3 120 1 3] :fig [4 3 120] :atie [10 21 3 -0.5] :afig [10 3 0.5]}]
    (when-let [{:keys [max-error]} (ko/deviation (get ties/figures k) params [[0]])]
      (is (< max-error 1e-3) (str k " " max-error)))))

(deftest i-1-is-stepped-one-sentence-per-step
  (let [steps (proofs/steps el/proofs-resource :euclid/I.1)]
    (is (= 12 (count steps)))
    (is (= "Let AB be the given finite straight line." (:claim (first steps))))
    (is (= #{:given :circle-a :circle-b :join :radius-a :radius-b :radii :equal :done}
           (set (map :stage steps))))))
