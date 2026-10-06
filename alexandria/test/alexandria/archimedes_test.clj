(ns alexandria.archimedes-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [desargues.board :as board]
            [desargues.board.construction]
            [desargues.board.euclid]
            [desargues.board.kernel :as kernel]
            [emmy.numerical.quadrature :as quadrature]))

(def shelves
  ["alexandria/archimedes/measurement_of_a_circle.edn"
   "alexandria/archimedes/quadrature_of_the_parabola.edn"])

(defn read-shelf [resource]
  (-> resource io/resource slurp edn/read-string))

(defn propositions []
  (mapcat :propositions (map read-shelf shelves)))

(def forbidden-result-params
  '#{pi circumference circle_area inscribed_area circumscribed_area
     lower96 upper96 first_area segment_area stage1_area partial3_area
     geometric_sum quadrature_estimate})

(defn- tmp-dir []
  (doto (io/file (System/getProperty "java.io.tmpdir") "alexandria-archimedes-test") .mkdirs))

(defn- with-param-inits [construction overrides]
  (update construction :params
          (fn [params]
            (mapv (fn [{:keys [id] :as p}]
                    (if (contains? overrides (symbol (name id)))
                      (assoc p :init (overrides (symbol (name id))))
                      p)) params))))

(defn- with-point-at [construction id at]
  (update construction :points
          (fn [points]
            (mapv (fn [p] (if (= id (:id p)) (assoc p :at at) p)) points))))

(defn- board-values
  ([construction] (board-values construction {}))
  ([construction param-overrides]
   (let [construction (with-param-inits construction param-overrides)
         plan (kernel/plan construction)
         spec* (assoc construction :params (vec (or (:params plan) (:params construction))))
         compiler (requiring-resolve 'desargues.board.compiler/raster-compiler)
         define-kernel (requiring-resolve 'desargues.board.compiler/define-kernel)
         kc (compiler)
         kvar (define-kernel kc (symbol (str (name (:id construction)) "-kernel!"))
                             ((:form plan) (symbol (str (name (:id construction)) "-kernel!"))))
         frame (board/sample-frame @kvar spec* (:outputs plan))
         b (board/board-value spec* plan {:wasm "test.wasm" :export (name (:id construction))} frame)]
     {:plan plan
      :board b
      :values (into {}
                    (map-indexed (fn [i check]
                                   [(:label check) (first (get-in b [:board/frame (keyword (str "o-ck" i))]))])
                                 (:checks construction)))})))
(defn- check-holds? [check value]
  (case (or (:predicate check) (when (contains? check :expected) :near) :finite)
    :near (<= (Math/abs (- (double value) (double (:expected check))))
              (double (or (:tolerance check) 1.0E-9)))
    :positive (pos? (double value))
    :negative (neg? (double value))
    :finite (Double/isFinite (double value))))

(defn- checks-hold? [construction param-overrides]
  (let [{:keys [values]} (board-values construction param-overrides)]
    (doseq [check (:checks construction)]
      (is (check-holds? check (values (:label check))) (:label check)))))

(deftest no-result-constants-in-params
  (doseq [{:keys [id construction]} (propositions)]
    (testing id
      (is (empty? (filter forbidden-result-params (map :id (:params construction))))))))

(deftest every-archimedes-board-plans-and-checks
  (doseq [{:keys [id construction]} (propositions)]
    (testing (str id " plans through desargues")
      (let [{:keys [plan board]} (board-values construction)]
        (is (seq (:outputs plan)))
        (is (seq (:layers plan)))
        (is (fn? (:form plan)))
        (is (= (:outputs plan) (:board/outputs board)))))
    (testing (str id " checks hold at initial params")
      (checks-hold? construction {}))))

(deftest checks-hold-after-dragging-params
  (let [by-id (into {} (map (juxt :id :construction) (propositions)))
        samples {:archimedes/circle-1 [{'r 0.75 'sides 12} {'r 1.4 'sides 48} {'r 1.8 'sides 96}]
                 :archimedes/circle-3 [{'r 0.6 'sides 96} {'r 1.5 'sides 96}]
                 :archimedes/parabola-17-24 [{'ax -0.75 'bx 1.25} {'ax -1.2 'bx 0.6} {'ax -0.25 'bx 1.4}]}]
    (doseq [[id envs] samples
            overrides envs
            :let [construction (by-id id)]]
      (testing (str id " " overrides)
        (checks-hold? construction overrides)))))

(deftest perturbing-free-point-breaks-a-check
  (let [by-id (into {} (map (juxt :id :construction) (propositions)))
        circle (-> (:archimedes/circle-1 by-id)
                   (with-point-at :T0 [2.6 0.5]))
        check (first (:checks circle))
        {:keys [values]} (board-values circle)]
    (is (not (check-holds? check (values (:label check))))
        "Moving the triangle's free vertex changes the computed area check")))

(deftest emmy-quadrature-checks-parabolic-segment
  (let [area (quadrature/definite-integral #(- 1 (* % %)) -1.0 1.0
                                           {:method :closed :compile? false})]
    (is (< (Math/abs (- area 4/3)) 1.0E-8))))

(def polygon-bound-property
  (prop/for-all [n (gen/elements [6 12 24 48 96])]
    (let [lower (* n (Math/sin (/ Math/PI n)))
          upper (* n (Math/tan (/ Math/PI n)))]
      (< lower Math/PI upper))))

(deftest polygon-bounds-property
  (let [result (tc/quick-check 50 polygon-bound-property)]
    (is (:pass? result) (pr-str result))))
