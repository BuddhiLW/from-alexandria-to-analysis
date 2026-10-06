(ns alexandria.descartes.geometrie-test
  (:require [alexandria.descartes.geometrie :as g]
            [alexandria.library :as library]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [desargues.board.construction :as c]
            [alexandria.ops.euclid]
            [alexandria.vocab]
            [emmy.env :as e]
            [hive-dsl.result :as r]))

(defn- grades [k] (map :grade (k (g/graded))))

(defn- close? [a b] (< (Math/abs (- (double a) (double b))) 1e-9))

(def segments (gen/fmap #(+ 0.2 (* 3 %)) (gen/double* {:min 0 :max 1 :NaN? false :infinite? false})))

(deftest every-claim-of-la-geometrie-is-proved
  (doseq [k [:book-1 :plane :pappus :normals :ellipse :signs :construction :trisection]]
    (testing k
      (is (seq (grades k)))
      (is (every? #{:grade/proved} (grades k))))))

(deftest plane-roots-hold-for-any-segments
  (let [result (tc/quick-check 60
                 (prop/for-all [a segments b segments]
                   (let [{:keys [O P M]} (g/plane-root-points a b)
                         len (fn [[x y] [u v]] (Math/hypot (- u x) (- v y)))
                         z (len O M) y (len P M)]
                     (and (close? (* z z) (+ (* a z) (* b b)))
                          (close? (* y y) (+ (* -1 a y) (* b b)))))))]
    (is (:pass? result) (pr-str (dissoc result :result-data)))))

(deftest descartes-ellipse-normal
  (testing "r = 2, q = 4, C at MA = 1: P at v = 1 - 1/2 + 1 = 3/2"
    (is (= 3/2 (g/ellipse-normal-foot 2 4 1)))
    (is (= 1 (g/ellipse-second-root 2 4 3/2 1)) "the two roots are one")
    (is (not= 1 (g/ellipse-second-root 2 4 2 1)) "P too far: two unequal roots")))

(deftest book-3-roots-from-raster
  (let [{:keys [mean trisection source]} (g/book-3-numbers)
        {:keys [gk third-chord GK rest-chord FL]} trisection]
    (is (= :raster source))
    (is (< (Math/abs (- (:root mean) (Math/cbrt 2))) 1e-9) "FL: the first of two means between 1 and 2")
    (is (< (Math/abs (- gk third-chord)) 1e-9) "gk = NQ, the chord of a third of the arc")
    (is (< (Math/abs (- GK rest-chord)) 1e-9) "GK = NV, a third of the rest of the circle")
    (is (< (Math/abs (+ FL gk GK)) 1e-9) "the false root FL = -(QN + NV)")))

(deftest rule-of-signs-on-descartes-quartic
  (is (= {:changes 3 :permanences 1} (g/sign-changes g/quartic)))
  (is (= {:changes 1 :permanences 0} (g/sign-changes [1 -2])))
  (is (= {:changes 0 :permanences 2} (g/sign-changes [1 0 2 3])) "zero coefficients are skipped")
  (is (every? zero? (map #(g/polynomial g/quartic %) [2 3 4 -5]))))

(deftest unit-constructions-hold-for-any-segments
  (let [result (tc/quick-check 60
                 (prop/for-all [a segments b segments th (gen/elements [0.4 0.9 1.3 2.0])]
                   (let [{:keys [B E]} (g/multiplication-points a b th)
                         {:keys [G I]} (g/square-root-points a)
                         len (fn [[x y] [u v]] (Math/hypot (- u x) (- v y)))]
                     (and (close? (len B E) (* a b))
                          (close? (len G I) (Math/sqrt a))))))]
    (is (:pass? result) (pr-str (dissoc result :result-data)))))

(deftest pappus-locus-type-follows-the-discriminant
  (let [{[lo hi] :roots} (g/parabola-ratios)]
    (is (< lo hi))
    (is (= :ellipse (g/classify (g/pappus-coefficients (/ (+ lo hi) 2)))))
    (is (= :hyperbola (g/classify (g/pappus-coefficients 1))))
    (is (= :hyperbola (g/classify (g/pappus-coefficients (* 2 lo)))))
    (is (< (Math/abs (double (g/discriminant (g/pappus-coefficients (rationalize lo))))) 1e-9))))

(deftest the-swept-point-holds-the-ratio
  (doseq [lam [1 -2 -0.5] phi [0.3 0.9 1.4 2.2]]
    (let [P ((g/pappus-locus lam) [phi])
          [d1 d2 d3 d4] (map #(g/oblique-distance % P) g/pappus-lines)]
      (is (close? (* d1 d3) (* lam d2 d4)) (str lam " " phi)))))

(deftest normal-root-is-double
  (testing "on y^2 = x at C = (1, 1)"
    (is (= 2 (g/second-root 2 1 1)) "P too far: the circle meets the curve again at x = 2")
    (is (= 0 (g/second-root 1 1 1)) "P too near: again at x = 0")
    (is (= 1 (g/second-root 3/2 1 1)) "P = x0 + r/2: the two roots are one")))

(deftest shelf-boards-hold
  (let [shelves (:ok (library/shelves))
        env-of (fn [{:keys [points]}]
                 (reduce (fn [env {:keys [id at op] :as p}]
                           (let [[x y] (if op (:xy (c/point p env)) at)] (assoc-in env [:points id] {:x x :y y})))
                         {:points {}} points))]
    (doseq [id [:descartes/multiplication :descartes/square-root :descartes/pappus-four-lines :descartes/normal]
            :let [{:keys [construction]} (shelves id)]]
      (testing id
        (is construction)
        (doseq [check (:checks construction)]
          (is (< (Math/abs (double (e/simplify (c/check check (env-of construction))))) 1e-9)))))))

(deftest the-propositions-are-shelf-data
  (let [res (g/proof)]
    (is (r/ok? res))
    (doseq [id [:descartes/unit :descartes/plane :descartes/pappus :descartes/normal :descartes/ellipse
                :descartes/signs :descartes/construction]]
      (is (seq (get-in res [:ok id :steps])) id)
      (is (every? (every-pred :claim :why :stage) (get-in res [:ok id :steps]))))))
