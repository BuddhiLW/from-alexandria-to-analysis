(ns alexandria.kepler.mysterium-test
  (:require [alexandria.kepler.mysterium :as my]
            [alexandria.library :as library]
            [alexandria.solids :as solids]
            [clojure.test :refer [deftest is testing]]
            [emmy.env :as e]
            [hive-dsl.result :as r]))

(defn- close? [a b tol] (< (Math/abs (- (double a) (double b))) tol))

(deftest the-five-solids-are-data
  (testing "vertices, edges and faces counted from the derived data"
    (is (= {:V 4 :E 6 :F 4} (solids/counts (solids/solids :tetrahedron))))
    (is (= {:V 8 :E 12 :F 6} (solids/counts (solids/solids :cube))))
    (is (= {:V 6 :E 12 :F 8} (solids/counts (solids/solids :octahedron))))
    (is (= {:V 20 :E 30 :F 12} (solids/counts (solids/solids :dodecahedron))))
    (is (= {:V 12 :E 30 :F 20} (solids/counts (solids/solids :icosahedron)))))
  (testing "faces are regular polygons of one kind"
    (is (= {:tetrahedron #{3} :cube #{4} :octahedron #{3} :dodecahedron #{5} :icosahedron #{3}}
           (update-vals solids/solids #(set (map count (:faces %)))))))
  (testing "every vertex is on the circumsphere, exactly"
    (doseq [[id s] solids/solids
            :let [r2 (solids/norm-sq (first (:vertices s)))]
            v (:vertices s)]
      (is (e/zero? (e/simplify (e/- (solids/norm-sq v) r2))) (str id)))))

(deftest the-ratios-are-proved
  (let [g (my/graded)]
    (is (every? #{:grade/proved} (map :grade (take (count my/identities) g))))
    (is (every? #{:grade/numeric} (map :grade (drop (count my/identities) g)))
        "Kepler's column agrees with the exact ratios to the unit")
    (is (every? #{:raster} (map :source (drop (count my/identities) g)))
        "the numbers come from raster kernels")))

(deftest the-raster-ratios-match-emmy
  (testing "the raster kernel of each exact ratio agrees with the solid built on JVM doubles (the oracle)"
    (doseq [id solids/solid-ids
            :let [r (solids/radii (solids/solid id {:numeric? true}))]]
      (is (close? (my/ratio-value id) (Math/sqrt (/ (double (:in r)) (double (:circum r)))) 1e-12) (str id)))))

(deftest the-model-against-copernicus
  (let [rows (into {} (map (juxt :solid identity)) (my/comparison))]
    (testing "fits within 1% for the tetrahedron and icosahedron"
      (is (< (Math/abs (:miss (rows :tetrahedron))) 0.01))
      (is (< (Math/abs (:miss (rows :icosahedron))) 0.01)))
    (testing "misses Jupiter by 9% and Mercury by 20%"
      (is (close? -0.091 (:miss (rows :cube)) 0.001))
      (is (close? -0.201 (:miss (rows :octahedron)) 0.001)))
    (testing "the octahedron's midsphere brings Mercury within 3%"
      (is (close? 707.1 (:midsphere-value (rows :octahedron)) 0.1))
      (is (< (Math/abs (:midsphere-miss (rows :octahedron))) 0.03)))))

(deftest the-shelf-claims-grade
  (let [res (library/shelves)
        kepler (for [[id p] (:ok res) :when (#{:kepler/mysterium-order :kepler/mysterium-table} id)
                     c (:claims p)]
                 c)]
    (is (r/ok? res))
    (is (= 5 (count kepler)))
    (is (every? #{:grade/proved} (map my/claim-grade kepler)))))

(deftest the-proof-is-shelf-data
  (let [p (get-in (my/proof) [:ok :kepler/mysterium])]
    (is (= 10 (count (:steps p))))
    (is (every? (comp string? :text) (:passages p)))))
