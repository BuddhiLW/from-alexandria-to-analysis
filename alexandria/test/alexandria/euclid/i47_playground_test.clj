(ns alexandria.euclid.i47-playground-test
  (:require [alexandria.euclid.i47-playground :as ip]
            [clojure.test :refer [deftest is testing]]))

(defn- row [s] (ip/named (mapv double ((apply ip/figure s) [0]))))
(defn- close? [a b] (< (Math/abs (- (double a) (double b))) 1e-9))

(deftest each-preset-names-its-case
  (is (= {:euclid :right :obtuse :obtuse :acute :acute :meno :right :flat :flat}
         (update-vals ip/presets (comp ip/kind row)))))

(deftest the-windmill-matches-only-at-the-right-angle
  (testing "I.47: BL = AB^2, CL = AC^2 and BL + CL = BC^2"
    (let [{:keys [BL CL a2 b2 c2]} (row ip/start)]
      (is (close? BL c2)) (is (close? CL b2)) (is (close? a2 (+ b2 c2)))))
  (testing "any triangle: AB^2 - BL = AB.AC cos A, BL + CL = BC^2, a^2 = b^2 + c^2 - 2 CA.AD"
    (doseq [s [[0 0 -1.5 2.6 4 0] [0 0 1.8 2.4 4 0] [1 -2 3 5 -4 1] [2 2 -3 1 5 -1]]]
      (let [{:keys [BL CL a2 b2 c2 dot ad excess]} (row s)]
        (is (close? (- c2 BL) dot))
        (is (close? (- b2 CL) dot))
        (is (close? (+ BL CL) a2))
        (is (close? excess (* -2 (Math/sqrt b2) ad)))))))

(deftest squares-stand-outward
  (testing "the square on each side lies on the other side of it from the third vertex"
    (doseq [s [ip/start [0 0 3 0 0 4] [1 -2 3 5 -4 1]]]
      (let [{:keys [A B C ab1 ac1 bc1]} (row s)
            side (fn [[px py] [qx qy] [rx ry]] (Math/signum (- (* (- qx px) (- ry py)) (* (- qy py) (- rx px)))))]
        (is (= -1.0 (* (side A B C) (side A B ab1))))
        (is (= -1.0 (* (side A C B) (side A C ac1))))
        (is (= -1.0 (* (side B C A) (side B C bc1))))))))

(deftest the-flat-triangle-is-finite
  (let [r (row [0 0 -2 0 4 0])]
    (is (every? #(Double/isFinite %) (flatten (vals r))))
    (is (= :flat (ip/kind r)))))
