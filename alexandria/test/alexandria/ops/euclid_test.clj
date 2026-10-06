(ns alexandria.ops.euclid-test
  (:require [alexandria.ops.euclid]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [desargues.board :as board]))

(def epsilon 1e-7)

(defn- near-zero? [x]
  (< (Math/abs (double x)) epsilon))

(defn- tmp-dir []
  (doto (io/file (System/getProperty "java.io.tmpdir") "alexandria-euclid-test")
    .mkdirs))

(defn- shelf []
  (edn/read-string (slurp (io/resource "alexandria/euclid/elements_1.edn"))))

(defn- frame-value [compiled k]
  (first (get-in compiled [:board/frame k])))

(defn- check-value [compiled k]
  (let [rk (keyword (str (name k) "ref"))]
    (if (contains? (:board/frame compiled) rk)
      (- (frame-value compiled k) (frame-value compiled rk))
      (frame-value compiled k))))

(defn- point [compiled i]
  [(frame-value compiled (keyword (str "o-pt" i "x")))
   (frame-value compiled (keyword (str "o-pt" i "y")))])

(defn- compile-construction [spec]
  (board/compile-board! spec {:out-dir (.getPath (tmp-dir))}))


(defn- check-keys [compiled]
  (->> (:board/outputs compiled)
       (filter #(re-matches #":?o-ck\d+(ref)?" (str %)))
       (remove #(re-find #"ref$" (str %)))))

(deftest every-elements-book-i-board-builds-and-checks-hold
  (doseq [{:keys [id title statement source construction]} (:propositions (shelf))]
    (testing (str id)
      (let [compiled (compile-construction construction)
            cks (check-keys compiled)]
        (is (qualified-keyword? id)) (is (seq title)) (is (seq statement)) (is (seq source))
        (is (= :construction (:kind construction))) (is (seq (:board/layers compiled))) (is (seq cks))
        (doseq [[idx k] (map-indexed vector cks)]
          (is (near-zero? (check-value compiled k))
              (str id " check " idx " " k " should be zero")))
        (doseq [overrides (case id
                            :euclid/I.1
                            [{:A [-0.75 0.1] :B [0.75 0.2]}
                             {:A [-0.4 -0.4] :B [1.0 0.35]}]
                            :euclid/I.2
                            [{:A [-0.2 0.15] :B [1.35 -0.1] :C [2.0 0.9]}
                             {:A [0.15 -0.25] :B [1.75 0.35] :C [2.55 0.0]}
                             {:A [-0.35 0.45] :B [1.2 0.1] :C [1.8 1.05]}]
                            :euclid/I.3
                            [{:A [0.0 0.0] :B [3.1 0.2] :C [0.8 1.15] :G [1.35 1.05]}
                             {:A [-0.1 -0.2] :B [3.0 0.35] :C [0.7 0.9] :G [1.25 1.1]}
                             {:A [0.2 0.1] :B [3.2 -0.1] :C [0.95 1.2] :G [1.45 0.85]}]
                            [])]
          (let [dragged (compile-construction (update construction :points
                                                       (fn [points]
                                                         (mapv #(if-let [at (get overrides (:id %))]
                                                                  (assoc % :at at)
                                                                  %)
                                                               points))))]
            (doseq [[idx k] (map-indexed vector (check-keys dragged))]
              (is (near-zero? (check-value dragged k))
                  (str id " dragged " overrides " check " idx " " k " should be zero")))))))))

(deftest circle-circle-crossing-lies-on-both-circles-and-side-zero-is-left
  (let [compiled (compile-construction {:id :circle-circle-op-test :kind :construction
                                        :points [{:id :A :at [0 0] :fixed? true}
                                                 {:id :B :at [1 0] :fixed? true}
                                                 {:id :X :op :circles :circles [[:A :B] [:B :A]] :side 0}]
                                        :checks [{:distance [:A :X] :against {:distance [:A :B]}}
                                                 {:distance [:B :X] :against {:distance [:B :A]}}]})]
    (is (near-zero? (check-value compiled :o-ck0)))
    (is (near-zero? (check-value compiled :o-ck1)))
    (is (< 0 (second (point compiled 2))) "side 0 is left of AB")))

(deftest line-circle-crossing-produces-line-beyond-endpoint
  (let [compiled (compile-construction {:id :line-circle-op-test :kind :construction
                                        :points [{:id :A :at [0 0] :fixed? true}
                                                 {:id :B :at [1 0] :fixed? true}
                                                 {:id :O :at [0 0] :fixed? true}
                                                 {:id :R :at [2 0] :fixed? true}
                                                 {:id :X :op :line-circle :line [:A :B] :circle [:O :R] :beyond? true}]
                                        :checks [{:collinear [:A :X :B]}
                                                 {:distance [:O :X] :against {:distance [:O :R]}}]})]
    (is (near-zero? (check-value compiled :o-ck0)))
    (is (near-zero? (check-value compiled :o-ck1)))
    (is (< 1.0 (first (point compiled 4))) "intersection is beyond B")))

(deftest segment-circle-cut-lies-on-segment-and-circle
  (let [compiled (compile-construction {:id :segment-circle-op-test :kind :construction
                                        :points [{:id :A :at [0 0] :fixed? true}
                                                 {:id :B :at [2 0] :fixed? true}
                                                 {:id :O :at [0 0] :fixed? true}
                                                 {:id :R :at [1 0] :fixed? true}
                                                 {:id :X :op :cut :segment [:A :B] :circle [:O :R] :which :second}]
                                        :checks [{:collinear [:A :X :B]}
                                                 {:distance [:O :X] :against {:distance [:O :R]}}]})]
    (is (near-zero? (check-value compiled :o-ck0)))
    (is (near-zero? (check-value compiled :o-ck1)))
    (let [[x _] (point compiled 4)] (is (<= 0.0 x 2.0)))))

(deftest replacing-i-1-constructed-point-breaks-equal-length-check
  (let [i-1 (-> (shelf) :propositions first :construction)
        broken (update i-1 :points #(conj (subvec (vec %) 0 2) {:id :C :at [0.0 0.25] :fixed? true}))
        compiled (compile-construction broken)]
    (is (not (near-zero? (check-value compiled :o-ck0))))))

(deftest replacing-i-2-result-with-wrong-circle-point-breaks-equal-length-check
  (let [i-2 (-> (shelf) :propositions second :construction)
        broken (update i-2 :points
                       (fn [points]
                         (conj (subvec (vec points) 0 5)
                               {:id :F :op :line-circle :line [:D :A] :circle [:A :B] :beyond? true})))
        compiled (compile-construction broken)]
    (is (not (near-zero? (check-value compiled :o-ck0))))))
