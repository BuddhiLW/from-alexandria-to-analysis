(ns alexandria.fermat.loci-test
  (:require [alexandria.fermat.loci :as loci]
            [alexandria.library :as library]
            [alexandria.ops.euclid]
            [alexandria.vocab]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [desargues.board.construction :as c]
            [emmy.env :as e]
            [hive-dsl.result :as r]))

(deftest every-locus-of-the-isagoge-is-proved
  (let [g (loci/graded)]
    (is (= 7 (count g)))
    (is (every? #{:grade/proved} (map :grade g)) (pr-str (remove (comp #{:grade/proved} :grade) g)))))

(def rationals (gen/fmap (fn [[n d]] (/ n d)) (gen/tuple (gen/choose -12 12) (gen/choose 1 7))))
(def positive (gen/fmap (fn [[n d]] (/ n d)) (gen/tuple (gen/choose 1 12) (gen/choose 1 7))))

(deftest each-figure-lies-on-its-equation-exactly
  (let [result (tc/quick-check 60
                 (prop/for-all [u rationals D positive B positive Z positive m positive]
                   (let [eqs (loci/equations {:D D :B B :Z Z :m m})
                         figs {:line (loci/line D B) :parabola (loci/parabola D) :circle (loci/circle B)
                               :ellipse (loci/ellipse B m)}
                         on? (fn [k] (= 0 ((eqs k) ((figs k) [u]))))]
                     (and (every? on? (keys figs))
                          (or (zero? u) (= 0 ((eqs :hyperbola) ((loci/hyperbola Z) [u]))))))))]
    (is (:pass? result) (pr-str (dissoc result :result-data)))))

(deftest the-circle-has-radius-b
  (testing "u = 1/2, B = 5: I = (3, 4), NI = 5"
    (is (= [3 4] ((loci/circle 5) [1/2])))
    (is (= [3 2] ((loci/ellipse 5 1/2) [1/2])) "the ellipse halves E")))

(deftest shelf-boards-hold
  (let [shelves (:ok (library/shelves))
        env-of (fn [{:keys [points]}]
                 (reduce (fn [env {:keys [id at op] :as p}]
                           (let [[x y] (if op (:xy (c/point p env)) at)] (assoc-in env [:points id] {:x x :y y})))
                         {:points {}} points))]
    (doseq [id [:fermat/locus-line :fermat/locus-circle]
            :let [{:keys [construction]} (shelves id)]]
      (testing id
        (is construction)
        (doseq [check (:checks construction)]
          (is (< (Math/abs (double (e/simplify (c/check check (env-of construction))))) 1e-9) (pr-str check)))))))

(deftest the-loci-are-shelf-data
  (let [res (loci/proof)
        steps (get-in res [:ok :fermat/loci :steps])]
    (is (r/ok? res))
    (is (= [:setup :line :hyperbola :parabola :circle :ellipse :descartes] (map :stage steps)))
    (is (every? (every-pred :claim :why :stage) steps))
    (is (every? :latin (butlast steps)) "every step but the comparison quotes Fermat")
    (is (every? :french (filter (comp #{:hyperbola :parabola :circle :ellipse} :stage) steps)))))
