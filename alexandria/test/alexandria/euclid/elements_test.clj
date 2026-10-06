(ns alexandria.euclid.elements-test
  (:require [alexandria.euclid.elements :as el]
            [alexandria.ops.euclid :as ops]
            [clojure.test :refer [deftest is testing]]
            [desargues.board.construction :as c]
            [emmy.env :as e]
            [hive-dsl.result :as r]))

(defn- close? [a b] (< (Math/abs (- (double a) (double b))) 1e-9))

(def expected
  "The grade of every check, in shelf order."
  {:euclid/I.4 [:grade/numeric :grade/proved :grade/numeric :grade/numeric]
   :euclid/I.5 (repeat 4 :grade/numeric)
   :euclid/I.6 [:grade/numeric :grade/numeric]
   :euclid/I.47 (cons :grade/numeric (repeat 6 :grade/proved))
   :euclid/I.48 [:grade/proved :grade/numeric :grade/numeric]})

(deftest every-new-proposition-grades-as-expected
  (doseq [[id grades] expected]
    (let [graded (el/graded id)]
      (testing (str id)
        (is (= grades (map :grade graded)))
        (is (every? string? (map :label graded)))
        (is (= (map #(if (= :grade/proved %) :emmy :raster) grades) (map :engine graded)))))))

(deftest i-47-is-proved-for-every-right-triangle
  (testing "the windmill's claims are Emmy zeros with A, B and the ratio of the legs symbolic"
    (is (every? #{:grade/proved} (map :grade (rest (el/graded :euclid/I.47))))))
  (testing "the squares are Euclid's: the square on BC as large as the two others"
    (let [{:keys [A B C D E F G H K]} (el/figure :euclid/I.47)
          area (fn [& ps] (Math/abs (double (c/check {:area [:a :b :c :d]}
                                                      {:points (zipmap [:a :b :c :d] (map (fn [[x y]] {:x x :y y}) ps))}))))]
      (is (close? 4 (area B D E C)))
      (is (close? (area B D E C) (+ (area A G F B) (area A C K H)))))))

(deftest the-ops-ported-from-set-theory
  (testing "square-corners turns left or right of A->B"
    (is (= [[1 1] [0 1]] (ops/square-corners [0 0] [1 0] :left)))
    (is (= [[1 -1] [0 -1]] (ops/square-corners [0 0] [1 0] :right))))
  (let [env {:points {:A {:x 0 :y 0} :B {:x 3 :y 4} :L {:x 1 :y 1} :D {:x 2 :y 0} :P {:x 5 :y 0}
                      :C {:x 0 :y 2}}}]
    (testing ":translate carries L along B->D"
      (is (= [0 -3] (:xy (c/point {:op :translate :of :L :by [:B :D]} env)))))
    (testing ":on-ray places AB from D towards P"
      (is (= [7 0] (mapv e/simplify (:xy (c/point {:op :on-ray :from :D :through :P :length [:A :B]} env))))))
    (testing ":copy-angle copies angle BAC at D with DF = AC"
      (let [[x y] (:xy (c/point {:op :copy-angle :angle [:B :A :C] :at [:P :D] :length [:A :C]} env))]
        (is (close? 2 (Math/hypot (- x 2) y)))
        (is (close? (Math/acos (/ 4 5)) (Math/acos (/ (- x 2) 2))))))))

(deftest symbolic-run-keeps-given-points-as-symbols
  (let [env (el/run (el/construction :euclid/I.6) :symbolic)]
    (is (= '[Bx By] [(get-in env [:points :B :x]) (get-in env [:points :B :y])]))
    (is (= [] (:param-syms env)))))

(deftest raster-frame-agrees-with-the-numeric-run
  (let [board (el/construction :euclid/I.47)
        env (el/run board :numeric)
        fig (el/figure :euclid/I.47)]
    (doseq [[id {:keys [x y]}] (:points env)]
      (is (close? x (first (fig id))) (str id))
      (is (close? y (second (fig id))) (str id)))))

(deftest proofs-are-shelf-data
  (let [res (el/proof)]
    (is (r/ok? res))
    (doseq [id [:euclid/I.1 :euclid/I.4 :euclid/I.5 :euclid/I.6 :euclid/I.47 :euclid/I.48]]
      (let [steps (get-in res [:ok id :steps])]
        (is (seq steps) (str id))
        (is (every? (fn [{:keys [claim why stage]}] (and (string? claim) (string? why) (keyword? stage))) steps))))))
