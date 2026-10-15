(ns alexandria.design-test
  (:require [alexandria.design :as d]
            [clojure.test :refer [deftest is testing]]))

(deftest scene-graph-comes-from-raster-and-fits-bed
  (let [{:keys [nodes dimensions times finishing-order constraints] :as design} (d/make-design)
        by-id (into {} (map (juxt :id identity) nodes))
        span (:span-mm constraints)]
    (is (every? #(<= % (:bed-mm constraints)) dimensions))
    (is (= [:cycloid :arc :line] finishing-order))
    (is (= finishing-order (mapv first (sort-by second times))))
    (doseq [kind [:cycloid :arc :line]]
      (let [pts (get-in by-id [kind :opts :points])]
        (is (= (inc (:sample-count constraints)) (count pts)))
        (is (< (Math/abs (double (ffirst pts))) 1e-8))
        (is (< (Math/abs (- span (double (first (last pts))))) 1e-5))
        (is (= :open-u (get-in by-id [kind :opts :profile])))))
    (is (= 3 (count (filter #(= :sphere (:node %)) nodes))))
    (is (every? #(not= :sphere (:node %)) (d/printable-nodes design)))))

(deftest dimensions-guard-printer-bed
  (testing "Impossible dimensions fail before emission"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"exceeds printer bed"
                          (d/make-design {:span-mm 300})))))
