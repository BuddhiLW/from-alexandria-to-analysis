(ns alexandria.medium.timeline-test
  (:require [alexandria.medium.timeline :as tl]
            [clojure.test :refer [deftest is testing]]))

(def steps [{:stage :a} {:stage :b} {:stage :c}])

(deftest advance
  (let [t (tl/toggle (tl/timeline steps {:a 1000}))]
    (testing "progress runs in the step's own duration"
      (is (== 0.5 (:progress (tl/advance t 500))))
      (is (== 0.125 (:progress (tl/advance (tl/goto t 1) 500)))))
    (testing "rolls into the next step, stops at the end"
      (is (= 1 (:step (tl/advance t 1000))))
      (is (tl/ended? (tl/advance (tl/goto t 2) 9000)))
      (is (not (:playing? (tl/advance (tl/goto t 2) 9000)))))
    (testing "paused does not move"
      (is (== 0 (:progress (tl/advance (tl/toggle t) 500)))))))

(deftest goto-seek-toggle
  (let [t (tl/timeline steps {})]
    (is (= 2 (:step (tl/goto t 99))))
    (is (= 0 (:step (tl/goto t -1))))
    (is (= {:progress 1 :playing? false} (select-keys (tl/seek (tl/toggle t) 7) [:progress :playing?])))
    (is (:playing? (tl/toggle (assoc (tl/seek (tl/goto t 2) 1) :playing? false))))))

(deftest stages-split-progress
  (is (= [(double 0) (double 0)] (mapv double (tl/stages 0 3))))
  (is (= [(double 2) (double 1)] (mapv double (tl/stages 1 3))))
  (is (= 1 (first (tl/stages 0.5 3)))))
