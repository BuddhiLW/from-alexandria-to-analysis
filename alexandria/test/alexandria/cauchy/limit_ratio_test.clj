(ns alexandria.cauchy.limit-ratio-test
  (:require [alexandria.cauchy.limit :as lim]
            [alexandria.cauchy.ratio :as ra]
            [clojure.test :refer [deftest is testing]]))

(def ok #{:grade/proved :grade/numeric})

(deftest the-eps-delta-tie-is-graded
  (let [gs (lim/grades)]
    (is (= 14 (count gs)))
    (doseq [{:keys [label grade]} gs] (is (ok grade) label))
    (testing "the closed forms are proved, the searches numeric"
      (is (= 10 (count (filter #(= :grade/proved (:grade %)) gs)))))))

(deftest the-search-agrees-with-emmys-root
  (doseq [a [0.4 1.0 2.2] eps [0.05 0.3 1.5]]
    (is (< (Math/abs (- (:delta (lim/search :square a eps)) (- (Math/sqrt (+ (* a a) eps)) a))) 1e-9))
    (is (< (Math/abs (- (:delta (lim/search :reciprocal a eps)) (/ (* eps a a) (+ 1 (* eps a))))) 1e-9))))

(deftest the-searched-window-keeps-the-graph-in-the-band
  (doseq [id [:square :reciprocal :sine] a [0.5 1.3] eps [0.1 0.4]]
    (let [{:keys [delta]} (lim/search id a eps)]
      (is (<= (lim/worst-value id a delta) (+ eps 1e-9)) (str id " " a " " eps))
      (is (> (lim/worst-value id a (* 1.01 delta)) eps) (str id " is the largest window")))))

(deftest at-the-jump-no-window-works
  (testing "eps below the jump: delta closes on 0 at a = 1 (down to the 1e-12 scale of the smoothed step)"
    (is (< (:delta (lim/search :jump 1.0 0.5)) 1e-10)))
  (testing "away from the jump the window reaches it and stops"
    (is (< (Math/abs (- (:delta (lim/search :jump 1.4 0.3)) 0.4)) 1e-9))))

(deftest root-and-ratio-are-graded
  (doseq [{:keys [label grade]} (ra/grades)] (is (ok grade) label)))

(deftest the-u-n-tie-round-trips
  (doseq [[id Us] {:quad [0.55 0.6 0.75 0.9] :e [0.1 0.3 0.6] :growth [1.2 1.5 1.9]}
          U Us
          :let [rows (@ra/tables id) k (get-in ra/series [id :k])
                N (ra/first-from rows k U)
                U' (ra/greatest-from rows k N)]]
    (is (some? N) (str id " " U))
    (is (if (= 1 (ra/side k)) (<= U' U) (>= U' U)))
    (is (<= (ra/first-from rows k U') N))
    (testing "and every root from N on is on U's side"
      (is (every? #(if (= 1 (ra/side k)) (<= (:root %) U) (>= (:root %) U)) (drop (dec N) rows))))))

(deftest no-u-when-k-is-one
  (is (nil? (ra/side 1)))
  (is (nil? (ra/u-range 1))))
