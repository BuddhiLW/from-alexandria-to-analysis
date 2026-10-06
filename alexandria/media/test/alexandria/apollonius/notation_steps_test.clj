(ns alexandria.apollonius.notation-steps-test
  (:require [alexandria.apollonius.notation-steps :as ns-steps]
            [alexandria.apollonius.notation-steps-view :as view]
            [alexandria.medium.figure :as figure]
            [alexandria.medium.kernel-oracle :as ko]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(def notebook
  "The notebook the passage is quoted from, relative to alexandria/media."
  "../../history-of-math/notebooks/apollonius_conics.clj")

(defn- squash [s] (str/trim (str/replace s #"\s+" " ")))

(deftest the-passage-is-eight-sentences-quoted-verbatim
  (let [{:keys [steps]} (ns-steps/passage)]
    (is (= (mapv #(str "S" %) (range 1 9)) (mapv :id steps)))
    (is (= ns-steps/step-count (count steps) (count ns-steps/durations)))
    (when (.exists (io/file notebook))
      (let [prose (->> (str/split-lines (slurp notebook))
                       (keep #(second (re-matches #"\s*;;\s?-?\s?(.*)" %)))
                       (str/join " ")
                       squash)]
        (doseq [{:keys [id text]} steps]
          (is (str/includes? prose (squash text)) (str id " is quoted from the notebook")))))))

(deftest each-step-draws-exactly-what-its-sentence-introduces
  (let [{:keys [steps]} (ns-steps/passage)
        drawn (mapv #(set (remove ns-steps/motions (keys %))) ns-steps/schedule)]
    (doseq [[i {:keys [id introduces]}] (map-indexed vector steps)]
      (is (= (set introduces) (drawn i)) id))
    (testing "nothing is introduced twice"
      (is (apply distinct? (mapcat :introduces steps))))
    (testing "at the end of step i every element of steps <= i is complete, none later has begun"
      (doseq [i (range ns-steps/step-count)
              :let [v (ns-steps/progress-of {:step i :progress 1})]]
        (doseq [j (range ns-steps/step-count) key (keys (ns-steps/schedule j))]
          (is (== (if (<= j i) 1 0) (v key)) (str "step " (inc i) " " key)))))))

(deftest the-transport
  (let [st (ns-steps/start)]
    (testing "a step played alone stops at its end"
      (let [end (ns-steps/advance st 1e9)]
        (is (= [0 1 false] [(:step end) (:progress end) (:playing? end)]))))
    (testing "play runs on through the steps"
      (let [st (ns-steps/toggle (ns-steps/advance st 1e9))]
        (is (= 1 (:step st)))
        (is (= 2 (:step (ns-steps/advance st 1e9))))))
    (testing "goto clamps and plays once"
      (is (= 7 (:step (ns-steps/goto st 99))))
      (is (= 0 (:step (ns-steps/goto st -3))))
      (is (false? (:through? (ns-steps/goto st 3)))))
    (testing "seek pauses"
      (is (= [0.4 false] ((juxt :progress :playing?) (ns-steps/seek st 0.4)))))))

(def data (ns-steps/data))

(deftest the-tilt-is-the-parabolas-until-s8
  (let [at (fn [step p] (:theta (ns-steps/scene-state {:step step :progress p} data)))]
    (doseq [i (range 7)]
      (is (== (:theta-par data) (at i 1))))
    (is (< (Math/abs (- (:theta-ell data) (at 7 0.5))) 1e-12) "S8 holds at the ellipse")
    (is (< (Math/abs (- (:theta-hyp data) (at 7 1))) 1e-12) "S8 ends at the hyperbola")))

(defn- named [theta]
  (let [{:keys [k h zb ztop x]} data]
    (ns-steps/named-map (first (figure/points (figure/->FnFigure ns-steps/named-figure)
                                              [k theta h zb ztop x] [[0]])))))

(defn- space "MathBox axes back to [X Y Z]." [[a b c]] [a c (- b)])
(defn- dist [u v] (Math/sqrt (reduce + (map #(let [d (- %1 %2)] (* d d)) u v))))
(defn- close? [a b] (< (Math/abs (- a b)) 1e-9))

(deftest the-lettered-points-are-apollonius-construction
  (let [{:keys [k zb x theta-par theta-ell theta-hyp]} data
        on-cone? (fn [q] (let [[X Y Z] (space q)] (close? (+ (* X X) (* Y Y)) (* k k Z Z))))]
    (testing "the parabola: D, E on the base circle; Q on the cone; y^2 = p x; PL = p at right angles to PM"
      ;; a hair off the tilt: at exactly theta-par d = PP' is a division by zero
      (let [{:keys [A B C M D E P V Q L w wq p y y2 px]} (named (+ theta-par 1e-12))]
        (is (= [0.0 0.0 0.0] (mapv double A)))
        (is (and (pos? w) (pos? wq)))
        (is (every? on-cone? [B C D E P Q]))
        (is (close? (second (space M)) 0) "M on BC, in the axial plane")
        (is (close? (nth (space M) 2) zb))
        (is (close? (dist V Q) y))
        (is (< (Math/abs (- y2 px)) 1e-9) "QV^2 = p.PV")
        (is (close? (* p x) px))
        (is (close? (dist P L) p))
        (let [dot (reduce + (map * (map - L P) (map - M P)))]
          (is (< (Math/abs dot) 1e-9) "PL is at right angles to PM"))))
    (testing "the ellipse: P' on the side AC, same nappe; the hyperbola: on AC produced past A"
      (let [{P' :P' d :d c :c} (named theta-ell)
            [X _ Z] (space P')]
        (is (neg? c))
        (is (and (pos? d) (pos? Z) (close? X (- (* k Z))))))
      (let [{P' :P' P :P d :d c :c C' :C'} (named theta-hyp)
            [X _ Z] (space P')]
        (is (pos? c))
        (is (and (neg? d) (neg? Z) (close? X (- (* k Z)))))
        (is (close? (dist P P') (Math/abs d)))
        (is (<= (- (:ztop data)) Z) "P' lies within the drawn upper nappe")
        (is (close? (nth (space C') 2) (- (:ztop data))))))))

(deftest the-constants-come-from-raster
  (let [{:keys [source ratio theta-par k]} data]
    (is (= :raster source))
    (is (< (Math/abs (- theta-par (Math/atan2 1 k))) 1e-4))
    (is (< (Math/abs (- (:pl-pa ratio) (:bc2-ba-ac ratio))) 1e-4) "I.11: PL : PA = BC^2 : BA.AC")))

(deftest the-sentences-parse
  (is (= [[:t "The "] [:b "axial triangle"] [:t " "] [:m "ABC"] [:t " is"]]
         (ns-steps/segments "The **axial triangle** $ABC$ is")))
  (is (= [[:m "d = PP′"]] (ns-steps/segments "$d = PP'$")))
  (is (= [[:t "Latin "] [:i "latus rectum"]] (ns-steps/segments "Latin *latus rectum*"))))

(def ^:private oracle-cases
  (let [{:keys [k h zb ztop x theta-par theta-ell theta-hyp]} data
        us (mapv (fn [i] [(- (* 0.17 i) 0.95)]) (range 12))]
    {:cone [[[k 0.6 0 zb 1] [[0 0] [0.5 0.5] [1 1]]]
            [[k 1 0 (- ztop) 0.4] [[0.3 0.2] [1 1]]]]
     ;; the section where the scene draws it: points beyond the drawn box (the
     ;; far reach of a parabola or hyperbola, |Z| large) are clipped there
     :section (vec (for [params [[k theta-par h 1] [k theta-ell h 1] [k theta-hyp h 0.7]]
                         :let [pts (figure/points (figure/->FnFigure ns-steps/section-figure) params us)]]
                     [params (vec (keep (fn [[u [_ y _]]] (when (<= (:y-lo data) y (:y-hi data)) u))
                                        (map vector us pts)))]))
     :plane [[[k theta-par h 0.3 1.4 -0.7 3.2 1.7] [[0 0] [1 1] [0.4 0.6]]]]
     :segment [[[0.1 -0.2 0.3 1.2 0.4 -0.5 0.6] [[0] [0.5] [1]]]]
     :named [[[k 0.7 h zb ztop x] [[0]]] [[k theta-ell h zb ztop x] [[0]]]
             [[k theta-hyp h zb ztop x] [[0]]]]}))

(deftest every-kernel-matches-its-emmy-oracle
  (is (= (set (keys ns-steps/figures)) (set (keys oracle-cases))))
  (doseq [[id cases] oracle-cases
          [params states] cases]
    (when-let [{:keys [wasm max-error nan-states]} (ko/deviation (ns-steps/figures id) params states)]
      (is wasm (str id))
      (is (empty? nan-states) (str id " " nan-states))
      (is (< max-error 1e-4) (str id " " max-error)))))

(deftest the-clerk-value
  (let [{:keys [steps kernels data]} (view/value)]
    (is (= 8 (count steps)))
    (is (= (set (keys ns-steps/figures)) (set (keys kernels))))
    (is (every? (comp string? :glue) (vals kernels)))
    (is (= :raster (:source data)))))
