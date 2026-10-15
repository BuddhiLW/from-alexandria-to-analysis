(ns alexandria.cauchy.playground-limits-test
  (:require [alexandria.cauchy.playground :as base]
            [alexandria.cauchy.playground-limits :as limits]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(def ids [:limit :continuity-ch2 :derivative :ivt])

(deftest each-passage-has-its-own-figure
  (let [forms (into {} (for [id ids] [id (pr-str ((limits/figures id)))]))]
    (is (= 4 (count (distinct (vals forms)))))
    (doseq [[id form] forms]
      (testing (name id)
        (is (str/includes? form "mafs.core/MovablePoint") "a draggable object on the figure")))
    (testing "the construction elements each passage names"
      (is (str/includes? (forms :limit) "\"1 + ε\"") "eps band")
      (is (str/includes? (forms :limit) "\"N(ε)\"") "the cut at N")
      (is (str/includes? (forms :limit) "BAND") "the band edge drags")
      (is (str/includes? (forms :continuity-ch2) "halvings") "alpha halved")
      (is (str/includes? (forms :continuity-ch2) "f(x+α) − f(x)"))
      (is (str/includes? (forms :derivative) "mafs.line/ThroughPoints") "the secant")
      (is (str/includes? (forms :derivative) "slope") "the tangent")
      (is (str/includes? (forms :ivt) "brackets") "bracketing intervals")
      (is (str/includes? (forms :ivt) "levels") "refined round after round"))))

(deftest presets-break-a-hypothesis
  (doseq [id ids]
    (testing (name id)
      (let [ps (limits/presets id)
            fs (if (= id :limit) base/sequences base/functions)]
        (is (<= 2 (count ps) 4))
        (is (str/starts-with? (first (last ps)) "Break it"))
        (doseq [[_ patch] ps :when (:f patch)]
          (is (contains? fs (:f patch)) "preset names a known function"))))))

(deftest raster-backed-widgets
  (doseq [id ids]
    (testing (name id)
      (let [form (pr-str (limits/widget id))]
        (is (str/includes? form "mafs.core/MovablePoint"))
        (is (str/includes? form "cauchy-limit-k-") "raster kernels bound once")
        (is (str/includes? form "reagent.core/cursor") "points write the section state")
        (is (str/includes? form "data-case"))
        (is (str/includes? form "Reset to Cauchy"))
        (is (not (re-find #"\bFIGURE\b|\bLEVA\b|\bPRESETS\b" form)) "every placeholder filled")))))
