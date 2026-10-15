(ns alexandria.cauchy.playground-test
  (:require [alexandria.cauchy.playground :as pg]
            [clojure.test :refer [deftest is]]))

(deftest ten-experiments-have-reset-and-presets
  (is (= #{:limit :continuity-ch2 :ivt :series :root-test :derivative
           :integral :refinement :fundamental :flat} (set (keys pg/setups))))
  (doseq [[id {:keys [title presets]}] pg/setups]
    (is (seq title) (str id))
    (is (<= 2 (count presets)) (str id))
    (let [widget (pr-str (pg/widget id))]
      (is (re-find #"data-playground" widget) (str id))
      (is (re-find #"Reset to Cauchy's setup" widget) (str id))
      (is (re-find #"Try this" widget) (str id)))))

(deftest continuous-and-discontinuous-options
  (is (contains? pg/functions "jump at 1"))
  (is (contains? pg/functions "flat e^(−1/x²)"))
  (is (contains? pg/sequences "1 + (−1)ⁿ (does not converge)")))
