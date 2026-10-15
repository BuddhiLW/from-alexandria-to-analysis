(ns alexandria.bernoulli.design-view-test
  (:require [alexandria.bernoulli.design-view :as view]
            [alexandria.design :as design]
            [clojure.test :refer [deftest is]]))

(deftest mathbox-uses-identical-raster-samples-as-print-graph
  (let [d (design/make-design)
        nodes (into {} (map (juxt :id :opts) (:nodes d)))
        pts (:points (nodes :cycloid))
        fragments (#'view/segments pts (view/colours :cycloid))]
    (is (= (dec (count pts)) (count fragments)))
    (is (every? #(re-find #"Parametric|Line" (pr-str %)) (take 2 fragments)))))
