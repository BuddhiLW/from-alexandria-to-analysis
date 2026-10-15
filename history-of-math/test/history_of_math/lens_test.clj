(ns history-of-math.lens-test
  (:require [clojure.test :refer [deftest is testing]]
            [history-of-math.build :as build]
            [history-of-math.lens :as lens]
            [history-of-math.links :as links]
            [history-of-math.timeline :as timeline]
            [hive-dsl.result :as r]
            [malli.core :as m]))

(deftest registry-and-schema
  (let [registry (lens/series)]
    (is (= :live (get-in registry [:erlangen :status])))
    (is (= "/clojure-on-zig-and-rust/" (get-in registry [:two-runtimes :url])))
    (is (= :planned (get-in registry [:group-theory :status])))
    (doseq [id (lens/ids)]
      (testing id
        (is (m/validate lens/Lens (lens/read-lens id)))))))

(deftest references
  (let [notebooks (build/discover build/notebooks-dir)]
    (is (r/ok? notebooks))
    (let [ids (set (map :id (:ok notebooks)))]
      (doseq [id (lens/ids)]
        (is (empty? (lens/validate-refs id (lens/read-lens id) ids)))))))

(deftest routes-and-arcs
  (is (= "../euler/" (links/notebook-href :notebook "euler")))
  (is (= "euler/" (links/notebook-href :hub "euler")))
  (let [t (:ok (timeline/timeline))
        notebooks (:ok (build/discover build/notebooks-dir))
        joined (timeline/join t notebooks)
        edges (timeline/influence-edges joined)]
    (is (some #(and (= "alhazen-optics" (:from %)) (= "kepler-laws" (:to %))) edges))
    (is (some #(and (= "alhazen-optics" (:notebook %)) (:built? %)) joined))
    (is (some #(and (= "apollonius-conics" (:from %)) (= "kepler-laws" (:to %))) edges))
    (is (some #(and (= "euler" (:from %)) (= "lagrange" (:to %))) edges))
    (let [data (timeline/interactive-data t joined)]
      (is (seq (:edges data)))
      (is (some #{:mechanics-physics} (:fields data)))
      (is (every? number? (map :x (:nodes data)))))))
