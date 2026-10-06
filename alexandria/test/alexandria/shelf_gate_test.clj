(ns alexandria.shelf-gate-test
  (:require [alexandria.library :as library]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [hive-dsl.result :as r]))

(def pending-coordinate-offenders
  ;; TODO: remove entries only when the named shelf is rewritten to vocabulary
  ;; claims or named Desargues/Alexandria checks.
  #{})

(def allowed-claim-ops
  '#{= + - * / expt area length angle ratio segment-area parabola hyperbola ellipse
     circle series-sum square rectangle cube cone sphere cylinder arc sector
     tangent chord diameter radius circumference volume surface-area differential})

(def allowed-checks
  #{:claim :distance :equal-lengths :angle :area :ratio :collinear :concurrent
    :cross-ratio :line-angle :perpendicular :concyclic :angle-sum})

(defn coordinate-access? [form]
  (boolean
   (some #(and (seq? %) (#{'x 'y} (first %)) (= 2 (count %)))
         (tree-seq coll? seq form))))

(defn value-check? [check]
  (or (contains? check :value)
      (and (vector? check) (= :value (first check)))))

(defn unknown-claim-op? [form]
  (boolean
   (some #(and (seq? %) (symbol? (first %)) (not (allowed-claim-ops (first %))))
         (tree-seq coll? seq form))))

(defn check-offense [shelf-id check]
  (cond
    (coordinate-access? check) {:shelf shelf-id :check check :reason :coordinate-access}
    (value-check? check) {:shelf shelf-id :check check :reason :value-check}
    (and (map? check) (:claim check) (unknown-claim-op? (:claim check)))
    {:shelf shelf-id :check check :reason :unknown-claim-op}
    (and (map? check) (not (some allowed-checks (keys check))))
    {:shelf shelf-id :check check :reason :unknown-check}
    :else nil))

(defn proposition-checks [p]
  (concat (:checks p) (:claims p)))

(defn shelf-offenses [catalogue-resource]
  (let [result (library/shelves catalogue-resource)
        shelves (if (r/ok? result) (:ok result) (throw (ex-info "shelves failed" result)))]
    (mapcat (fn [[id p]] (keep #(check-offense id %) (proposition-checks p))) shelves)))

(deftest catalogued-shelves-use-vocabulary-claims-or-named-checks
  (let [offenses (vec (shelf-offenses "alexandria/catalogue.edn"))
        unexpected (remove #(pending-coordinate-offenders (:shelf %)) offenses)]
    (when (seq offenses)
      (println "Pending Alexandria shelf gate offenders:" (pr-str offenses)))
    (is (empty? unexpected))))

(deftest gate-catches-coordinate-fixture
  (let [bad {:claim '(= (x A) (length A B))}]
    (is (= :coordinate-access (:reason (check-offense :fixture/bad bad))))))
