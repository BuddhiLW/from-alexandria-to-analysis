(ns alexandria.formula
  "Frozen historical formulas: Emmy expressions plus source passage metadata."
  (:refer-clojure :exclude [freeze])
  (:require [alexandria.notation :as notation]
            [clojure.walk :as walk]
            [emmy.env :as e]
            [malli.core :as m]))

(def Formula
  [:map
   [:name symbol?]
   [:expr any?]
   [:bindings [:map-of symbol? any?]]
   [:passage [:map
              [:author keyword?]
              [:work keyword?]
              [:locus string?]
              [:quote string?]]]])

(defn freeze
  "Validate and freeze a formula map's Emmy expression."
  [formula]
  (let [f (update formula :expr e/freeze)]
    (when-not (m/validate Formula f)
      (throw (ex-info "Invalid Alexandria formula" {:formula f :explain (m/explain Formula f)})))
    f))

(defn thaw
  "Return the Emmy value represented by formula, with bindings substituted."
  [formula]
  (let [{:keys [expr bindings]} (freeze formula)]
    (walk/postwalk #(if (and (symbol? %) (contains? bindings %)) (bindings %) %) expr)))

(defn render [formula notation]
  (notation/render-expr notation (thaw formula)))
