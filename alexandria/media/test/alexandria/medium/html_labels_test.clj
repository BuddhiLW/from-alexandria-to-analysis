(ns alexandria.medium.html-labels-test
  "No MathBox text in the apollonius scenes: every letter is an HTML label
   of alexandria.medium.html-labels (MathBox's Format/Label reads glyphs
   back from a 2D canvas, which anti-fingerprinting browsers answer with
   noise). The browser half is history-of-math/dev/labels_e2e.mjs."
  (:require [alexandria.apollonius.cone-widget :as w]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.walk :as walk]))

(defn- collect [pred form]
  (let [acc (volatile! [])]
    (walk/postwalk (fn [x] (when (pred x) (vswap! acc conj x)) x) form)
    @acc))

(deftest the-cone-widget-letters-are-html
  (let [frag (w/cone)
        points (collect #(and (vector? %) (= 'emmy.mathbox.components.plot/Point (first %))) frag)
        overlays (collect #(and (vector? %) (= 'alexandria.medium.html-labels/overlay (first %))) frag)]
    (testing "MathBox draws the dots only"
      (is (seq points))
      (is (not-any? #(contains? (second %) :label) points)))
    (testing "one overlay carries Apollonius' letters, bound to the scene's MathBox"
      (is (= 1 (count overlays)))
      (let [{:keys [box range labels]} (second (first overlays))
            texts (set (collect string? labels))]
        (is (= 'box box))
        (is (= w/scene-range range))
        (is (every? texts ["A" "B" "C" "B′" "C′" "P" "M" "D" "E" "V" "Q" "Q′" "H" "K" "L" "P′" "F"]))))
    (testing "the scene hands its MathBox to the overlay, and draws no axis ticks"
      (is (seq (collect #(= '(fn [b] (reset! box b)) %) frag)))
      (is (seq (collect #(and (map? %) (= [] (:axes %))) frag))))))

(deftest no-mathbox-text-in-the-browser-scenes
  (doseq [f ["alexandria/apollonius/notation_steps.cljc" "alexandria/apollonius/conics_3d.cljs"]
          :let [src (slurp (io/resource f))]]
    (testing f
      (is (not (str/includes? src "mb/Label")))
      (is (not (str/includes? src "mb/Format")))
      (is (str/includes? src "labels/overlay"))
      (is (str/includes? src "[alexandria.medium.html-labels :as labels]")))))
