(ns alexandria.ptolemy.widgets-test
  "Ptolemy's widgets build on the raster backend and ship wasm kernels;
   the scenes draw every stage from the FnFigure oracle."
  (:require [alexandria.medium.figure :as figure]
            [alexandria.medium.scene :as scene]
            [alexandria.proofs :as proofs]
            [alexandria.ptolemy.chords :as chords]
            [alexandria.ptolemy.chords-scenes]
            [alexandria.ptolemy.chords-view :as view]
            [alexandria.ptolemy.epicycle-widget :as epw]
            [alexandria.ptolemy.theorem-widget :as thw]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(deftest fragments-build
  (doseq [[nm f] {"tied" thw/tied "playground" thw/playground "epicycle" epw/playground}]
    (testing nm
      (let [s (pr-str (f))]
        (is (str/includes? s "WebAssembly") (str nm " ships a wasm kernel"))))))

(deftest scenes-draw
  (let [ctx {:figures {:half (figure/->FnFigure (:f (:half chords/figures)))
                       :sum (figure/->FnFigure (:f (:sum chords/figures)))}
             :palette {:ink "#000" :muted "#888"} :controls {:beta 72 :alpha 72 :beta2 60}}]
    (doseq [id [:ptolemy/half :ptolemy/sum]
            {:keys [stage]} (proofs/steps view/proofs-resource id)]
      (let [h (scene/draw id stage 1 ctx)]
        (is (not (str/includes? (pr-str h) "no drawing for")) (str id " " stage))))))

(deftest players-and-table
  (is (some? (view/half)))
  (is (some? (view/sum)))
  (is (some? (view/table))))
