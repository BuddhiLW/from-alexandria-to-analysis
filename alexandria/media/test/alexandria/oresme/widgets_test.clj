(ns alexandria.oresme.widgets-test
  "Oresme's widgets build on the raster backend and ship wasm kernels; the
   scenes draw every stage from the FnFigure oracle."
  (:require [alexandria.medium.figure :as figure]
            [alexandria.medium.scene :as scene]
            [alexandria.oresme.latitude :as lat]
            [alexandria.oresme.latitude-widget :as lw]
            [alexandria.oresme.mean-speed :as ms]
            [alexandria.oresme.mean-widget :as mw]
            [alexandria.oresme.scenes]
            [alexandria.oresme.series :as series]
            [alexandria.oresme.series-widget :as sw]
            [alexandria.proofs :as proofs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(deftest fragments-build
  (doseq [[nm f] {"latitude" lw/playground "tied" mw/tied "series" sw/playground}]
    (testing nm
      (is (str/includes? (pr-str (f)) "WebAssembly") (str nm " ships a wasm kernel")))))

(deftest scenes-draw
  (let [ctx {:figures {:profile (figure/->FnFigure lat/profile) :summary (figure/->FnFigure lat/summary)
                       :proof (figure/->FnFigure ms/proof-figure) :bar (figure/->FnFigure series/bar-figure)}
             :data {:harmonic (series/player-data)}
             :palette {:ink "#000" :muted "#888" :found "#ff0"} :controls {:v0 0 :v1 4}}]
    (doseq [id [:oresme/latitude :oresme/mean :oresme/harmonic]
            {:keys [stage]} (proofs/steps lw/proofs-resource id)]
      (is (not (str/includes? (pr-str (scene/draw id stage 1 ctx)) "no drawing for")) (str id " " stage)))))

(deftest players
  (is (some? (lw/player)))
  (is (some? (mw/player)))
  (is (some? (sw/player)))
  (is (some? (mw/derivation))))
