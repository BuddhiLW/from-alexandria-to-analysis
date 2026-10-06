(ns alexandria.euler.opera-scenes-test
  (:require [alexandria.euler.graphs :as graphs]
            [alexandria.euler.graphs-scenes]
            [alexandria.euler.opera-view :as view]
            [alexandria.euler.polyhedra :as polyhedra]
            [alexandria.euler.polyhedra-scenes]
            [alexandria.euler.series :as series]
            [alexandria.euler.series-scenes]
            [alexandria.medium.figure :as figure]
            [alexandria.medium.kernel :as kernel]
            [alexandria.medium.scene :as scene]
            [alexandria.palette :as palette]
            [alexandria.proofs :as proofs]
            [clojure.test :refer [deftest is testing]]))

(defn- figures [m] (update-vals m (comp figure/->FnFigure :f)))

(def ^:private cases
  {:euler/koenigsberg {:figures {} :data (view/koenigsberg-data)}
   :euler/basel {:figures (figures series/figures) :data (view/basel-data)}
   :euler/exponential {:figures (figures series/figures) :data {} :controls [{:x 0.5} {:x 3.14159}]}
   :euler/polyhedra {:figures (figures polyhedra/figures) :data (view/polyhedra-data)}})

(deftest every-stage-is-drawn
  (doseq [[id {:keys [figures data controls] :or {controls [{}]}}] cases
          {:keys [stage]} (proofs/steps graphs/proofs-resource id)
          c controls
          p [0 0.37 1]]
    (let [h (scene/draw id stage p {:figures figures :data data :controls c
                                    :palette (palette/palette :deck)})]
      (is (and (vector? h) (not= :text (first h))) (str id " " stage " at " p)))))

(deftest every-figure-compiles-to-a-kernel
  (doseq [[k fig] (merge series/figures polyhedra/figures)]
    (let [{:keys [glue fallback]} (kernel/kernel fig)]
      (is (string? glue) (str k))
      (is (seq fallback) (str k)))))

(deftest the-players-mount
  (testing "each player is a Clerk value carrying its steps"
    (doseq [player [view/koenigsberg view/basel view/exponential view/polyhedra]]
      (is (seq (get-in (player) [:nextjournal/value :steps]))))))

(deftest the-latin-is-quoted
  (doseq [id [:euler/koenigsberg :euler/basel :euler/exponential :euler/polyhedra]]
    (let [{:keys [latin english]} (view/passages id)]
      (is (seq (:passages latin)) (str id))
      (is (= (count (:passages latin)) (count english)) (str id)))))
