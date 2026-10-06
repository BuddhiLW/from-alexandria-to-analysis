(ns alexandria.descartes.geometrie-view
  "La Geometrie as proof players for a Clerk notebook: each function returns
   a Clerk value, the steps (alexandria.descartes.geometrie/proof) played
   over the scenes of alexandria.descartes.geometrie-scenes."
  (:require [alexandria.descartes.geometrie :as g]
            [alexandria.medium.clerk :as medium]
            [alexandria.proofs :as proofs]
            [hive-dsl.result :as r]
            [alexandria.raster :as raster]))

(def ^:private render-fn 'alexandria.descartes.geometrie-scenes/render)

(defn section
  "A section's data (:passage :source :steps), or nil."
  [id]
  (let [res (g/proof)] (when (r/ok? res) (get-in res [:ok id]))))

(defn- steps [id] (proofs/steps g/proofs-resource id))

(defn unit
  "Book I: product, quotient and square root with the unit; sliders for the
   two segments BD = a and BC = b. Every point is a raster kernel."
  []
  (medium/player render-fn :descartes/unit (steps :descartes/unit)
                 (select-keys g/figures [:multiplication :square-root :root-arc]) {}
                 {:window [[-1.4 5.6] [-1.3 3.2]] :height 360
                  :controls [{:id :a :label "BD = a" :min 0.3 :max 3 :step 0.05 :init 2}
                             {:id :b :label "BC = b" :min 0.3 :max 2 :step 0.05 :init 1.5}]
                  :durations {:multiply 6000 :similar 7000 :root 6000 :mean 6000}}))

(defn pappus-data
  "The lines (exact rationals, as doubles) and the two parabola ratios (the
   discriminant's roots, from a raster kernel)."
  []
  {:lines (mapv (fn [l] (update-vals l #(mapv double %))) g/pappus-lines)
   :parabola-at (:roots (g/parabola-ratios))})

(defn pappus
  "Pappus' four lines: the point C runs on the locus with its four oblique
   lines drawn; a slider for the ratio lam. For these lines the locus is an
   ellipse for lam between the two parabola ratios (about -19.3 and
   -0.005) and a hyperbola outside them."
  []
  (medium/player render-fn :descartes/pappus (steps :descartes/pappus)
                 (select-keys g/figures [:pappus :pappus-foot :pappus-products :pappus-discriminant]) (pappus-data)
                 {:window [[-1.2 7.2] [-1.4 4.6]] :height 420
                  :controls [{:id :lam :label "ratio lam" :min -24 :max 3 :step 0.05 :init 1}]
                  :durations {:distances 6000 :equation 8000 :locus 9000 :classify 6000 :ratio 10000}}))

(defn normal
  "Book II: the circle about P cuts the parabola twice, then touches it."
  []
  (medium/player render-fn :descartes/normal (steps :descartes/normal)
                 (select-keys g/figures [:normal :normal-meet :parabola]) {}
                 {:window [[-1.6 3.2] [-1.8 1.95]] :height 360
                  :durations {:two-roots 6000 :touching 6000}}))

(defn signs
  "Book III: the quartic, its roots, and the count of its signs."
  []
  (medium/player render-fn :descartes/signs (steps :descartes/signs)
                 (select-keys g/figures [:quartic]) {}
                 {:window [[-6.4 5.6] [-2.4 3.0]] :height 320 :durations {:count 8000}}))

(defn plane
  "Book I, figs. 3 and 4: the plane problems by one right triangle and one
   circle; sliders for a and b."
  []
  (medium/player render-fn :descartes/plane (steps :descartes/plane)
                 (select-keys g/figures [:plane-root :chord-root :circle]) {}
                 {:window [[-1.2 4.8] [-0.8 3.4]] :height 360
                  :controls [{:id :a :label "a" :min 0.4 :max 3 :step 0.05 :init 2}
                             {:id :b :label "b" :min 0.3 :max 2 :step 0.05 :init 1}]
                  :durations {:produce 6000 :chord 6000 :none 8000}}))

(defn ellipse
  "Book II on Descartes' own ellipse (r = 2, q = 4, C at MA = 1): the circle
   about P cuts the ellipse at C and E, then touches it at v = 3/2."
  []
  (medium/player render-fn :descartes/ellipse (steps :descartes/ellipse)
                 (select-keys g/figures [:ellipse :ellipse-circle :ellipse-meet]) {}
                 {:window [[-0.6 4.8] [-1.9 2.1]] :height 360
                  :durations {:cut 6000 :touch 6000}}))

(defn construction-data
  "The two cases of fig. 27 the scenes draw, with their roots from raster:
   the trisection of the arc 2 (z^3 = 3z - q) and two means between 1 and 2
   (z^3 = 2)."
  []
  (let [{:keys [mean trisection]} (g/book-3-numbers)
        {:keys [q gk GK FL]} trisection]
    {:trisection (merge trisection {:p 3 :r 0 :roots [gk GK FL]})
     :means {:p 0 :q -2 :r 0 :roots [(:root mean)] :given (:given mean)}}))

(defn construction
  "Book III, fig. 27: the parabola and the circle that meet at the roots,
   Descartes' demonstration, then figs. 28 and 30."
  []
  (medium/player render-fn :descartes/construction (steps :descartes/construction)
                 (select-keys g/figures [:parabola-iii :construction-circle]) (construction-data)
                 {:window [[-3.2 3.2] [-0.6 4.9]] :height 420
                  :durations {:means 8000 :three 8000}}))
