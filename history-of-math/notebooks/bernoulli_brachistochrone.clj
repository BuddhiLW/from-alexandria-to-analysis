;; # Bernoulli's challenge: the fastest descent
;;
;; In June 1696 Johann Bernoulli published a challenge in the Leipzig
;; *Acta Eruditorum*. A bead starts from rest at A and falls under gravity
;; to B. Which path takes the least time? Galileo's circle arc beats the
;; straight chord; Bernoulli's answer is the cycloid. This problem connects
;; Newton and Leibniz to Euler's and Lagrange's calculus of variations.
^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns bernoulli-brachistochrone
  {:history/year 1696 :history/title "Bernoulli: the brachistochrone and a marble race"
   :history/era "Early calculus"
   :history/fields #{:analysis :mechanics-physics} :nextjournal.clerk/toc true}
  (:require [alexandria.bernoulli.brachistochrone :as b]
            [alexandria.bernoulli.design-view :as design-view]
            [alexandria.bernoulli.race-widget :as race]
            [alexandria.design :as design]
            [history-of-math.assets :as assets]
            [history-of-math.page :as page]
            [history-of-math.widgets :as widgets]
            [nextjournal.clerk :as clerk]))

^{::clerk/visibility {:code :hide :result :hide}} (widgets/install!)

;; ## The invitation, 1696
;;
;; > *Datis in plano verticali duobus punctis A et B assignare mobili M viam
;; > AMB, per quam gravitate sua descendens et moveri incipiens a puncto A,
;; > brevissimo tempore perveniat ad alterum punctum B.*
;; >
;; > Given two points A and B in a vertical plane, assign the moving body M
;; > a path AMB along which it descends under its own weight from A to B in
;; > the shortest time. (English rendering: this series.)
;;
;; Johann Bernoulli, *Problema novum ad cujus solutionem mathematici
;; invitantur*, Acta Eruditorum, June 1696, p. 269. The letters A, M, B
;; name the start, moving body, and destination. Take A = (0, 0), depth y
;; positive downward; B = (X, Y). A wheel of radius r rolling through θ
;; gives x = r(θ − sin θ), y = r(1 − cos θ).

;; ## From the time integral to a cycloid
;;
;; The bead's speed after dropping y is √(2gy). A short distance
;; ds = √(1 + y′²) dx therefore takes dt = ds/√(2gy). Integrate to get
;; T[y] = ∫√((1 + y′²)/(2gy)) dx. Emmy differentiates this Lagrangian;
;; since it has no explicit x, the Beltrami first integral stays constant.
;; Substituting the cycloid into y(1 + y′²) gives 2r, exactly.

(race/derivation)

(clerk/html (into [:div {:class "brach-grades"}]
                  (for [{:keys [label grade]} (concat (b/graded) (race/grades))]
                    [:p [:strong (name grade)] " — " label])))

;; ## Move B; race the three paths
;;
;; Change X with Y held fixed, or Y with X fixed. Changing r or θ holds
;; the other fixed and moves B. Changing T holds θ fixed and changes r.
;; The panel pushes every solved value back without re-triggering itself.
;; Brent solves the endpoint on the JVM; the live page uses raster-compiled
;; Newton steps. Red is the cycloid, blue Galileo's circle arc, grey the
;; straight chord. The beads interpolate the same samples used for timing.

(race/widget)

;; ## From an objective to a physical track
;;
;; The objective is the time integral. Constraints: shared A and B, a
;; 12 mm marble, a 2 mm wall, and a 256 mm printer bed. Raster samples
;; the paths; a scene graph computes the U-channels and base. Ideal sliding
;; times do not predict the rolling time of real steel marbles: rotational
;; inertia and friction are excluded from the variational model.

;; ## Orbit the computed printable scene
;; The red, blue and grey rails are exactly the samples passed to Blender.
;; Drag to rotate, scroll to zoom. The three lanes share A and B in projection.
(design-view/scene)

(let [d (design/make-design)]
  (clerk/html [:div
               [:p "Computed finish order: " (pr-str (:finishing-order d))]
               [:p "Cycloid (red): 0.795 s ideal, 0.883 s Bullet; circle (blue): 0.820 s ideal, 0.967 s Bullet; straight (green): 0.997 s ideal, 1.067 s Bullet. The rolling trial agrees on the order, not the ideal times."]
               [:video {:src (assets/data-url "brachistochrone-race.mp4" "video/mp4") :controls true :muted true :plays-inline true :style {:max-width "100%"}}]
               [:p "Ideal sliding times (s): " (pr-str (:times d))]
               [:p [:a {:href (assets/data-url "brachistochrone-race.glb" "model/gltf-binary") :download "brachistochrone-race.glb"} "GLB preview"]]
               [:img {:src (assets/data-url "brachistochrone-race.png" "image/png") :alt "Cycloid, circular arc and straight U-channel tracks, with finish stripes and catch tray" :style {:max-width "100%"}}]]))

;; The full-size watertight STL (28 MB) is not shipped; target/ is git-ignored.
;; Regenerate it from the worktree root (one runtime at a time):
;;   cd alexandria && clojure -J-Xmx2g -Sdeps '{:deps {org.clojure/data.json {:mvn/version "2.5.2"}}}' -M -e '(require (quote alexandria.design) (quote clojure.data.json)) (spit "../target/design.json" (clojure.data.json/write-str (alexandria.design/make-design)))' && cd ..
;;   flatpak run --filesystem="$PWD" org.blender.Blender -b --factory-startup --python "$PWD/alexandria/media/scripts/brachistochrone_blender.py" -- "$PWD/target/design.json" "$PWD/target/design" --export
;; This writes target/design/brachistochrone-race.{stl,glb,png}; add --animation
;; for the 72 frames behind the MP4 (ffmpeg line in the script docstring).

(page/prev-next "notebooks/bernoulli_brachistochrone.clj")
