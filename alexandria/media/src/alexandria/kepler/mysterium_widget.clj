(ns alexandria.kepler.mysterium-widget
  "Kepler's nest of solids in space (Mysterium Cosmographicum, 1596), as an
   emmy-viewers MathBox widget on the raster backend, with tied controls
   (widget brief, section 5; the bar is the Apollonius conics playground).

   The six planetary spheres are six Leva controls tied by the solids:

     r_inner = (r/R)_solid r_outer      (alexandria.kepler.mysterium/ratio)

   so every radius is r_Saturn times the exact chain of ratios outside it
   (alexandria.kepler.mysterium/chain). Move any radius and the other five
   follow: the touched one is clamped to its range, Saturn's radius is
   solved back (saturn-of-radius, linear), and every other radius is
   recomputed forward. One raster kernel per source block, mounted with
   alexandria.medium.tie (origin and epsilon guards).

   The scene: the six spheres translucent, each solid drawn by its edges
   inscribed in the sphere outside it, so its faces touch the sphere
   inside it; in the equatorial plane, dashed rings at Copernicus' radii
   (Kepler's table, Dreyer 1906 p. 375) beside each sphere's own equator,
   and Mercury's ring on the octahedron's midsphere. A shell slider picks
   one solid: it is drawn bold, named live (English and Greek), and its
   relation is shown with the current numbers substituted.

   Every number on the page comes from raster: the tie and the readout
   are wasm kernels called in the browser, the unit vertices are the exact
   Emmy vertices of alexandria.solids evaluated by alexandria.raster on the
   JVM, and the spheres and rings are compiled by emmy-viewers' MathBox
   plot on *backend* :raster."
  (:require [alexandria.kepler.mysterium :as my]
            [alexandria.medium.tie :as tie]
            [alexandria.raster :as raster]
            [alexandria.solids :as solids]
            [clojure.walk :as walk]
            [emmy.clerk :as ec]
            [emmy.env :as e]
            [emmy.leva :as leva]
            [emmy.mathbox.plot :as plot]
            [emmy.viewer :as ev]
            [emmy.viewer.compile :as vc]
            [emmy.viewer.raster]
            [nextjournal.clerk :as clerk]))

;; ---------------------------------------------------------------------------
;; The tie

(def tied
  "The six radii, outside in (alexandria.kepler.mysterium/planets)."
  my/planets)

(def chain-value
  "r_i / r_Saturn as doubles, by raster (the exact chain's kernel)."
  (memoize (fn [i] (raster/value (fn [] (my/chain i))))))

(def saturn-range "Saturn's slider [min max step]." [0.4 1.0 0.001])

(def ranges
  "[min max step] per planet: Saturn's range carried in by the chain, so a
   clamp of the touched key alone keeps every other key in its range."
  (into {}
        (map-indexed (fn [i k]
                       (let [c (chain-value i) [lo hi st] saturn-range]
                         [k [(* lo c) (* hi c) (if (zero? i) st (if (< c 0.1) 0.0002 (if (< c 0.3) 0.0005 st)))]]))
                     tied)))

(def labels
  {:saturn "r Saturn" :jupiter "r Jupiter" :mars "r Mars" :earth "r Earth" :venus "r Venus" :mercury "r Mercury"})

(defn- lo [k] (first (ranges k)))
(defn- hi [k] (second (ranges k)))

(defn from-source
  "The solved radii when planet i is the source: its radius r clamped to
   its range, Saturn's solved back, every radius forward."
  [i r]
  (let [k (tied i)
        rs (my/saturn-of-radius i (tie/clamp r (lo k) (hi k)))]
    (mapv #(my/radius-of-saturn % rs) (range 6))))

(defn tie
  "The tie kernel: for the six radii, the solved state for each source in
   the order of `tied` (36 numbers)."
  [r0 r1 r2 r3 r4 r5]
  (fn [_] (vec (mapcat (fn [i r] (from-source i r)) (range 6) [r0 r1 r2 r3 r4 r5]))))

(defn solve
  "The tie kernel at a state (six radii), by raster on the JVM:
   {source [six radii]}."
  [state]
  (let [v (vec (binding [raster/*simplify?* false]
                 (apply raster/value (fn [a b c d f g] ((tie a b c d f g) 0)) state)))]
    (tie/blocks tied v)))

(def readout-index
  "Where each number sits in the readout kernel's output."
  {:kepler 0 :copernicus 6 :ratio 12 :copernicus-ratio 17 :miss 22 :mercury-mid 27})

(defn readout
  "The page's numbers for Saturn's radius rs, by raster: Kepler's six
   radii, Copernicus' six (his table's factors), the five ratios r/R, the
   five Copernican ratios, the five misses (r/R - c)/c, and Mercury on the
   octahedron's midsphere."
  [rs]
  (fn [_]
    (let [cop (map #(/ (:copernicus %) 1000) my/kepler-table)]
      (vec (concat (map #(my/radius-of-saturn % rs) (range 6))
                   (map #(e/* rs %) my/copernicus-chain)
                   (map (fn [[_ id _]] (my/ratio id)) my/shells)
                   cop
                   (map (fn [[_ id _] c] (e// (e/- (my/ratio id) c) c)) my/shells cop)
                   [(e/* rs (my/chain 5 :mid))])))))

(defn init
  "The starting state: Saturn's sphere 0.9, the rest by the tie (raster)."
  []
  (zipmap tied ((solve (vec (repeat 6 0.9))) :saturn)))

;; ---------------------------------------------------------------------------
;; The solids, as unit wireframes (circumradius 1), by raster

(defn unit-solid
  "{:vertices [[x y z] ..] :edges [[i j] ..]}: solid id scaled to
   circumradius 1, the exact Emmy vertices evaluated by raster."
  [id]
  (let [s (solids/solids id)
        R (my/radius id :circum)]
    {:vertices (mapv (fn [v] (vec (raster/value (fn [] (mapv #(e// % R) v))))) (:vertices s))
     :edges (:edges s)}))

;; ---------------------------------------------------------------------------
;; The page

(def colours
  {:cube "#B03A2E" :tetrahedron "#D68910" :dodecahedron "#1E8449" :icosahedron "#2471A3" :octahedron "#7D3C98"
   :sphere "#8A9BB0" :copernicus "#E67E22" :line "#3C4650"})

(def greek
  {:cube "κύβος" :tetrahedron "πυραμίς" :dodecahedron "δωδεκάεδρον" :icosahedron "εἰκοσάεδρον" :octahedron "ὀκτάεδρον"})

(def ratio-tex
  "The closed form of r/R (graded proved in alexandria.kepler.mysterium)."
  {:cube "\\tfrac{1}{\\sqrt3}" :tetrahedron "\\tfrac13" :dodecahedron "\\sqrt{\\tfrac{5+2\\sqrt5}{15}}"
   :icosahedron "\\sqrt{\\tfrac{5+2\\sqrt5}{15}}" :octahedron "\\tfrac{1}{\\sqrt3}"})

(def scene-range (vec (repeat 3 [-1.05 1.05])))

(defn- template
  "Fill a quoted browser form: every ?name symbol is replaced by subs."
  [subs form]
  (walk/postwalk (fn [x] (if (and (symbol? x) (contains? subs x)) (subs x) x)) form))

(defn sphere
  "The sphere of radius r, (u, v) in [0 2pi] x [0 pi]."
  [r]
  (fn [[u v]] [(e/* r (e/cos u) (e/sin v)) (e/* r (e/sin u) (e/sin v)) (e/* r (e/cos v))]))

(defn ring
  "The circle of radius c * rs in the equatorial plane, t in [0 2pi]."
  [c]
  (fn [rs] (fn [t] [(e/* c rs (e/cos t)) (e/* c rs (e/sin t)) 0])))

(defn- radius-sym [i] (symbol (str "r" i)))

(defn- edges-form
  "The edges of shell j's solid, inscribed in sphere j (radius r_j, a
   readout number): one MathBox line per edge, bold when the shell is
   picked."
  [j id]
  (let [{:keys [vertices edges]} (unit-solid id)
        r (radius-sym j)
        p (fn [[x y z]] [(list '* r x) (list '* r y) (list '* r z)])]
    (into [:<>]
          (for [[a b] edges]
            (plot/line {:coords [(p (vertices a)) (p (vertices b))]
                        :color (colours id)
                        :width (list 'if (list '= 'shell (inc j)) 5 2)
                        :opacity (list 'if (list '= 'shell (inc j)) 1 0.55)})))))

(defn- scene [!s]
  (plot/scene
   {:range scene-range
    :scale [1 1 1]
    :ref '(fn [b] (reset! box b))
    :camera [1.1 -1.9 0.9]
    :axes [] :grids []
    :container {:style {:height "580px" :width "100%"}}}
   (into [:<>]
         (for [k tied]
           (plot/parametric-surface {:f (ev/with-params {:atom !s :params [k]} sphere)
                                     :u [0 (* 2 Math/PI)] :v [0 Math/PI] :u-samples 40 :v-samples 24
                                     :color (colours :sphere) :opacity 0.1 :shaded? false :simplify? false
                                     :grid-u 12 :grid-v 6 :grid-opacity 0.12})))
   (into [:<>]
         (for [[i c] (map-indexed vector (map chain-value (range 6)))]
           (plot/parametric-curve {:f (ev/with-params {:atom !s :params [:saturn]} (ring c))
                                   :t [0 (* 2 Math/PI)] :samples 97 :color (colours :line) :width 1.5
                                   :opacity 0.7 :simplify? false})))
   (into [:<>]
         (for [c (rest my/copernicus-chain)]
           (plot/parametric-curve {:f (ev/with-params {:atom !s :params [:saturn]} (ring (double c)))
                                   :t [0 (* 2 Math/PI)] :samples 97 :color (colours :copernicus) :width 2.5
                                   :simplify? false})))
   (plot/parametric-curve {:f (ev/with-params {:atom !s :params [:saturn]}
                                              (ring (raster/value (fn [] (my/chain 5 :mid)))))
                           :t [0 (* 2 Math/PI)] :samples 97 :color (colours :octahedron) :width 1.5
                           :opacity 0.8 :simplify? false})
   (into [:<>] (map-indexed (fn [j [_ id _]] (edges-form j id)) my/shells))))

(def caption
  (str "Six spheres, one per planet, and between each two a regular solid inscribed in the outer sphere and "
       "circumscribed about the inner one. Every radius is a slider and every radius is tied: move one and the "
       "other five follow, because each ratio r/R is fixed by its solid. Grey rings are the spheres' equators; "
       "orange rings are Copernicus' radii from De revolutionibus, as Kepler set them beside his own; the purple "
       "ring is Mercury on the octahedron's midsphere, Kepler's repair. Drag to rotate, scroll to zoom."))

(def quotes
  [["Kepler, Mysterium Cosmographicum (1596), preface"
    "Terra est Circulus mensor omnium. Illi circumscribe Dodecaedron: Circulus hoc comprehendens erit Mars."
    "The Earth is the circle that measures all. Circumscribe a dodecahedron about it: the circle containing that will be Mars."]
   ["Copernicus, De revolutionibus (1543), I.10"
    "In medio vero omnium residet Sol."
    "In the middle of all sits the Sun."]])

(defn- body
  [{:keys [tie-b ro-b !p !s !x]}]
  (let [[tsym tform] tie-b [rsym rform] ro-b
        opts {:tie-sym tsym :p !p :s !s :keys tied :ranges ranges :counter "__mysteriumSolves"}
        ix readout-index
        shells (mapv (fn [[outer id inner]] [(name outer) (name id) (name inner) (solids/names id) (greek id) (ratio-tex id) (colours id)])
                     my/shells)]
    (template
     {'?ro rsym '?s !s '?x !x '?shells shells '?ix ix
      '?leva (leva/sub-panel {:fill true :flat true :title-bar false}
                             (leva/controls {:atom !p :schema (tie/schema tied ranges labels)})
                             (leva/controls {:atom !x :schema {:shell {:min 1 :max 5 :step 1 :label "shell"}}}))
      '?scene (scene !s)
      '?caption caption
      '?quotes quotes
      '?range scene-range
      '?watch-off (tie/unwatch-form !p)}
     (list 'reagent.core/with-let
           (into [tsym tform rsym rform]
                 (concat (tie/locals opts)
                         ['box '(atom nil)
                          'ready '(reagent.core/atom false)
                          '_poll (list 'let ['poll (list 'fn 'poll []
                                                         (list 'if (list 'and (list '.ready tsym) (list '.ready rsym))
                                                               '(reset! ready true)
                                                               '(js/setTimeout poll 20)))]
                                       '(poll))]))
           '(let [st @?s
                  rs (get st :saturn)
                  v (when @ready (?ro (array 0) (array rs)))
                  at (fn [k i] (if v (aget v (+ (get ?ix k) i)) js/NaN))
                  r0 (at :kepler 0) r1 (at :kepler 1) r2 (at :kepler 2) r3 (at :kepler 3) r4 (at :kepler 4) r5 (at :kepler 5)
                  shell (get @?x :shell)
                  f4 (fn [x] (if (js/isFinite x) (.replace (.toFixed x 4) "-" "−") "…"))
                  [outer id inner nm gk rtex col] (nth ?shells (dec shell))
                  j (dec shell)
                  tex (str "r_{\\text{" inner "}} = " rtex "\\, r_{\\text{" outer "}} = "
                           (f4 (at :ratio j)) " \\times " (f4 (at :kepler j)) " = " (f4 (at :kepler (inc j))))
                  ctex (str "\\text{Copernicus: } " (f4 (at :copernicus (inc j))) " = "
                            (f4 (at :copernicus-ratio j)) " \\times " (f4 (at :copernicus j)))
                  planets ["Saturn" "Jupiter" "Mars" "Earth" "Venus" "Mercury"]
                  labels (when v (vec (for [i (range 6) :let [r (at :kepler i)]]
                                        {:text (nth planets i) :at [(* 0.7071 r) (* -0.7071 r) 0]
                                         :colour "#3C4650" :halo "rgba(255,255,255,0.95)" :size 13 :offset [4 6]})))]
              [:div {:data-figure "kepler-mysterium"
                     :data-radii (js/JSON.stringify (clj->js (mapv (fn [i] (at :kepler i)) (range 6))))
                     :data-copernicus (js/JSON.stringify (clj->js (mapv (fn [i] (at :copernicus i)) (range 6))))
                     :data-state (js/JSON.stringify (clj->js (mapv (fn [k] (get st k)) [:saturn :jupiter :mars :earth :venus :mercury])))}
               [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
                [:div {:style {:flex "1 1 460px" :min-width "320px" :position "relative"}}
                 [:f> alexandria.medium.html-labels/overlay {:box box :range ?range :labels (or labels [])}]
                 ?scene]
                [:div {:style {:flex "0 1 330px" :min-width "280px"}}
                 ?leva
                 [:div {:style {:font-size "1.2em" :font-weight 600 :margin "0.3em 0" :color col}}
                  [:span {:data-solid-name id} nm] " (" gk "): " outer " | " inner]
                 [:div {:data-formula tex} [:f> nextjournal.clerk.render/render-katex tex {:inline? false}]]
                 [:div [:f> nextjournal.clerk.render/render-katex ctex {:inline? false}]]
                 [:div {:data-miss (str (at :miss j))
                        :style {:font-weight 600 :color (if (< (js/Math.abs (at :miss j)) 0.01) "#1E8449" "#B03A2E")}}
                  "Kepler against Copernicus: " (.toFixed (* 100 (at :miss j)) 1) "%"]
                 [:table {:style {:font-family "ui-monospace, monospace" :font-size "0.82em" :margin-top "0.5em" :border-collapse "collapse"}}
                  [:thead [:tr [:th {:style {:text-align "left"}} "sphere"] [:th "Kepler"] [:th "Copernicus"]]]
                  (into [:tbody]
                        (for [i (range 6)]
                          [:tr {:key i :style {:font-weight (if (or (= i j) (= i (inc j))) 700 400)}}
                           [:td (nth planets i)] [:td {:style {:padding "0 0.6em"}} (f4 (at :kepler i))]
                           [:td {:style {:color "#E67E22" :padding "0 0.6em"}} (f4 (at :copernicus i))]]))]
                 [:div {:style {:font-size "0.82em" :opacity 0.8 :margin-top "0.3em"}}
                  "Mercury on the octahedron's midsphere: " (f4 (at :mercury-mid 0))]]]
               [:p {:style {:font-size "0.9em" :opacity 0.85}} ?caption]
               (into [:div]
                     (for [[src latin english] ?quotes]
                       [:blockquote {:key src :style {:margin "0.4em 0" :font-style "italic" :opacity 0.85}}
                        latin [:br] english [:br] [:span {:style {:font-style "normal" :font-size "0.85em"}} "— " src]]))])
           '(finally ?watch-off)))))

(defn widget
  "Kepler's nest of solids, the six radii tied. An emmy-viewers fragment
   for a notebook after (history-of-math.widgets/install!); compiled on
   the raster backend whatever the caller's binding."
  ([] (widget (init)))
  ([start-state]
   (binding [vc/*backend* :raster]
     (let [s (mapv start-state tied)]
       (ev/with-let [!p start-state !s start-state !x {:shell 3}]
         (body {:tie-b [(gensym "tie") (vc/compiled-fn tie s [0] {:simplify? false})]
                :ro-b (tie/kernel-binding "readout" readout [(first s)])
                :!p !p :!s !s :!x !x}))))))

;; ---------------------------------------------------------------------------
;; The derivation, for the page

(defn derivation
  "Emmy's chain of radii and the tie's grades, as a Clerk column."
  []
  (apply clerk/col
         (concat
          (for [{:keys [label eqs]} (my/chain-equations)]
            (apply clerk/col
                   (clerk/html [:div {:style {:font-size "0.85em" :opacity 0.7 :margin-top "0.5em"}} label])
                   (for [q eqs] (ec/->TeX q :simplify? true))))
          [(clerk/html
            (into [:ul {:style {:font-size "0.9em"}}]
                  (for [{:keys [label grade]} (my/chain-grades)]
                    [:li {:data-grade (name grade)} [:code (name grade)] " " label])))])))
