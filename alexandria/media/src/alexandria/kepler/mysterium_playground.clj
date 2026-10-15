(ns alexandria.kepler.mysterium-playground
  "A free-form playground on Kepler's own setup (Mysterium Cosmographicum,
   1596; playground brief): six spheres, five regular solids, each solid
   inscribed in the sphere outside it and circumscribed about the sphere
   inside it. The reader is free to

     - REORDER the solids (a select per gap; picking a solid swaps it with
       the gap that held it, so the five always stay a permutation);
     - SCALE the nest: drag any planet's point on the equatorial section
       (Saturn's radius is solved back from it, r_Saturn = r_i / k_i) or set
       r_Saturn on the slider;
     - move Mercury from the octahedron's insphere to its midsphere
       (Kepler's own repair, cap. XIV), a break of his rule shown as such.

   Every number on screen comes from raster kernels: the readout kernel
   (nest-readout) gives the six radii, the chain factors k_i, Copernicus'
   radii, the five misses (q - c)/c against Copernicus' ratios and their
   root mean square; the inverse kernel (saturn-of) solves r_Saturn back
   from a dragged planet. The ratios r/R of the solids are the exact Emmy
   ratios of alexandria.kepler.mysterium evaluated by raster on the JVM;
   the verdict ranks the reader's order among all 120 orders, each scored
   by the same readout kernel in the browser.

   3D: the spheres and the solids in MathBox on *backend* :raster,
   orbitable, beside a 2D equatorial section in Mafs whose points drag."
  (:require [alexandria.kepler.mysterium :as my]
            [alexandria.kepler.mysterium-widget :as nest]
            [alexandria.medium.tie :as tie]
            [alexandria.raster :as raster]
            [alexandria.solids :as solids]
            [clojure.walk :as walk]
            [emmy.env :as e]
            [emmy.leva :as leva]
            [emmy.mafs :as mafs]
            [emmy.mathbox.plot :as plot]
            [emmy.viewer :as ev]
            [emmy.viewer.compile :as vc]
            [emmy.viewer.raster]))

;; ---------------------------------------------------------------------------
;; The kernels

(def copernicus-ratios
  "Copernicus' inner/outer ratios per gap, outside in (Kepler's table,
   Dreyer 1906 p. 375), exact rationals."
  (mapv #(/ (:copernicus %) 1000) my/kepler-table))

(def readout-index
  "Where each block sits in the readout kernel's output."
  {:radius 0 :chain 6 :copernicus 12 :miss 18 :rms 23})

(defn nest-readout
  "The nest for Saturn's radius rs and the ratios q0..q4 of the solids in
   the five gaps, outside in: [r0..r5 k0..k5 c0..c5 m0..m4 rms] with
   r_i = rs k_i, k_i = q0 ... q_(i-1), Copernicus' radii c_i, the misses
   m_j = (q_j - C_j)/C_j against Copernicus' ratio C_j, and the root mean
   square of the misses."
  [rs q0 q1 q2 q3 q4]
  (fn [_]
    (let [qs [q0 q1 q2 q3 q4]
          ks (vec (reductions e/* 1 qs))
          cs (vec (reductions e/* 1 copernicus-ratios))
          ms (mapv (fn [q c] (e// (e/- q c) c)) qs copernicus-ratios)]
      (vec (concat (map #(e/* rs %) ks) ks (map #(e/* rs %) cs) ms
                   [(e/sqrt (e// (reduce e/+ (map e/square ms)) 5))])))))

(defn saturn-of
  "Inverse: Saturn's radius from planet i's radius r and its chain factor k."
  [r k]
  (fn [_] [(e// r k)]))

;; ---------------------------------------------------------------------------
;; Data from the shelf, by raster

(def kepler-order "Kepler's order, outside in." (mapv second my/shells))

(defn ratios
  "{solid {:in r/R :mid r_mid/R}}: the exact ratios by raster."
  []
  (into {} (for [id solids/solid-ids] [id {:in (my/ratio-value id :in) :mid (my/ratio-value id :mid)}])))

(defn- perms [xs]
  (if (empty? xs) [[]] (vec (for [x xs p (perms (vec (remove #{x} xs)))] (into [x] p)))))

(def orders "All 120 orders of the five solids." (perms kepler-order))

(defn order-ratios
  "q0..q4 for an order; the innermost gap on the midsphere when mid."
  [rt order mid]
  (vec (map-indexed (fn [j id] (get-in rt [id (if (and mid (= j 4)) :mid :in)])) order)))

(defn readout
  "The readout kernel by raster on the JVM (for tests and the first paint)."
  [rs order mid]
  (vec (binding [raster/*simplify?* false]
         (apply raster/value (fn [& xs] ((apply nest-readout xs) 0)) rs (order-ratios (ratios) order mid)))))

(defn rank
  "[rank rms best-rms] of an order among all 120 (1 = best; ties share)."
  [order mid]
  (let [rt (ratios)
        score (fn [o] (nth (vec (binding [raster/*simplify?* false]
                                  (apply raster/value (fn [& xs] ((apply nest-readout xs) 0)) 1 (order-ratios rt o mid))))
                           (:rms readout-index)))
        all (map score orders)
        s (score order)]
    [(inc (count (filter #(< % (- s 1e-12)) all))) s (apply min all)]))

;; ---------------------------------------------------------------------------
;; The page

(def start-state {:order kepler-order :mid false})
(def start-controls {:rs 0.9 :shell 2})

(def presets
  [["Kepler's order" {:order kepler-order :mid false} {:rs 0.9 :shell 2}]
   ["Swap cube and tetrahedron" {:order [:tetrahedron :cube :dodecahedron :icosahedron :octahedron] :mid false} {:rs 0.9 :shell 1}]
   ["Swap the dual pair dodecahedron | icosahedron" {:order [:cube :tetrahedron :icosahedron :dodecahedron :octahedron] :mid false} {:rs 0.9 :shell 3}]
   ["Mercury on the midsphere (Kepler's repair)" {:order kepler-order :mid true} {:rs 0.9 :shell 5}]])

(def planet-names ["Saturn" "Jupiter" "Mars" "Earth" "Venus" "Mercury"])

(def scene-range (vec (repeat 3 [-1.05 1.05])))

(defn- template [subs form]
  (walk/postwalk (fn [x] (if (and (symbol? x) (contains? subs x)) (subs x) x)) form))

(defn- unit-solids []
  (into {} (for [id solids/solid-ids] [id (nest/unit-solid id)])))

(defn- scene [!r !c]
  (plot/scene
   {:range scene-range :scale [1 1 1]
    :ref '(fn [b] (reset! box b))
    :camera [1.1 -1.9 0.9] :axes [] :grids []
    :container {:style {:height "520px" :width "100%"}}}
   (into [:<>]
         (for [i (range 6)]
           (plot/parametric-surface {:f (ev/with-params {:atom !r :params [(keyword (str "r" i))]} nest/sphere)
                                     :u [0 (* 2 Math/PI)] :v [0 Math/PI] :u-samples 40 :v-samples 24
                                     :color (nest/colours :sphere) :opacity 0.1 :shaded? false :simplify? false
                                     :grid-u 12 :grid-v 6 :grid-opacity 0.12})))
   (into [:<>]
         (for [c (rest my/copernicus-chain)]
           (plot/parametric-curve {:f (ev/with-params {:atom !c :params [:rs]} (nest/ring (double c)))
                                   :t [0 (* 2 Math/PI)] :samples 97 :color (nest/colours :copernicus) :width 2.5
                                   :simplify? false})))
   'edges))

(def caption
  (str "Kepler's setup, open: pick the solid in each gap (the one it replaces moves to the gap it left), "
       "drag a planet's point on the equatorial section or the r Saturn slider to scale the whole nest, "
       "and put Mercury on the innermost solid's midsphere as Kepler did. The 3D view orbits (drag) and zooms (scroll): "
       "grey spheres are Kepler's, orange rings Copernicus' radii. Every number is computed by raster."))

(defn playground
  "The Mysterium playground: an emmy-viewers fragment on the raster
   backend."
  []
  (binding [vc/*backend* :raster]
    (let [rt (ratios)
          r0 (readout (:rs start-controls) kepler-order false)
          start-radii (zipmap (map #(keyword (str "r" %)) (range 6)) (take 6 r0))
          [ro roform] (tie/kernel-binding "nest" nest-readout (into [0.9] (order-ratios rt kepler-order false)))
          [inv invform] (tie/kernel-binding "saturnof" saturn-of [0.5 0.5])
          shells (mapv (fn [id] [id (solids/names id) (nest/greek id) (nest/colours id)]) solids/solid-ids)]
      (ev/with-let [!s start-state !c start-controls !r start-radii]
        (template
         {'?s !s '?c !c '?r !r '?ro ro '?inv inv '?ix readout-index '?rt rt '?orders orders
          '?kepler kepler-order '?units (unit-solids) '?shells (into {} (map (fn [[id & more]] [id (vec more)]) shells))
          '?ids solids/solid-ids '?planets planet-names '?presets presets '?start-s start-state '?start-c start-controls
          '?range scene-range '?caption caption
          '?leva (leva/sub-panel {:fill true :flat true :title-bar false}
                                 (leva/controls {:atom !c :schema {:rs {:min 0.05 :max 1 :step 0.001 :label "r Saturn"}
                                                                   :shell {:min 1 :max 5 :step 1 :label "shell"}}}))
          '?scene (scene !r !c)}
         (list 'reagent.core/with-let
               [ro roform inv invform
                'box '(atom nil)
                'ready '(reagent.core/atom false)
                'qs '(fn [order mid] (apply array (map-indexed (fn [j id] (get-in ?rt [id (if (and mid (= j 4)) :mid :in)])) order)))
                'run '(fn [rs order mid] (?ro (array 0) (.concat (array rs) (qs order mid))))
                'scores '(atom {})
                'score-of '(fn [mid] (or (get @scores mid)
                                         (let [m (into {} (map (fn [o] [o (aget (run 1 o mid) (get ?ix :rms))]) ?orders))]
                                           (swap! scores assoc mid m) m)))
                'sync! '(fn [] (when @ready
                                 (let [v (run (get @?c :rs) (get @?s :order) (get @?s :mid))]
                                   (reset! ?r (into {} (map (fn [i] [(keyword (str "r" i)) (aget v i)]) (range 6)))))))
                '_w '(do (add-watch ?s ::sync (fn [_ _ _ _] (sync!))) (add-watch ?c ::sync (fn [_ _ _ _] (sync!))))
                '_poll (list 'let ['poll (list 'fn 'poll []
                                               (list 'if (list 'and (list '.ready ro) (list '.ready inv))
                                                     '(do (reset! ready true) (sync!))
                                                     '(js/setTimeout poll 20)))]
                             '(poll))]
               '(let [{:keys [order mid]} @?s
                      {:keys [rs shell]} @?c
                      v (when @ready (run rs order mid))
                      at (fn [k i] (if v (aget v (+ (get ?ix k) i)) js/NaN))
                      f4 (fn [x] (if (js/isFinite x) (.replace (.toFixed x 4) "-" "−") "…"))
                      pct (fn [x] (if (js/isFinite x) (str (if (neg? x) "−" "+") (.toFixed (js/Math.abs (* 100 x)) 1) "%") "…"))
                      radii (mapv #(at :radius %) (range 6))
                      sc (when v (score-of mid))
                      rms (at :rms 0)
                      kepler-rms (when sc (get sc ?kepler))
                      best (when sc (apply min (vals sc)))
                      rank (when sc (inc (count (filter #(< % (- rms 1e-12)) (vals sc)))))
                      kepler? (= order ?kepler)
                      verdict (cond (nil? sc) "…"
                                    (and kepler? (= rank 1)) (str "Kepler's order fits best: no other of the 120 orders misses Copernicus by less (rms " (.toFixed (* 100 rms) 1) "%)")
                                    (= rank 1) (str "this order ties Kepler's (rms " (.toFixed (* 100 rms) 1) "%): the solids you swapped are a dual pair, with the same r/R")
                                    :else (str "this order ranks " rank " of 120 (rms " (.toFixed (* 100 rms) 1) "%): Kepler's order fits better (rms " (.toFixed (* 100 kepler-rms) 1) "%)"))
                      nested (and v (every? (fn [j] (< (js/Math.abs (- (/ (nth radii (inc j)) (nth radii j)) (nth (qs order mid) j))) 1e-9)) (range 5)))
                      j (dec shell)
                      id (nth order j)
                      [nm gk col] (get ?shells id)
                      outer (nth ?planets j) inner (nth ?planets (inc j))
                      q (nth (qs order mid) j)
                      fit (fn [m] (let [a (js/Math.abs m)] (cond (< a 0.01) "fits Copernicus within 1%" (< a 0.05) "close: within 5%" :else "misses")))
                      tex (str "r_{\\text{" inner "}} = \\tfrac{r}{R}\\, r_{\\text{" outer "}} = " (f4 q) " \\times " (f4 (at :radius j)) " = " (f4 (at :radius (inc j))))
                      ctex (str "\\text{Copernicus: } \\tfrac{r_{\\text{" inner "}}}{r_{\\text{" outer "}}} = " (f4 (/ (at :copernicus (inc j)) (at :copernicus j)))
                                ",\\quad \\text{miss} = " (.replace (pct (at :miss j)) "%" "\\%"))
                      edges (into [:<>]
                                  (for [g (range 5) :let [sid (nth order g) {:keys [vertices edges]} (get ?units sid)
                                                          r (nth radii g) p (fn [[x y z]] [(* r x) (* r y) (* r z)])]
                                        :when (js/isFinite r)
                                        [a b] edges]
                                    (with-meta
                                      [emmy.mathbox.components.plot/Line {:coords [(p (nth vertices a)) (p (nth vertices b))]
                                                                          :color (nth (get ?shells sid) 2)
                                                                          :width (if (= g j) 5 2) :opacity (if (= g j) 1 0.55)}]
                                      {:key (str g sid a "-" b)})))
                      labels (when v (vec (for [i (range 6) :let [r (nth radii i)]]
                                            {:text (nth ?planets i) :at [(* 0.7071 r) (* -0.7071 r) 0]
                                             :colour "#3C4650" :halo "rgba(255,255,255,0.95)" :size 13 :offset [4 6]})))
                      choose (fn [g sid] (swap! ?s update :order
                                                (fn [o] (let [k (first (keep-indexed (fn [i x] (when (= x sid) i)) o))] (if k (assoc o k (nth o g) g sid) o)))))]
                  [:div {:data-playground "kepler-mysterium-nest"
                         :data-order (js/JSON.stringify (clj->js (map name order)))
                         :data-radii (js/JSON.stringify (clj->js radii))
                         :data-misses (js/JSON.stringify (clj->js (mapv #(at :miss %) (range 5))))
                         :data-rms (str rms) :data-rank (str rank) :data-mid (str mid)}
                   [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
                    [:div {:style {:flex "1 1 440px" :min-width "320px" :position "relative"}}
                     [:f> alexandria.medium.html-labels/overlay {:box box :range ?range :labels (or labels [])}]
                     ?scene]
                    [:div {:style {:flex "0 1 360px" :min-width "300px"}}
                     ?leva
                     [:div {:data-section true}
                      [mafs.core/Mafs {:view-box {:x [-1.05 1.05] :y [-1.05 1.05]} :height 300}
                       [mafs.coordinates/Cartesian {}]
                       (into [:<>] (for [i (range 6) :let [r (nth radii i)] :when (js/isFinite r)]
                                     (with-meta [mafs.core/Circle {:center [0 0] :radius r :color (if (or (= i j) (= i (inc j))) col "#8A9BB0")
                                                                   :weight (if (or (= i j) (= i (inc j))) 2.5 1) :fill-opacity 0}]
                                       {:key (str "k" i)})))
                       (into [:<>] (for [i (range 1 6) :let [c (at :copernicus i)] :when (js/isFinite c)]
                                     (with-meta [mafs.core/Circle {:center [0 0] :radius c :color "#E67E22" :stroke-style "dashed" :fill-opacity 0}]
                                       {:key (str "c" i)})))
                       (into [:<>] (for [i (range 6) :let [r (nth radii i)] :when (js/isFinite r)]
                                     (with-meta [mafs.core/Text {:x (* -0.7071 r) :y (* 0.7071 r) :size 11} (nth ?planets i)]
                                       {:key (str "t" i)})))
                       (let [shown (reduce (fn [acc i] (let [r (nth radii i)] (if (and (js/isFinite r) (or (empty? acc) (>= (- (at :chain (peek acc)) (at :chain i)) 0.24))) (conj acc i) acc))) [] (range 6))] [:<> (into [:<>] (for [i (range 6) :let [r (nth radii i)] :when (and (js/isFinite r) (not (some #{i} shown)))] (with-meta [mafs.core/Point {:x (* 0.7071 r), :y (* -0.7071 r), :color "#3C4650"}] {:key (str "d" i)}))) (into [:<>] (for [i shown :let [r (nth radii i) k (at :chain i)]] (with-meta
                                       [mafs.core/MovablePoint {:point [(* 0.7071 r) (* -0.7071 r)] :color "#3C4650"
                                                                :constrain (fn [[x y]] (let [d (max 0.002 (/ (- x y) 1.41421356))] [(* d 0.7071) (* d -0.7071)]))
                                                                :on-move (fn [[x y]]
                                                                           (let [d (max 0.002 (/ (- x y) 1.41421356))
                                                                                 s (aget (?inv (array 0) (array d k)) 0)]
                                                                             (swap! ?c assoc :rs (max 0.05 (min 1 s)))))}]
                                       {:key (str "p" i)})))])]]
                     [:div {:style {:font-size "0.8em" :opacity 0.75}} "Equatorial section: drag a planet's dot to scale the nest."]]]
                   [:div {:style {:font-size "1.15em" :font-weight 600 :margin "0.4em 0" :color col}}
                    "shell " shell ": " [:span {:data-solid-name (name id)} nm] " (" gk "), " outer " | " inner " — "
                    [:span {:data-fit true} (fit (at :miss j))]]
                   [:div [:f> nextjournal.clerk.render/render-katex tex {:inline? false}]]
                   [:div [:f> nextjournal.clerk.render/render-katex ctex {:inline? false}]]
                   [:table {:style {:font-size "0.86em" :border-collapse "collapse" :margin "0.4em 0"}}
                    [:thead [:tr [:th "gap"] [:th "solid"] [:th "r/R"] [:th "Copernicus"] [:th "miss"] [:th "case"]]]
                    (into [:tbody]
                          (for [g (range 5) :let [sid (nth order g) m (at :miss g)]]
                            [:tr {:key g :style {:font-weight (if (= g j) 700 400)}}
                             [:td {:style {:padding "0 0.5em"}} (nth ?planets g) " | " (nth ?planets (inc g))]
                             [:td [:select {:data-gap g :value (name sid)
                                            :on-change (fn [ev] (choose g (keyword (.. ev -target -value))))}
                                   (for [x ?ids] [:option {:key (name x) :value (name x)} (first (get ?shells x))])]
                              (when (and mid (= g 4)) " (midsphere)")]
                             [:td {:style {:padding "0 0.5em"}} (f4 (nth (qs order mid) g))]
                             [:td {:style {:padding "0 0.5em" :color "#E67E22"}} (f4 (/ (at :copernicus (inc g)) (at :copernicus g)))]
                             [:td {:data-miss g :style {:padding "0 0.5em" :color (if (< (js/Math.abs m) 0.01) "#1E8449" (if (< (js/Math.abs m) 0.05) "#B9770E" "#B03A2E"))}} (pct m)]
                             [:td {:style {:padding "0 0.5em"}} (fit m)]]))]
                   [:label [:input {:type "checkbox" :data-mid true :checked mid :on-change (fn [_] (swap! ?s update :mid not))}]
                    " Mercury on the innermost solid's midsphere (Kepler's repair)"]
                   [:div {:style {:margin "0.4em 0"}} [:strong {:data-verdict true} verdict]]
                   [:div
                    [:span {:data-invariant (if (= rank 1) "pass" "fail")
                            :style {:display "inline-block" :padding "0.15em 0.6em" :border-radius "0.6em" :color "white" :margin "0.2em"
                                    :background (if (= rank 1) "#2E8B57" "#C0392B")}}
                     (if (= rank 1) "✓ the best order of the five solids" "✗ not the best order: Kepler's fits Copernicus better")]
                    [:span {:data-invariant (if mid "fail" "pass")
                            :style {:display "inline-block" :padding "0.15em 0.6em" :border-radius "0.6em" :color "white" :margin "0.2em"
                                    :background (if mid "#C0392B" "#2E8B57")}}
                     (if mid "✗ Kepler's rule broken: Mercury rides the midsphere, not the insphere" "✓ every sphere is the insphere of the solid outside it")]
                    [:span {:data-invariant (if nested "pass" "fail")
                            :style {:display "inline-block" :padding "0.15em 0.6em" :border-radius "0.6em" :color "white" :margin "0.2em"
                                    :background (if nested "#2E8B57" "#C0392B")}}
                     (if nested "✓ each ratio r_inner / r_outer is its solid's r/R" "✗ the nest does not close")]]
                   [:p {:style {:font-size "0.9em" :opacity 0.85}} ?caption]
                   [:p "Try this:"]
                   (into [:div]
                         (for [[label s c] ?presets]
                           [:button {:key label :type "button" :data-preset label :style {:margin "0.2em"}
                                     :on-click (fn [] (reset! ?s s) (reset! ?c c))} label]))
                   [:button {:type "button" :data-preset "reset" :style {:margin "0.2em"}
                             :on-click (fn [] (reset! ?s ?start-s) (reset! ?c ?start-c))} "Reset to Kepler's nest"]])
               '(finally (remove-watch ?s ::sync) (remove-watch ?c ::sync))))))))
