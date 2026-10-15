(ns history-of-math.timeline-view
  "Interactive timeline: raster-computed positions supplied by the JVM,
   Reagent only chooses which fields and influence edges to highlight."
  (:require [reagent.core :as r]
            [clojure.string :as str]))

(defn- two-lines
  "A title split at the space nearest its middle, so a label outside the
   ring stays narrow."
  [title]
  (let [mid (/ (count title) 2)
        spaces (keep-indexed (fn [i c] (when (= c " ") i)) title)]
    (if (and (< 12 (count title)) (seq spaces))
      (let [i (apply min-key #(js/Math.abs (- % mid)) spaces)]
        [(subs title 0 i) (subs title (inc i))])
      [title])))

(defn- strand-map
  "The strands on a ring with their bridges as chords: a lens :link between
   works on different strands. Clicking a strand selects its timeline."
  [{:keys [strands bridges ring]} selected]
  (let [{:keys [width height cx cy r]} ring
        by-id (into {} (map (juxt :id identity)) strands)
        lit? (fn [{:keys [from to]}] (or (nil? @selected) (= @selected from) (= @selected to)))]
    [:svg {:class "strand-map" :view-box (str "0 0 " width " " height) :role "img"
           :aria-label "The strands of mathematics and the bridges between them"
           :style {:display "block" :width "100%" :max-width (str width "px") :height "auto"
                   :background "#101e30" :font-family "ui-serif, Georgia, serif"}}
     [:circle {:cx cx :cy cy :r r :fill "none" :stroke "#2a3b52"}]
     (for [{:keys [from to x y target-x target-y count notes] :as b} bridges
           :let [mx (/ (+ x target-x) 2) my (/ (+ y target-y) 2)
                 ;; the chord bows toward the centre, more for a long one
                 qx (+ mx (* 0.45 (- cx mx))) qy (+ my (* 0.45 (- cy my)))]]
       ^{:key (str (name from) "~" (name to))}
       [:path {:class "strand-bridge" :data-from (name from) :data-to (name to)
               :d (str "M" x "," y " Q" qx "," qy " " target-x "," target-y)
               :fill "none" :stroke "#e7a85c" :stroke-width (min 6 (+ 1.5 count))
               :stroke-linecap "round"
               :stroke-opacity (if (lit? b) 0.8 0.08)}
        [:title (str (:title (by-id from)) " ↔ " (:title (by-id to)) ": " (str/join "; " notes))]])
     (for [{:keys [id title colour count x y scope]} strands
           :let [on? (or (nil? @selected) (= @selected id))
                 dx (- x cx) dy (- y cy) d (max 1 (js/Math.hypot dx dy))
                 lx (+ x (* 14 (/ dx d))) ly (+ y (* 14 (/ dy d)))
                 anchor (cond (< (js/Math.abs dx) 20) "middle" (pos? dx) "start" :else "end")
                 lines (two-lines title)
                 n-lines (clojure.core/count lines)
                 ly0 (cond (< dy -20) (- ly (* 15 (dec n-lines)))
                           (> dy 20) (+ ly 10)
                           :else (+ ly 4 (* -7 (dec n-lines))))]]
       ^{:key (name id)}
       [:g {:class "strand-node" :data-strand (name id) :style {:cursor "pointer"}
            :opacity (if on? 1 0.4)
            :on-click #(reset! selected (if (= @selected id) nil id))}
        [:title scope]
        [:circle {:cx x :cy y :r (+ 6 (min 8 (/ count 4))) :fill colour :stroke "#101e30" :stroke-width 2}]
        [:text {:x lx :y ly0 :fill "#e9f1f5" :font-size 14 :text-anchor anchor}
         (for [[i line] (map-indexed vector lines)]
           ^{:key i} [:tspan {:x lx :dy (if (zero? i) 0 15)} line])]])]))

(defn render [{:keys [nodes edges strands width height heights] :as data}]
  (r/with-let [selected (r/atom nil)]
    (let [sel @selected
          strand-of (into {} (map (juxt :id identity)) strands)
          shown (if sel (filterv #(some #{sel} (:fields %)) nodes) nodes)
          row-y (fn [n] (if sel (get-in n [:rows sel]) (:y n)))
          by-id (into {} (keep (fn [n] (when (and (:id n) (:built? n)) [(:id n) n]))) shown)
          h (if sel (get heights sel height) height)]
      [:section {:class "history-timeline" :data-strand (if sel (name sel) "all")}
       [:div {:class "timeline-fields" :style {:display "flex" :flex-wrap "wrap" :gap "0.5em" :margin "0.7em 0"}}
        [:button {:data-field "all" :aria-pressed (nil? sel) :on-click #(reset! selected nil)} "All strands"]
        (for [{:keys [id title colour count]} strands]
          ^{:key (name id)}
          [:button {:data-field (name id) :aria-pressed (= sel id)
                    :style {:border-bottom (str "3px solid " colour)}
                    :on-click #(reset! selected (if (= sel id) nil id))}
           (str title " (" count ")")])]
       (when sel
         [:p {:class "strand-scope" :style {:opacity 0.85}} [:strong (:title (strand-of sel))] " — " (:scope (strand-of sel))])
       [strand-map data selected]
       [:svg {:class "timeline-svg" :view-box (str "0 0 " width " " h) :role "img"
              :aria-label "History of mathematics timeline and influence arcs"
              :style {:max-width "100%" :height "auto" :background "#101e30"
                      :font-family "ui-serif, Georgia, serif"}}
        [:line {:x1 40 :x2 (- width 40) :y1 40 :y2 40 :stroke "#b3c8d9"}]
        [:text {:x 290 :y 25 :fill "#b3c8d9"} "⋯"]
        (for [{:keys [from to note]} edges
              :let [a (by-id from) b (by-id to)] :when (and a b)
              :let [x (:x a) y (row-y a) tx (:x b) ty (row-y b)]]
          ^{:key (str from "→" to "-" y "-" ty)}
          [:path {:class "influence-arc" :data-from from :data-to to
                  :d (str "M" x "," y " Q" (/ (+ x tx) 2) "," (- (min y ty) 48) " " tx "," ty)
                  :fill "none" :stroke "#e7a85c" :stroke-width 1.7 :stroke-opacity 0.72}
           [:title note]])
        (for [[i {:keys [id x year figure work built? fields colour] :as n}] (map-indexed vector shown)
              :let [y (row-y n)
                    right? (> x (* 0.62 width))
                    others (remove #{sel} fields)]]
          ^{:key (str i "-" figure "-" year)}
          [:g {:class "timeline-entry" :data-notebook id :data-fields (str/join " " (map name fields))
               :data-active "true"}
           [:line {:x1 x :x2 x :y1 40 :y2 y :stroke colour :stroke-opacity 0.32}]
           [:circle {:cx x :cy 40 :r 3 :fill colour}]
           [:circle {:cx x :cy y :r 5 :fill (if built? colour "none") :stroke colour}]
           (let [label [:text {:x (if right? (- x 10) (+ x 10)) :y (+ y 4) :font-size 13
                               :text-anchor (if right? "end" "start")
                               :fill (if built? "#e9f1f5" "#afbdc9") :font-style (if built? "normal" "italic")}
                        (str year "  " figure ", " work (when-not built? " (planned)"))
                        (for [f others :let [{:keys [title colour]} (strand-of f)]]
                          ^{:key (name f)}
                          [:tspan {:class "strand-chip" :data-strand (name f) :fill colour :font-size 10 :font-style "normal"}
                           (str "  ◆ " title)])]]
             (if built?
               [:a {:href (str id "/") :data-ignore-anchor-click "true"} label]
               label))])]])))
