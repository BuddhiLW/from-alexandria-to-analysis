(ns alexandria.medium.player
  "The proof player in the browser: the steps of a proof beside a figure
   that moves while each step is read.

     clock         the one effect: animation frames as elapsed milliseconds
     transport     previous / play-pause / next and a scrubber
     step-list     the steps; the current one marked, any one clickable
     stage-frame   the SVG the scene draws into
     proof-player  the above over one timeline (alexandria.medium.timeline)
     render        Clerk's entry: the value built by alexandria.medium.clerk"
  (:require [alexandria.medium.figure :as figure]
            [alexandria.medium.scene :as scene]
            [alexandria.medium.svg :as svg]
            [alexandria.medium.timeline :as tl]
            [alexandria.palette :as pal]
            [reagent.core :as r]))

;; ---------------------------------------------------------------------------
;; Boundary: time

(defn clock!
  "Calls (on-dt ms) once per animation frame with the milliseconds since the
   last frame. Returns a function that stops it."
  [on-dt]
  (let [alive (atom true)
        last-ts (atom nil)
        frame (fn frame [ts]
                (when @alive
                  (on-dt (if @last-ts (- ts @last-ts) 0))
                  (reset! last-ts ts)
                  (js/requestAnimationFrame frame)))]
    (js/requestAnimationFrame frame)
    #(reset! alive false)))

;; ---------------------------------------------------------------------------
;; Components: each takes the timeline value and `act`, (act f & args)
;; applying a timeline transition

(def ^:private button-style
  {:font-family "ui-monospace, monospace" :font-size "0.95em" :padding "0.15em 0.7em"
   :margin-right "0.35em" :border "1px solid currentColor" :border-radius "0.35em"
   :background "transparent" :color "inherit" :cursor "pointer" :opacity 0.8})

(defn transport [timeline act]
  [:div {:style {:display "flex" :align-items "center" :margin-top "0.5em"}}
   [:button {:style button-style :on-click #(act tl/goto (dec (:step timeline)))} "◀"]
   [:button {:style button-style :on-click #(act tl/toggle)} (if (:playing? timeline) "❚❚" "▶")]
   [:button {:style button-style :on-click #(act tl/goto (inc (:step timeline)))} "▶▶"]
   [:input {:type "range" :min 0 :max 1000 :style {:flex "1"}
            :value (js/Math.round (* 1000 (:progress timeline)))
            :on-change #(act tl/seek (/ (js/parseInt (.. % -target -value)) 1000))}]])

(defn step-list [steps timeline act colour]
  [:ol {:style {:flex "1 1 300px" :margin 0 :padding-left "1.6em" :font-size "0.92em" :line-height 1.45}}
   (for [[i {:keys [claim why]}] (map-indexed vector steps)
         :let [current? (= i (:step timeline))]]
     ^{:key i}
     [:li {:on-click #(act tl/goto i)
           :style {:cursor "pointer" :margin-bottom "0.45em" :padding "0.15em 0.4em"
                   :border-left (str "3px solid " (if current? colour "transparent"))
                   :opacity (if current? 1 0.55)}}
      claim " " [:em {:style {:opacity 0.7 :white-space "nowrap"}} "(" why ")"]])])

(defn stage-frame [window height background drawing]
  [:svg {:view-box (apply svg/view-box window)
         :style {:width "100%" :height (str height "px") :background background :border-radius "0.5em"}}
   drawing])

(defn controls-panel
  "A slider per control spec {:id :label :min :max :step}, over the map of
   current values; (set-value! id v) on input. The inputs are uncontrolled:
   the clock re-renders the player every animation frame, and a controlled
   :value re-set from the atom on each of those renders fought the drag (the
   thumb jumped back to a lagging value). The input owns its value; the atom
   follows it, and the scene reads the atom on the next frame."
  [specs values set-value!]
  (into [:div {:style {:display "flex" :flex-wrap "wrap" :gap "0.4em 1.2em" :margin-top "0.4em"
                       :font-family "ui-monospace, monospace" :font-size "0.85em"}}]
        (for [{:keys [id label min max step init]} specs
              :let [v (get values id)]]
          [:label {:style {:display "flex" :align-items "center" :gap "0.5em"}}
           [:span {:data-control (name id)} (str label " = " v)]
           [:input {:type "range" :min min :max max :step (or step 1) :default-value (or init v)
                    :data-control (name id)
                    :on-input #(set-value! id (js/parseFloat (.. % -target -value)))}]])))

;; ---------------------------------------------------------------------------
;; Composition

(defn proof-player
  "The player of `steps` ([{:claim :why :stage}]) for `scene-id` over ctx
   (alexandria.medium.scene). opts: :durations {stage ms}, :window
   [[x0 x1] [y0 y1]], :height px, :controls [{:id :label :min :max :step
   :init}] whose current values the scene reads in ctx :controls."
  [scene-id steps ctx {:keys [durations window height controls] :or {height 340}}]
  (r/with-let [timeline (r/atom (tl/timeline steps durations))
               values (r/atom (into {} (map (juxt :id :init)) controls))
               act (fn [f & args] (apply swap! timeline f args))
               stop! (clock! #(swap! timeline tl/advance %))]
    (let [t @timeline
          {:keys [background found]} (:palette ctx)
          stage (:stage (nth steps (:step t)))]
      [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
       [:div {:style {:flex "1 1 440px" :min-width "300px"}}
        [stage-frame window height background
         (scene/draw scene-id stage (:progress t) (assoc ctx :controls @values))]
        [transport t act]
        (when (seq controls)
          [controls-panel controls @values #(swap! values assoc %1 %2)])]
       [step-list steps t act found]])
    (finally (stop!))))

(defn render
  "Clerk render-fn. value: {:scene :steps :kernels {name kernel} :data
   :palette :durations :window :height :controls}."
  [{:keys [scene steps kernels data palette] :as value}]
  (r/with-let [figures (into {} (map (fn [[k v]] [k (figure/kernel-figure v)])) kernels)]
    [proof-player scene steps
     {:figures figures :data data :palette (pal/resolve-palette palette)}
     (select-keys value [:durations :window :height :controls])]))
