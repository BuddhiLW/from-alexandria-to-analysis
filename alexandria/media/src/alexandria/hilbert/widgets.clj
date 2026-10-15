(ns alexandria.hilbert.widgets
  "Hilbert's five axiom groups, and computed Desargues configurations in the
   Euclidean and Moulton planes."
  (:require [alexandria.hilbert.moulton :as m]
            [alexandria.medium.flow :as flow]
            [emmy.mafs :as mafs]
            [emmy.viewer :as ev]
            [emmy.viewer.compile :as vc]
            [emmy.viewer.raster]))

(def axiom-groups
  [["I. Incidence" "Which points, lines and planes lie on one another."]
   ["II. Order" "Between-ness on each line, including Pasch's axiom."]
   ["III. Parallels" "At most one non-intersecting line through a point."]
   ["IV. Congruence" "Transport and comparison of segments and angles."]
   ["V. Continuity" "Archimedean and completeness conditions."]])

(defn axiom-table
  "The five groups as cards; the group the Moulton model gives up is marked."
  []
  [:div {:data-figure "hilbert-axiom-groups"
         :style {:display "flex" :flex-wrap "wrap" :gap "0.6em"}}
   (for [[title text] axiom-groups]
     (let [weak (= title "I. Incidence")]
       [:div {:key title :data-group title
              :style {:flex "1 1 160px" :padding "0.6em" :border-radius "6px"
                      :border (str "2px solid " (if weak "#C0392B" "#1F6FB4"))}}
        [:strong title] [:p {:style {:margin "0.3em 0"}} text]
        (when weak
          [:p {:style {:color "#C0392B" :margin 0}}
           "Plane incidence survives in Moulton's plane; the spatial axioms that force Desargues do not."])]))])

(defn- xy [o i] [(list 'aget o (* 2 i)) (list 'aget o (inc (* 2 i)))])

(defn- point [o i colour label]
  (let [[x y] (xy o i)]
    [:<> (mafs/point {:x x :y y :color colour})
     (mafs/text label {:x x :y y :attach "nw"})]))

(defn- segment [o i j colour]
  (mafs/segment {:point1 (xy o i) :point2 (xy o j) :color colour}))

(defn- diagram
  "Sides BC and B'C' are drawn through their kinks on x = 0 (points 10 and
   11), straight when the bend is 1; grey extensions reach X, Y, Z along
   the straight left branches. A' lies beyond O on the ray from A."
  [o]
  (mafs/mafs {:view-box {:x [-7 4] :y [-6 4]} :height 390}
             (into [:<>]
                   (concat (for [[a b] [[1 2] [2 10] [10 3] [3 1] [4 5] [5 11] [11 6] [6 4]]]
                             (segment o a b (if (or (< a 4) (= a 10)) "#1F6FB4" "#E39B2D")))
                           (for [[a b] [[0 1] [0 2] [0 3] [0 4]]]
                             (segment o a b "#8A9BB0"))
                           (for [[a b] [[2 7] [5 7] [2 8] [5 8] [1 9] [4 9]]]
                             (segment o a b "#C9D3DE"))
                           [(segment o 7 8 "#C0392B") (segment o 8 9 "#C0392B")]
                           (for [[i name] (map-indexed vector ["O" "A" "B" "C" "A′" "B′" "C′" "X" "Y" "Z"])]
                             (point o i (if (< i 7) "#1F6FB4" "#C0392B") name))))))

(defn independence-player
  "Switch models or set the bend. Each configuration and the signed area
   of XYZ are recomputed by the same Emmy function compiled to a raster
   kernel."
  []
  (binding [vc/*backend* :raster]
    (ev/with-let [!s {:bend 1}]
      (let [[k form] (flow/kernel "moulton" (fn [b] (fn [_] (m/coordinates b))) [1] [0])]
        (flow/template
         {'?s !s '?k k 'DIAGRAM (diagram 'o)}
         (list 'reagent.core/with-let [k form]
               '(let [bend (:bend @?s) o (?k (array 0) (array bend))
                      gap (aget o 24) aligned (< (js/Math.abs gap) 0.001)]
                  [:div {:data-figure "hilbert-moulton" :data-residual (str gap)
                         :data-model (if aligned "euclidean" "moulton")}
                   [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em"}}
                    [:div {:style {:flex "1 1 460px"}} DIAGRAM]
                    [:div {:style {:flex "1 1 220px"}}
                     [:p "Two triangles are perspective from O. X, Y and Z, in red, are the intersections of corresponding sides."]
                     [:button {:type "button" :data-model-button "euclidean"
                               :on-click (fn [] (swap! ?s assoc :bend 1))} "Straight lines"]
                     [:button {:type "button" :data-model-button "moulton"
                               :on-click (fn [] (swap! ?s assoc :bend 0.5))} "Moulton lines"]
                     [:label {:style {:display "block" :margin-top "0.5em"}}
                      "bend of falling lines right of x = 0: " (.toFixed bend 2)
                      [:input {:type "range" :min 0.25 :max 1 :step 0.05 :value bend :style {:width "100%"}
                               :on-change (fn [ev] (swap! ?s assoc :bend (js/parseFloat (.. ev -target -value))))}]]
                     [:p {:data-verdict true} (if aligned "Desargues holds: X, Y, Z are collinear."
                                                  "Desargues fails: X, Y, Z are not collinear.")]
                     [:p "Signed double area of XYZ, from raster: " [:span {:data-gap true} (.toFixed gap 4)]]]]])))))))
