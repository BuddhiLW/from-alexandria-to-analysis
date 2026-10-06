(ns alexandria.medium.svg
  "SVG hiccup in a y-up plane: every point is [x y] with y up, drawn at
   [x -y]. Pure; colours are CSS strings (see alexandria.palette)."
  (:require [clojure.string :as str]))

(defn- num-str [x]
  #?(:clj (format "%.4f" (double x))
     :cljs (.toFixed (js/Number x) 4)))

(defn points-attr
  "The SVG points attribute of ps."
  [ps]
  (str/join " " (map (fn [[x y]] (str (num-str x) "," (num-str (- y)))) ps)))

(defn polygon
  "opts: :stroke :fill :width :opacity, and :attrs merged last (e.g.
   alexandria.medium.anim/create)."
  [ps {:keys [stroke fill width opacity attrs] :or {fill "none" width 0.012 opacity 1}}]
  [:polygon (merge {:points (points-attr ps) :fill fill :fill-opacity opacity
                    :stroke (or stroke "none") :stroke-width width :stroke-linejoin "round"}
                   attrs)])

(defn polyline
  "opts: :stroke :width :dash, and :attrs merged last."
  [ps {:keys [stroke width dash attrs] :or {width 0.012}}]
  [:polyline (merge {:points (points-attr ps) :fill "none" :stroke stroke :stroke-width width
                     :stroke-dasharray dash :stroke-linejoin "round" :stroke-linecap "round"}
                    attrs)])

(defn segment [a b style] (polyline [a b] style))

(defn circle
  "opts: :stroke :fill :width :opacity, and :attrs merged last."
  [[cx cy] r {:keys [stroke fill width opacity attrs] :or {fill "none" width 0.012 opacity 1}}]
  [:circle (merge {:cx cx :cy (- cy) :r r :fill fill :fill-opacity opacity
                   :stroke (or stroke "none") :stroke-width width}
                  attrs)])

(defn dot [[x y] colour] [:circle {:cx x :cy (- y) :r 0.03 :fill colour}])

(defn text
  "Text at point [x y]. opts: :size, :anchor (start|middle|end), :colour,
   :italic?, :mono?."
  [[x y] s {:keys [size anchor colour italic? mono?] :or {size 0.11 anchor "middle"}}]
  [:text {:x x :y (- y) :font-size size :text-anchor anchor :fill colour
          :font-family (if mono? "ui-monospace, monospace" "ui-serif, Georgia, serif")
          :font-style (if italic? "italic" "normal")}
   s])

(defn group
  "Children under one opacity."
  [opacity & children]
  (into [:g {:opacity opacity}] children))

(defn layer
  "Children under group attributes, e.g. alexandria.medium.anim/fade."
  [attrs & children]
  (into [:g attrs] children))

(defn view-box
  "The viewBox attribute of the y-up window [x0 x1] by [y0 y1]."
  [[x0 x1] [y0 y1]]
  (str/join " " (map num-str [x0 (- y1) (- x1 x0) (- y1 y0)])))
