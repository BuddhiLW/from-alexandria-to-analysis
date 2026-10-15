(ns alexandria.dedekind.playground-scenes
  "Dedicated SVG figures for each of six passages; raster-backed geometry and live controls."
  (:require [alexandria.medium.figure :as figure]
            [alexandria.medium.svg :as svg]
            [reagent.core :as r]))

(defn fmt [x] (.toFixed (js/Number x) 3))
(defn choose [s label values]
  [:label label [:select {:value (get @s (first values))
                          :on-change #(swap! s assoc (first values) (js/parseFloat (.. % -target -value)))}
                  (for [[v text] (second values)] ^{:key v} [:option {:value v} text])]])
(defn slider [s k label lo hi step]
  [:label label " " (fmt (get @s k))
   [:input {:type "range" :data-play-control (name k) :min lo :max hi :step step
            :value (get @s k) :on-change #(swap! s assoc k (js/parseFloat (.. % -target -value)))}]])
(defn preset [s label patch] [:button {:type "button" :on-click #(swap! s merge patch)} label])
(defn frame [window & elements]
  (into [:svg {:view-box (apply svg/view-box window)
               :style {:width "100%" :height "300px" :background "#faf7f0"}}] elements))
(defn drag [state key px py lo hi x0 x1]
  [:circle {:cx px :cy (- py) :r 0.035 :fill "#d75848" :style {:cursor "ew-resize"}
            :data-drag (name key)
            :on-pointer-down #(.setPointerCapture (.-currentTarget %) (.-pointerId %))
            :on-pointer-move (fn [evt]
                               (when (.hasPointerCapture (.-currentTarget evt) (.-pointerId evt))
                                 (let [box (.getBoundingClientRect (.-ownerSVGElement (.-currentTarget evt)))
                                       p (+ x0 (* (/ (- (.-clientX evt) (.-left box)) (.-width box)) (- x1 x0)))]
                                   (swap! state assoc key (max lo (min hi p))))))}])
(defn line [a b colour] (svg/segment a b {:stroke colour :width 0.009}))
(defn raster [f params xs]
  (figure/points f params (mapv vector xs)))
(defn cut-figure [s f]
  (let [{:keys [d bound cut]} @s
        root (js/Math.sqrt d)
        xs (for [q (range 1 (inc bound)) p (range 0 (* 2 q))] (/ p q))
        ys (raster f [cut d] xs)
        below (filter #(neg? (second %)) ys)
        above (filter #(pos? (second %)) ys)]
    [:div
     (frame [[0 2.05] [-0.45 0.65]]
            (line [0 0] [2 0] "#293a52")
            (for [[x _] below] ^{:key (str "b" x)} (svg/dot [x -0.07] "#537caa"))
            (for [[x _] above] ^{:key (str "a" x)} (svg/dot [x 0.07] "#d9a23c"))
            (line [cut -0.23] [cut 0.28] "#d75848")
            (drag s :cut cut 0.33 0 2 0 2.05))
     [:p {:data-case true} (if (< (js/Math.abs (- cut root)) 0.012)
                             (if (some #(<= (js/Math.abs (- % cut)) 0.0001) xs)
                               "rational cut: a boundary fraction belongs to Q"
                               "irrational cut: no greatest A₁ or least A₂")
                             "Move the cut to √D: the classes cease to coincide with its square test")]
     [:p {:data-readout true} "D = " (fmt d) "; √D = " (fmt root)
      "; boundary = " (fmt cut) "; A₁ samples " (count below) "; A₂ samples " (count above)]
     (slider s :d "number D" 1 3 0.01) (slider s :bound "denominator bound" 2 32 1)
     (slider s :cut "cut position" 0 2 0.001)
     [:p "Try this: choose √3; use a rational cut; return to √2. Eudoxus, Elements V Def. 5, sorts m/n by the same comparison."]
     (preset s "√3" {:d 3 :cut 1.732}) (preset s "3/2" {:d 2.25 :cut 1.5})
     (preset s "Reset" {:d 2 :bound 16 :cut 1.414})]))
(defn zigzag-figure [s f data]
  (let [i (max 1 (int (:index @s)))
        v (nth (:rationals data) (dec i))
        x (first (first (raster f [i] [i])))]
    [:div
     (frame [[0 6.2] [-4.5 0.8]]
            (for [q (range 1 9) p (range 1 9)]
              ^{:key (str p "/" q)} [:g
                                      (svg/dot [(* 0.7 p) (* -0.48 q)] (if (= (+ p q) (+ 2 (int (/ i 4)))) "#d9a23c" "#537caa"))
                                      (svg/text [(* 0.7 p) (* -0.48 q)] (str p "/" q) {:size 0.11})])
            (drag s :index (min 5.6 (* 0.07 x)) 0.35 1 80 0 80))
     [:p {:data-case true} "Every reduced fraction gets a finite place; unreduced cells are skipped"]
     [:p {:data-readout true} "bijection index N = " i "; rₙ = " v]
     (slider s :index "N" 1 80 1)
     [:p "Try this: advance the enumeration; jump to the 30th fraction; reset."]
     (preset s "Next" {:index (min 80 (inc i))}) (preset s "N = 30" {:index 30}) (preset s "Reset" {:index 8})]))
(defn height-figure [s f data]
  (let [h (max 1 (int (:height @s)))
        rows (take h (:rows data))
        last-row (last rows)
        cursor (first (first (raster f [h] [h])))]
    [:div
     (frame [[-3.3 3.3] [-0.5 1.5]]
            (line [-3.1 0] [3.1 0] "#293a52")
            (for [row rows item row]
              ^{:key (str (:N item) (:label item))}
              [:g (svg/dot [(:x item) (* 0.16 (:N item))] "#537caa")
               (svg/text [(:x item) (+ 0.17 (* 0.16 (:N item)))] (:label item) {:size 0.095})])
            (drag s :height (min 3 cursor) -0.3 1 4 1 4))
     [:p {:data-case true} (if (< h 4) "Finite list at each height" "Height 4: √2 and the golden ratio appear")]
     [:p {:data-readout true} "h = " h "; φ(h) = " (count last-row) "; listed so far = " (reduce + (map count rows))]
     (slider s :height "height h" 1 4 1)
     [:p "Try this: inspect height 1, then height 4; reset."]
     (preset s "h = 1" {:height 1}) (preset s "h = 4" {:height 4}) (preset s "Reset" {:height 2})]))
(defn diagonal-figure [s f data]
  (let [rows (:rows data)
        row (int (:row @s)) col (int (:col @s))
        changed (reduce (fn [rs [i j]] (update-in rs [i j] #(- 1 %))) rows (keys (:edits @s)))
        digits (mapv (fn [i] (- 1 (get-in changed [i i]))) (range 8))
        cursor (first (first (raster f [col] [col])))]
    [:div
     (frame [[-0.6 8.5] [-9.4 1]]
            (for [i (range 8) j (range 8)]
              ^{:key (str i "-" j)}
              [:g (svg/text [(+ j 0.3) (- -0.4 i)] (str (get-in changed [i j]))
                            {:size 0.42 :colour (if (= i j) "#d75848" "#293a52")})
               (when (and (= i row) (= j col))
                 (svg/circle [(+ j 0.3) (- -0.4 i)] 0.37 {:stroke "#d9a23c"}))])
            (drag s :col cursor 0 0 7 0 7))
     [:p {:data-case true} "The flipped diagonal differs from every row at that row's own place"]
     [:p {:data-readout true} "new number = 0." (apply str digits) "₂; edited row " (inc row) ", column " (inc col)]
     (slider s :row "row" 0 7 1) (slider s :col "digit" 0 7 1)
     [:button {:type "button" :on-click #(swap! s update-in [:edits [row col]] not)} "Flip digit"]
     [:p "Try this: edit the first diagonal digit; edit the last; reset."]
     (preset s "First" {:row 0 :col 0}) (preset s "Last" {:row 7 :col 7})
     (preset s "Reset" {:row 0 :col 0 :edits {}})]))
(defn measure-figure [s f data]
  (let [{:keys [eps depth]} @s
        ps (take (int depth) (:rationals data))
        lengths (map-indexed (fn [i _] (* eps (js/Math.pow 0.5 (inc i)))) ps)
        total (reduce + lengths)
        edge (second (first (raster f [eps depth] [1])))]
    [:div
     (frame [[-0.1 1.1] [-0.4 0.48]]
            (line [0 0] [1 0] "#293a52")
            (for [[p len] (map vector ps lengths)]
              ^{:key p} (svg/polygon [[(- p (/ len 2)) -0.09] [(+ p (/ len 2)) -0.09]
                                      [(+ p (/ len 2)) 0.09] [(- p (/ len 2)) 0.09]]
                                     {:fill "#537caa" :opacity 0.35}))
            (drag s :eps (min 1 eps) 0.31 0.01 1 -0.1 1.1))
     [:p {:data-case true} "A finite cover never covers every rational; the infinite cover has measure zero as ε → 0"]
     [:p {:data-readout true} "Σₙ≤N ε/2ⁿ = " (fmt total) " < ε = " (fmt eps)
      "; next interval scale = " (fmt edge)]
     (slider s :eps "ε" 0.01 1 0.01) (slider s :depth "enumeration depth N" 1 30 1)
     [:p "Try this: shrink ε; increase N; reset."]
     (preset s "Small ε" {:eps 0.05}) (preset s "Deep cover" {:depth 25}) (preset s "Reset" {:eps 0.5 :depth 8})]))
(defn dirichlet-figure [s f]
  (let [{:keys [n tag]} @s
        n (int n)
        cells (range n)
        position (first (first (raster f [tag] [tag])))]
    [:div
     (frame [[-0.05 1.05] [-0.3 1.3]]
            (for [i cells :let [l (/ i n) r (/ (inc i) n)]]
              ^{:key i} [:g
                         (svg/polygon [[l 0] [r 0] [r 1] [l 1]] {:fill "#d9a23c" :opacity 0.3 :stroke "#d9a23c" :width 0.003})
                         (line [l 0] [r 0] "#537caa")])
            (drag s :tag position 1.14 0 1 -0.05 1.05))
     [:p {:data-case true} "Not Riemann-integrable: both rational and irrational points occur in every cell"]
     [:p {:data-readout true} "n = " n "; upper U = " (fmt 1) "; lower L = " (fmt 0)
      "; U − L = " (fmt 1) "; Lebesgue integral = " (fmt 0)]
     (slider s :n "partition n" 2 36 1) (slider s :tag "inspect tag" 0 1 0.01)
     [:p "Try this: refine n; change the tag; reset: the gap never closes."]
     (preset s "Refine" {:n 32}) (preset s "Right tag" {:tag 1}) (preset s "Reset" {:n 8 :tag 0.5})]))
(defn playground [scene initial kernels data]
  (r/with-let [s (r/atom initial)
               f (figure/kernel-figure (get kernels (if (= scene :cut) :curve
                                                     (if (= scene :measure) :cover :position))))]
    [:section {:data-playground (name scene) :class "rigor-playground"}
     [:h4 (case scene :cut "Dedekind's cut" :zigzag "Cantor's zigzag" :heights "Algebraic height"
                :diagonal "Cantor's editable diagonal" :measure "Lebesgue's ε-cover" :dirichlet "Dirichlet's partition")]
     (case scene :cut (cut-figure s f) :zigzag (zigzag-figure s f data)
           :heights (height-figure s f data) :diagonal (diagonal-figure s f data)
           :measure (measure-figure s f data) :dirichlet (dirichlet-figure s f))]))
(defn render [value] [playground (:scene value) (:initial value) (:kernels value) (:data value)])
