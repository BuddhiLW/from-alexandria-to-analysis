(ns alexandria.medium.html-labels
  "Letters on a MathBox scene as HTML, not as MathBox text.

   MathBox draws a Format/Label (and axis ticks) by writing the glyphs on a
   2D canvas and reading the pixels back (getImageData) into a texture.
   Browsers that defend against canvas fingerprinting (Mullvad Browser, Tor
   Browser, Firefox resistFingerprinting, Brave) answer that read-back with
   noise, and every letter turns into a hatched block. This overlay never
   reads a canvas: each label is an absolutely positioned div over the
   MathBox canvas, moved once per animation frame to the screen point where
   three.js' camera projects the label's point.

     [overlay {:box    an atom holding the MathBox root (the MathBox :ref)
               :range  the Cartesian's [[x0 x1] [y0 y1] [z0 z1]]
               :scale  the Cartesian's [sx sy sz] (default [1 1 1])
               :labels a seq of labels, or a fn of no arguments returning
                       one (called every frame)}]

   A label: {:text \"A\" :at [x y z] (the Cartesian's own coordinates, the
   ones the scene emits) :colour :opacity (0..1) :italic? :size (px)
   :offset [dx dy] (px, dy up, as MathBox's Label) :background :halo (the
   text-shadow colour, dark by default)}. A label
   whose point is not finite, lies behind the camera, or has opacity 0 is
   hidden. Mount the overlay as a sibling of the MathBox element inside a
   wrapper with position: relative.

   The only arithmetic here is the view's: the Cartesian's affine map from
   its range to MathBox world units (mathbox view/cartesian.js) and the
   camera's projection. The points themselves come from the scene's
   kernels."
  (:require [reagent.core :as r]))

(defn world
  "The Cartesian's map from its coordinates to world units: each axis's
   range onto [-scale, scale] (the Cartesian at the origin, unrotated)."
  [range scale]
  (let [scale (or scale [1 1 1])]
    (fn [p]
      (mapv (fn [x [lo hi] s] (* s (/ (- (* 2 x) (+ lo hi)) (- hi lo))))
            p range scale))))

(defn project
  "[sx sy] in pixels of a canvas w x h for world point [x y z] through
   camera (its matrixWorldInverse and projectionMatrix current), or nil
   when the point lies behind the camera."
  [^js camera w h [x y z]]
  (let [m (.. camera -matrixWorldInverse -elements)
        p (.. camera -projectionMatrix -elements)
        at (fn [^js a i] (aget a i))
        ;; view = M [x y z 1] (column-major)
        vx (+ (* (at m 0) x) (* (at m 4) y) (* (at m 8) z) (at m 12))
        vy (+ (* (at m 1) x) (* (at m 5) y) (* (at m 9) z) (at m 13))
        vz (+ (* (at m 2) x) (* (at m 6) y) (* (at m 10) z) (at m 14))
        vw (+ (* (at m 3) x) (* (at m 7) y) (* (at m 11) z) (at m 15))
        cx (+ (* (at p 0) vx) (* (at p 4) vy) (* (at p 8) vz) (* (at p 12) vw))
        cy (+ (* (at p 1) vx) (* (at p 5) vy) (* (at p 9) vz) (* (at p 13) vw))
        cw (+ (* (at p 3) vx) (* (at p 7) vy) (* (at p 11) vz) (* (at p 15) vw))]
    (when (> cw 1e-9)
      [(* w (/ (+ 1 (/ cx cw)) 2)) (* h (/ (- 1 (/ cy cw)) 2))])))

(defn- finite? [p] (and (sequential? p) (= 3 (count p)) (every? #(js/isFinite %) p)))

(defn- element!
  "The pool's i-th div, made on first use."
  [^js root pool i]
  (or (nth @pool i nil)
      (let [d (js/document.createElement "div")]
        (set! (.. d -style -cssText)
              (str "position:absolute;left:0;top:0;white-space:nowrap;pointer-events:none;"
                   "font-family:Georgia,'Times New Roman',serif;font-weight:bold;line-height:1;"
                   "padding:1px 3px;border-radius:3px;will-change:transform;display:none;"
                   ""))
        (.appendChild root d)
        (swap! pool conj d)
        d)))

(defn- place!
  "Set div d to label lab at screen point q (or hide it)."
  [^js d {:keys [text colour opacity italic? size offset background halo]} q]
  (let [s (.-style d)
        o (if (some? opacity) opacity 1)]
    (if (or (nil? q) (<= o 0.01))
      (set! (.-display s) "none")
      (let [[sx sy] q
            [dx dy] (or offset [10 12])]
        (when (not= (.-textContent d) text)
          (set! (.-textContent d) text)
          (.setAttribute d "data-label" text))
        (set! (.-display s) "block")
        (set! (.-opacity s) (str o))
        (set! (.-color s) (or colour "#ECEEE4"))
        (set! (.-fontSize s) (str (or size 17) "px"))
        (set! (.-fontStyle s) (if italic? "italic" "normal"))
        (set! (.-background s) (or background "transparent"))
        (let [h (or halo "rgba(0,0,0,0.9)")]
          (set! (.-textShadow s) (str "0 0 3px " h ",0 0 1px " h)))
        (set! (.-transform s)
              (str "translate(" (+ sx dx) "px," (- sy dy) "px) translate(-50%,-50%)"))))))

(defn draw!
  "One frame: every label of props placed over the canvas of the MathBox
   in (:box props), relative to the overlay root."
  [^js root pool {:keys [box range scale labels]}]
  (when-let [^js b (some-> box deref)]
    (when-let [^js three (.-three b)]
      (let [^js camera (.-camera three)
            ^js canvas (some-> three .-renderer .-domElement)]
        (when (and camera canvas)
          (.updateMatrixWorld camera)
          (let [cr (.getBoundingClientRect canvas)
                rr (.getBoundingClientRect root)
                ox (- (.-left cr) (.-left rr))
                oy (- (.-top cr) (.-top rr))
                w (.-width cr) h (.-height cr)
                ->world (world range scale)
                labs (vec (if (fn? labels) (labels) labels))]
            (dotimes [i (count labs)]
              (let [lab (nth labs i)
                    at (:at lab)
                    q (when (finite? at)
                        (when-let [[sx sy] (project camera w h (->world at))]
                          [(+ ox sx) (+ oy sy)]))]
                (place! (element! root pool i) lab q)))
            (doseq [d (drop (count labs) @pool)]
              (set! (.. ^js d -style -display) "none"))))))))

(defn overlay
  "Reagent component: the labels of props over a MathBox canvas (see the
   namespace doc). It runs its own requestAnimationFrame loop while
   mounted, so it follows the orbit camera between React renders."
  [props]
  (let [root (atom nil)
        pool (atom [])
        current (atom props)
        alive (atom true)
        tick (fn tick [_]
               (when @alive
                 (when-let [r @root]
                   (try (draw! r pool @current)
                        (catch :default e (js/console.warn "html-labels" e))))
                 (js/requestAnimationFrame tick)))]
    (js/requestAnimationFrame tick)
    (r/create-class
     {:display-name "html-labels"
      :component-will-unmount (fn [_] (reset! alive false))
      :reagent-render
      (fn [props]
        (reset! current props)
        [:div {:ref #(reset! root %)
               :data-html-labels "true"
               :style {:position "absolute" :left 0 :top 0 :right 0 :bottom 0
                       :overflow "hidden" :pointer-events "none" :z-index 2}}])})))
