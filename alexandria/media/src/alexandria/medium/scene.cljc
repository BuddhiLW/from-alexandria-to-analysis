(ns alexandria.medium.scene
  "What a proof looks like at each step: `draw` is open on [scene
   stage-kind], so a proof adds its drawings by defmethod and the player
   never changes.

     stage   a keyword, or a vector [kind & args] (e.g. [:upper 2])
     p       progress through the step, in [0 1]
     ctx     {:figures {name IFigure} :data {...} :palette {...}}")

(defn stage-kind [stage] (if (vector? stage) (first stage) stage))

(defmulti draw
  "SVG hiccup of `scene` at `stage`, progress p."
  (fn [scene stage _p _ctx] [scene (stage-kind stage)]))

(defmethod draw :default [scene stage _ {:keys [palette]}]
  [:text {:x 0 :y 0 :font-size 0.15 :fill (:muted palette)}
   (str "no drawing for " scene " " (pr-str stage))])
