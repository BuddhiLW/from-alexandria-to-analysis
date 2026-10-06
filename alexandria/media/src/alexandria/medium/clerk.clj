(ns alexandria.medium.clerk
  "Mounting a proof player in a Clerk notebook. The player runs in the
   browser (alexandria.medium.player); `render-fn` names the namespace whose
   scene methods it draws with, which Clerk loads with :require-cljs."
  (:require [alexandria.medium.kernel :as kernel]
            [nextjournal.clerk :as clerk]))

(defn viewer
  "The Clerk viewer of a proof player whose scenes live with render-fn."
  [render-fn]
  {:name render-fn
   :require-cljs true
   :transform-fn clerk/mark-presented
   :render-fn render-fn})

(defn player
  "A Clerk value playing `steps` ([{:claim :why :stage}]) with scene
   `scene-id`. figures: {name {:f :params :state}}, compiled to kernels here;
   data: the numbers the scene shows; opts: :durations :window :height
   :palette."
  [render-fn scene-id steps figures data opts]
  (clerk/with-viewer (viewer render-fn)
    (merge {:scene scene-id
            :steps steps
            :kernels (kernel/kernels figures)
            :data data}
           opts)))
