(ns alexandria.design
  "Backend-neutral printable brachistochrone race. All coordinates are mm;
   the objective and the three paths come from Bernoulli's descent problem.
   The nodes are EDN, not Blender commands."
  (:require [alexandria.bernoulli.brachistochrone :as b]
            [clojure.edn :as edn]))

(def defaults
  {:span-mm 195.0 :drop-mm 97.5 :bed-mm 256.0 :marble-diameter-mm 12.0
   :wall-mm 2.0 :clearance-mm 0.6 :base-mm 4.0 :sample-count 120
   :lane-spacing-mm 26.0 :margin-mm 12.0})

(defn make-design
  "Compute scene nodes in millimetres from the descent-time objective."
  ([] (make-design defaults))
  ([constraints]
   (let [{:keys [span-mm drop-mm bed-mm marble-diameter-mm wall-mm clearance-mm base-mm sample-count lane-spacing-mm margin-mm] :as c} (merge defaults constraints)
         _ (when-not (and (pos? span-mm) (pos? drop-mm) (pos? wall-mm) (pos? marble-diameter-mm) (pos? sample-count)) (throw (ex-info "Positive dimensions and sample count required" {:constraints c})))
         X (/ span-mm 100.0) Y (/ drop-mm 100.0)
         paths (b/samples X Y sample-count)
         ts (b/times X Y)
         max-depth (apply max (map (fn [pts] (apply max (map second pts))) (vals paths)))
         channel-radius (+ (/ marble-diameter-mm 2) clearance-mm wall-mm)
         height (+ (* 100 max-depth) margin-mm marble-diameter-mm channel-radius 2)
         width (+ span-mm (* 2 margin-mm))
         depth (+ (* 2 lane-spacing-mm) marble-diameter-mm (* 2 wall-mm) (* 2 margin-mm))
         _ (when (> (apply max [width height depth]) bed-mm) (throw (ex-info "Design exceeds printer bed" {:dimensions [width height depth] :bed bed-mm})))
         lane (zipmap [:cycloid :arc :line] [(- lane-spacing-mm) 0.0 lane-spacing-mm])
         point (fn [kind [x y]] [(* 100 x) (- height margin-mm marble-diameter-mm (* 100 y)) (lane kind)])
         ;; Sample indices are derived from physical 40 mm spacing, not a fixed count.
         support-step (max 1 (int (Math/ceil (/ (* sample-count 40.0) span-mm))))
         nodes (vec
                (concat
                 [{:node :box :id :base :opts {:center [(/ span-mm 2) (- (/ base-mm 2)) 0] :size [width base-mm depth] :material :base}}]
                 (for [kind [:cycloid :arc :line]]
                   {:node :tube-along-curve :id kind
                    :opts {:points (mapv (partial point kind) (paths kind))
                           :inner-radius-mm (+ (/ marble-diameter-mm 2) clearance-mm)
                           :outer-radius-mm channel-radius :wall-mm wall-mm
                           :profile :open-u :material kind}})
                 (for [kind [:cycloid :arc :line]
                       i (range support-step sample-count support-step)
                       :let [[x y z] (point kind (nth (paths kind) i))]]
                   {:node :box :id (keyword (str (name kind) "-support-" i))
                    :opts {:center [x (/ y 2) z] :size [2 (+ y 2) 2] :material :base}})
                 [{:node :box :id :start-gate :opts {:center [0 (- height margin-mm (/ marble-diameter-mm 2)) 0]
                                                    :size [wall-mm (+ marble-diameter-mm (* 2 wall-mm)) depth] :material :gate}}
                  {:node :box :id :catch-tray :opts {:center [span-mm 2 0] :size [20 4 depth] :material :base}}]
                 (for [kind [:cycloid :arc :line]]
                   {:node :sphere :id (keyword (str (name kind) "-marble"))
                    :opts {:center (point kind [0 0]) :radius-mm (/ marble-diameter-mm 2) :material :steel :simulation-only true}})))]
     {:objective {:minimise :descent-time :expression "integral ds / sqrt(2 g y)"}
      :constraints c :units :millimetres :dimensions [width height depth]
      :times ts :finishing-order (b/finishing-order ts) :paths paths :nodes nodes})))

(defn printable-nodes [design]
  (remove #(get-in % [:opts :simulation-only]) (:nodes design)))

(defn save! [design path]
  (spit path (pr-str (dissoc design :paths)))
  path)

(defn load-design [path] (edn/read-string (slurp path)))
