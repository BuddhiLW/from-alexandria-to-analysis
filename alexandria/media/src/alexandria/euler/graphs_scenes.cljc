(ns alexandria.euler.graphs-scenes
  "The seven bridges of Koenigsberg (E53), one drawing per stage
   (alexandria.medium.scene/draw): the city map collapses to Euler's figure
   of four points and seven lines, the degrees are counted, the letters do
   not fit.

   ctx :data carries alexandria.euler.graphs data in plain numbers:
     :city      {:regions {k {:node :outline}} :bridges {id {:feet}}}
     :bridges   [{:id :ends}]
     :degrees   {k d}
     :table     Euler's table (par. 14)"
  (:require [alexandria.euler.draw :as d]
            [alexandria.medium.anim :as a]
            [alexandria.medium.plane :as plane]
            [alexandria.medium.scene :as scene]
            [alexandria.medium.svg :as svg]
            [clojure.string :as str]
            #?(:cljs [alexandria.medium.player :as player])
            [alexandria.medium.math :as m]))

#?(:cljs (def render
           "Clerk's render-fn for these scenes."
           player/render))

(def ^:private scene-id :euler/koenigsberg)
(def ^:private water "#2c5d7c")
(def ^:private land "#3a4a3f")

;; ---------------------------------------------------------------------------
;; The map and its graph

(defn- bends
  "{bridge-id bend}: parallel bridges between the same two regions bow
   apart, a single bridge is straight."
  [bridges]
  (into {}
        (mapcat (fn [[_ bs]]
                  (let [n (count bs)]
                    (map-indexed (fn [i {:keys [id]}] [id (if (= 1 n) 0 (* 0.32 (- i (/ (dec n) 2))))]) bs)))
                (group-by (comp set :ends) bridges))))

(defn- bridge-path
  "The bridge as drawn at collapse t: its span between its feet (t = 0),
   the line between the two region points (t = 1)."
  [{:keys [city]} bend {:keys [id ends]} t]
  (let [[u v] ends
        node #(get-in city [:regions % :node])
        dist (fn [[x y] [p q]] (m/hypot (- x p) (- y q)))
        [f0 f1] (get-in city [:bridges id :feet])
        [f0 f1] (if (<= (dist f0 (node u)) (dist f1 (node u))) [f0 f1] [f1 f0])
        line (d/quad (node u) (node v) bend)
        span (d/sample #(plane/lerp-point f0 f1 %) 0 1 24)]
    (mapv #(plane/lerp-point %1 %2 t) span line)))

(defn- region-shape
  "Region k's outline shrunk towards its point by t."
  [{:keys [city]} k t]
  (let [{:keys [node outline]} (get-in city [:regions k])]
    (mapv #(plane/lerp-point % node t) outline)))

(defn- map-and-graph
  "The city at collapse t (0 the map, 1 Euler's figure); opts: :highlight
   region keys, :bridge-f progress of the bridge labels."
  [palette data t {:keys [highlight label-f] :or {label-f 1}}]
  (let [bs (bends (:bridges data))
        regions (keys (get-in data [:city :regions]))]
    (into [:g
           (svg/polygon [[-2.7 -1.7] [2.7 -1.7] [2.7 1.7] [-2.7 1.7]] {:fill water :opacity (* 0.55 (- 1 t))})]
          (concat
           (for [k regions]
             (svg/polygon (region-shape data k t)
                          {:fill (if (contains? highlight k) (:found palette) land)
                           :opacity (- 1 (* 0.85 t)) :stroke (:muted palette) :width 0.01}))
           (for [{:keys [id] :as b} (:bridges data)
                 :let [ps (bridge-path data (bs id) b t)]]
             [:g (svg/polyline ps {:stroke (:construction palette) :width (+ 0.06 (* -0.035 t))})
              (d/label palette (nth ps 12) (name id) label-f {:dx 0.05 :dy 0.05 :size 0.1
                                                             :colour (:construction palette)})])
           (for [k regions
                 :let [p (get-in data [:city :regions k :node])]]
             [:g (d/dot p (if (contains? highlight k) (:found palette) (:ink palette)) 0.07 t)
              (d/label palette p (name k) 1 {:dx 0.1 :dy 0.1 :size 0.17})])))))

;; ---------------------------------------------------------------------------
;; Stages

(defmethod scene/draw [scene-id :city] [_ _ p {:keys [palette data]}]
  [:g (map-and-graph palette data 0 {:label-f (a/play p 0.2 0.8)})
   (d/readout palette (a/play p 0.5 0.8) [-2.6 -1.85]
              ["A the Kneiphof, B and C the banks, D between the branches"])])

(defmethod scene/draw [scene-id :collapse] [_ _ p {:keys [palette data]}]
  (let [t (a/play p 0.1 0.8)]
    [:g (map-and-graph palette data t {})
     (d/readout palette (a/play p 0.75 0.95) [-2.6 -1.85]
                ["4 points, 7 lines: only which region a bridge joins matters"])]))

(defn- walk-letters
  "A walk A-B-D-C over bridges a, f, g: its record grows one letter a bridge."
  [palette data p]
  (let [bs (bends (:bridges data))
        by-id (into {} (map (juxt :id identity)) (:bridges data))
        walk [[:A :a :B] [:B :f :D] [:D :g :C]]
        fs (a/lagged p 3 0.9 0.1 0.8)
        letters (str/join (cons "A" (keep-indexed (fn [i [_ _ v]] (when (pos? (nth fs i)) (name v))) walk)))]
    [:g (map-and-graph palette data 1 {})
     (into [:g] (map (fn [[_ id _] f] (d/stroke (bridge-path data (bs id) (by-id id) 1) (:found palette) 0.05 f))
                     walk fs))
     (d/readout palette 1 [-2.6 -1.85] [(str "record: " letters "   (" (dec (count letters)) " bridges, " (count letters) " letters)")])]))

(defmethod scene/draw [scene-id :letters] [_ _ p {:keys [palette data]}]
  (walk-letters palette data p))

(defmethod scene/draw [scene-id :degree] [_ _ p {:keys [palette data]}]
  (let [bs (bends (:bridges data))
        at-a (filter #(some #{:A} (:ends %)) (:bridges data))
        fs (a/lagged p (count at-a) 0.6 0.05 0.6)]
    [:g (map-and-graph palette data 1 {:highlight #{:A}})
     (into [:g] (map (fn [b f] (d/stroke (bridge-path data (bs (:id b)) b 1) (:found palette) 0.05 f)) at-a fs))
     (d/readout palette (a/play p 0.55 0.8) [-2.6 -1.85]
                ["5 bridges at A: in, out, in, out, in: A is written (5 + 1)/2 = 3 times"])]))

(defmethod scene/draw [scene-id :count] [_ _ p {:keys [palette data]}]
  (let [{:keys [rows sum prefixed]} (:table data)
        fs (a/lagged p (count rows) 0.5 0.05 0.6)]
    [:g (map-and-graph palette data 1 {:highlight (set (map :region rows))})
     (into [:g] (map-indexed (fn [i [{:keys [region degree letters]} f]]
                               (d/readout palette f [1.15 (- 1.45 (* 0.26 i))]
                                          [(str (name region) ": " degree " bridges, " letters " letters")]))
                             (map vector rows fs)))
     (d/readout palette (a/play p 0.65 0.85) [1.15 0.25]
                [(str "needed: " sum " letters") (str "a walk of 7 bridges: " prefixed) "impossible"])]))

(defmethod scene/draw [scene-id :handshake] [_ _ p {:keys [palette data]}]
  (let [bs (bends (:bridges data))
        n (count (:bridges data))
        fs (a/lagged p n 0.5 0.05 0.75)
        total (reduce + (vals (:degrees data)))]
    [:g (map-and-graph palette data 1 {})
     (into [:g] (map (fn [b f]
                       (let [ps (bridge-path data (bs (:id b)) b 1)]
                         [:g (d/dot (first ps) (:found palette) 0.05 f) (d/dot (peek ps) (:found palette) 0.05 f)]))
                     (:bridges data) fs))
     (d/readout palette (a/play p 0.7 0.9) [-2.6 -1.85]
                [(str "5 + 3 + 3 + 3 = " total " = 2 x " n ": each bridge has two ends")])]))

(defmethod scene/draw [scene-id :rule] [_ _ p {:keys [palette data]}]
  [:g (map-and-graph palette data (- 1 (a/there-and-back p)) {:highlight #{:A :B :C :D}})
   (d/readout palette (a/play p 0.1 0.35) [-2.6 -1.85]
              ["odd regions: 0 → from anywhere; 2 → from one of them; 4 here → none"])])

;; ---------------------------------------------------------------------------
;; Par. 11-21: even counts, the table, fifteen bridges, doubled bridges

(defn- graph-at
  "An abstract multigraph: nodes {k [x y]} as dots with Euler's letters,
   bridges [{:id :ends}] as bowed lines; `lit` {bridge-id f} strokes the
   crossed ones to fraction f in the found colour."
  [palette nodes bridges lit highlight]
  (let [bs (bends bridges)]
    (into [:g]
          (concat
           (for [{:keys [id ends] :as b} bridges
                 :let [[u v] ends ps (d/quad (nodes u) (nodes v) (bs id))]]
             [:g (svg/polyline ps {:stroke (:construction palette) :width 0.025})
              (d/stroke ps (:found palette) 0.05 (get lit id 0))
              (d/label palette (nth ps 12) (name id) 1 {:dx 0.03 :dy 0.03 :size 0.085
                                                        :colour (:construction palette)})])
           (for [[k p] nodes]
             [:g (d/dot p (if (contains? highlight k) (:found palette) (:ink palette)) 0.07 1)
              (d/label palette p (name k) 1 {:dx 0.1 :dy 0.1 :size 0.17})])))))

(defn- walk-record
  "The letters of the first n steps [[from bridge to] ...] of a walk."
  [steps n]
  (apply str (name (ffirst steps)) (map (fn [[_ b v]] (str (name b) (name v))) (take n steps))))

(defn- play-walk
  "A walk drawn crossing by crossing over the graph, its record growing."
  [palette p {:keys [nodes bridges steps]} highlight caption]
  (let [n (count steps)
        fs (a/lagged p n 0.85 0.05 0.85)
        lit (into {} (map (fn [[_ b _] f] [b f]) steps fs))
        done (count (filter #(>= % 1) fs))]
    [:g (graph-at palette nodes bridges lit highlight)
     (d/readout palette 1 [-2.6 -1.65] [(walk-record steps done) caption])]))

(defmethod scene/draw [scene-id :even] [_ _ p {:keys [palette]}]
  (let [nodes {:A [0 0.2] :B [-1.4 0.2] :C [1.4 0.2]}
        bridges [{:id :a :ends [:A :B]} {:id :b :ends [:A :B]}
                 {:id :c :ends [:A :C]} {:id :d :ends [:A :C]}]
        from-a [[:A :a :B] [:B :b :A] [:A :c :C] [:C :d :A]]
        from-b [[:B :a :A] [:A :c :C] [:C :d :A] [:A :b :B]]
        first-half (a/play p 0 0.5)
        second-half (a/play p 0.5 1)]
    (if (< p 0.5)
      (play-walk palette first-half {:nodes nodes :bridges bridges :steps from-a} #{:A}
                 "4 bridges at A, start in A: A written 4/2 + 1 = 3 times")
      (play-walk palette second-half {:nodes nodes :bridges bridges :steps from-b} #{:A}
                 "start elsewhere: A written 4/2 = 2 times"))))

(defmethod scene/draw [scene-id :table] [_ _ p {:keys [palette data]}]
  (let [{:keys [rows sum prefixed bridges]} (:table data)
        fs (a/lagged p (count rows) 0.5 0.05 0.6)]
    [:g (map-and-graph palette data 1 {:highlight (set (map :region rows))})
     (d/readout palette (a/play p 0 0.15) [0.95 1.6] [(str "bridges " bridges ", above: " prefixed)])
     (into [:g] (map-indexed (fn [i [{:keys [region degree letters even?]} f]]
                               (d/readout palette f [0.95 (- 1.3 (* 0.24 i))]
                                          [(str (name region) (if even? "*" " ") "  " degree "   " letters)]))
                             (map vector rows fs)))
     (d/readout palette (a/play p 0.7 0.9) [0.95 0.2]
                [(str "sum " sum (if (> sum prefixed) " > " " = ") prefixed)])]))

(defmethod scene/draw [scene-id :fifteen] [_ _ p {:keys [palette data]}]
  (let [{:keys [table] :as f} (:fifteen data)]
    (play-walk palette p f (set (:odd f))
               (str "15 bridges, " (:sum table) " letters = 15 + 1: from D or E"))))

(defmethod scene/draw [scene-id :doubled] [_ _ p {:keys [palette data]}]
  (play-walk palette p (:doubled data) #{}
             "each bridge twice: every count even, a walk from any region"))
