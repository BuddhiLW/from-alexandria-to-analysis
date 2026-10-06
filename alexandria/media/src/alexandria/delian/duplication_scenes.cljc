(ns alexandria.delian.duplication-scenes
  "The doubling of the cube on the proof player. Every curve and every
   moving point is a figure of alexandria.delian.duplication (a raster
   kernel in the browser); every printed number comes from ctx :data,
   computed on the JVM by alexandria.raster. Scenes do layout and lerps only.

     :delian/hippocrates  the cube, the eightfold cube, the doubled square
     :delian/menaechmus   the two parabolas and the hyperbola; slider a
     :delian/mesolabe     Eratosthenes' frames sliding to the means
     :delian/conchoid     Nicomedes' curve traced
     :delian/cissoid      Diocles' curve traced"
  (:require [alexandria.medium.anim :as a]
            [alexandria.medium.figure :as figure]
            [alexandria.medium.scene :as scene]
            [alexandria.medium.svg :as svg]
            #?(:cljs [alexandria.medium.player :as player])))

#?(:cljs (def render "Clerk's render-fn for these scenes." player/render))

(defn- fmt [x digits]
  #?(:clj (format (str "%." digits "f") (double x))
     :cljs (.toFixed (js/Number x) digits)))

(defn- states
  "n + 1 states [u] evenly from u0 to u1 (memoized: a figure reuses its
   Float64Array while the states are identical)."
  [u0 u1 n]
  (mapv (fn [i] [(+ u0 (* (- u1 u0) (/ i n)))]) (range (inc n))))

(def ^:private states* (memoize states))

(defn- curve [figures k params u0 u1 n style]
  (svg/polyline (figure/points (get figures k) params (states* u0 u1 n)) style))

(defn- draw-on
  "The first fraction f of a curve's points (Manim's Create on a polyline)."
  [figures k params u0 u1 n f style]
  (let [ps (figure/points (get figures k) params (states* u0 u1 n))
        m (max 2 (int (* f (count ps))))]
    (svg/polyline (subvec ps 0 (min m (count ps))) style)))

;; ---------------------------------------------------------------------------
;; Hippocrates: the cube, the eightfold cube, the square on the diagonal

(defn- cube-at [[x y] s colour]
  (let [d (* 0.45 s)]
    [:g (svg/polygon [[x y] [(+ x s) y] [(+ x s) (+ y s)] [x (+ y s)]] {:stroke colour :width 0.02})
     (svg/polygon [[x (+ y s)] [(+ x d) (+ y s d)] [(+ x s d) (+ y s d)] [(+ x s) (+ y s)]] {:stroke colour :width 0.02})
     (svg/polygon [[(+ x s) y] [(+ x s d) (+ y d)] [(+ x s d) (+ y s d)] [(+ x s) (+ y s)]] {:stroke colour :width 0.02})]))

(defmethod scene/draw [:delian/hippocrates :cube] [_ _ p {:keys [palette]}]
  (svg/layer (a/fade (a/play p 0 0.4)) (cube-at [0 0] 1 (:ink palette))
             (svg/text [0.5 -0.3] "the altar: side a, volume a^3" {:colour (:ink palette) :size 0.13})))

(defmethod scene/draw [:delian/hippocrates :eight] [_ _ p {:keys [palette]}]
  (let [f (a/play p 0 0.7)]
    [:g (cube-at [0 0] 1 (:ink palette))
     (svg/layer (a/fade f) (cube-at [1.6 0] 2 (:given palette))
                (svg/text [2.6 -0.3] "side 2a: volume 8 a^3, not 2 a^3" {:colour (:given palette) :size 0.13}))]))

(defmethod scene/draw [:delian/hippocrates :square] [_ _ p {:keys [palette]}]
  (let [f (a/play p 0 0.6)]
    [:g (svg/polygon [[0 0] [1 0] [1 1] [0 1]] {:stroke (:ink palette) :width 0.02})
     (svg/segment [0 0] [1 1] {:stroke (:found palette) :width 0.02})
     (svg/layer (a/fade f)
                (svg/polygon [[0 0] [1 1] [0 2] [-1 1]] {:stroke (:found palette) :fill (:found palette) :opacity 0.15 :width 0.02})
                (svg/text [0 2.2] "the square on the diagonal is double (Meno)" {:colour (:found palette) :size 0.13}))]))

(defmethod scene/draw [:delian/hippocrates :means] [_ _ p {:keys [palette data]}]
  (let [c (:cube-root-2 data)
        ls [["a" 1] ["x" c] ["y" (* c c)] ["2a" 2]]
        fs (a/lagged p 4 0.4 0 0.8)]
    (into [:g (svg/text [1 1.9] (str "a : x = x : y = y : 2a,  x = " (fmt c 6) " a  (raster)") {:colour (:ink palette) :size 0.12 :mono? true})]
          (map-indexed (fn [i [label len]]
                         (let [yy (- 1.4 (* 0.45 i))]
                           [:g (svg/segment [0 yy] [(* len (nth fs i)) yy] {:stroke (if (#{1 2} i) (:found palette) (:ink palette)) :width 0.04})
                            (svg/text [-0.15 (- yy 0.04)] label {:colour (:ink palette) :anchor "end" :size 0.13})]))
                       ls))))

;; ---------------------------------------------------------------------------
;; Menaechmus: x^2 = a y, y^2 = 2a x, x y = 2 a^2 with the slider a

(defn- axes [palette]
  [:g (svg/segment [-0.2 0] [4.2 0] {:stroke (:ink palette) :width 0.01})
   (svg/segment [0 -0.2] [0 4.2] {:stroke (:ink palette) :width 0.01})
   (svg/text [4.25 -0.15] "x" {:colour (:ink palette) :size 0.13 :italic? true})
   (svg/text [-0.15 4.2] "y" {:colour (:ink palette) :size 0.13 :italic? true})])

(defn- menaechmus-figure [stage p {:keys [palette figures data controls]}]
  (let [av (or (:a controls) 1.4)
        b (* 2 av)
        c (:cube-root-2 data)
        f #(a/play p 0 0.8)
        show (fn [k] (#{k} stage))
        [[mx my]] (figure/points (:meeting figures) [av c] [[0]])
        par1 (fn [fr] (draw-on figures :parabola-x [av] 0 2.4 80 fr {:stroke (:given palette) :width 0.025}))
        par2 (fn [fr] (draw-on figures :parabola-y [b] 0 2.8 80 fr {:stroke (:found palette) :width 0.025}))
        hyp (fn [fr] (draw-on figures :hyperbola [av b] (/ (* av b) 4.2) 4.2 120 fr {:stroke (:ink palette) :width 0.02 :dash "0.06 0.04"}))]
    [:g (axes palette)
     (case stage
       :given (svg/text [2 3.6] "given A = a and E = 2a; seek the means" {:colour (:ink palette) :size 0.14})
       :lines [:g (svg/segment [0 0] [mx 0] {:stroke (:found palette) :width 0.03})
               (svg/segment [mx 0] [mx (* (a/play p 0 0.8) my)] {:stroke (:found palette) :width 0.03})]
       :parabola (par2 (f))
       :hyperbola [:g (par2 1) (hyp (f))]
       :second [:g (par2 1) (hyp 1) (par1 (f))]
       [:g (par2 1) (hyp 1) (when (#{:second :cube-root :meet} stage) (par1 (if (show :meet) 0 1)))])
     (when (#{:meet :cube-root :second} stage)
       [:g (svg/dot [mx my] (:found palette))
        (svg/segment [mx 0] [mx my] {:stroke (:found palette) :width 0.012 :dash "0.04 0.03"})
        (svg/segment [0 my] [mx my] {:stroke (:found palette) :width 0.012 :dash "0.04 0.03"})
        (svg/text [(+ mx 0.08) (+ my 0.12)] "Theta" {:colour (:found palette) :size 0.12 :italic? true :anchor "start"})])
     (svg/text [2.2 -0.45]
               (str "a = " (fmt av 2) ":  x = " (fmt mx 4) " = a . " (fmt c 6) "  (cube root of 2, raster brent)")
               {:colour (:ink palette) :size 0.11 :mono? true})]))

(doseq [stage [:given :lines :parabola :hyperbola :meet :second :cube-root]]
  (defmethod scene/draw [:delian/menaechmus stage] [_ st p ctx] (menaechmus-figure st p ctx)))

;; ---------------------------------------------------------------------------
;; Eratosthenes' mesolabe: a = 2 (AE), b = 1 (DTheta), w = 1

(defn- mesolabe-figure [s p {:keys [palette figures data]}]
  (let [av 2 w 1
        [A B G D] (figure/points (:mesolabe figures) [av w s] [[0] [1] [2] [3]])
        edges (figure/points (:frame-edge figures) [w s] [[0] [1] [2]])
        frame (fn [[x0 _] colour]
                [:g (svg/polygon [[x0 0] [(+ x0 w) 0] [(+ x0 w) av] [x0 av]] {:stroke colour :width 0.015})
                 (svg/segment [x0 av] [(+ x0 w) 0] {:stroke colour :width 0.012})])]
    [:g (svg/segment [-0.2 0] [3.4 0] {:stroke (:ink palette) :width 0.012})
     (frame (nth edges 2) (:ink palette)) (frame (nth edges 1) (:ink palette)) (frame (nth edges 0) (:given palette))
     (svg/polyline [A B G D] {:stroke (:found palette) :width 0.02})
     (for [[pt label] (map vector [A B G D] ["A" "B" "G" "D"])]
       [:g {:key label} (svg/dot pt (:found palette))
        (svg/segment pt [(first pt) 0] {:stroke (:found palette) :width 0.025})
        (svg/text [(first pt) (+ (second pt) 0.12)] label {:colour (:found palette) :size 0.12 :italic? true})])
     (svg/dot [(first D) 1] (:given palette))
     (svg/text [1.6 2.5]
               (let [[x y] (:mesolabe-means data)]
                 (str "AE = 2, DTheta = 1:  means " (fmt x 4) ", " (fmt y 4) "  (raster)"))
               {:colour (:ink palette) :size 0.11 :mono? true})]))

(defn- slide-at [p data]
  (let [s* (:mesolabe-slide data)] (+ 1 (* (- s* 1) (a/play p 0.05 0.9)))))

(defmethod scene/draw [:delian/mesolabe :frames] [_ _ _ {:as ctx}] (mesolabe-figure 1 0 ctx))
(defmethod scene/draw [:delian/mesolabe :slide] [_ _ p {:keys [data] :as ctx}] (mesolabe-figure (slide-at p data) p ctx))
(doseq [stage [:line :means]]
  (defmethod scene/draw [:delian/mesolabe stage] [_ _ p {:keys [data] :as ctx}]
    (mesolabe-figure (:mesolabe-slide data) p ctx)))

;; ---------------------------------------------------------------------------
;; Nicomedes' conchoid: pole (0, -1), ruler y = 0, interval 2

(defn- conchoid-figure [p {:keys [palette figures]}]
  (let [d 1 l 2
        f (a/play p 0 0.85 a/linear)
        phi (+ 0.35 (* f 2.44))
        [[cx cy]] (figure/points (:conchoid figures) [d l] [[phi]])
        rx (* cx (/ d (+ cy d)))]
    [:g (svg/segment [-3.5 0] [3.5 0] {:stroke (:ink palette) :width 0.015})
     (svg/dot [0 (- d)] (:ink palette))
     (svg/text [0.15 (- -1 0.1)] "pole E" {:colour (:ink palette) :size 0.12 :anchor "start"})
     (draw-on figures :conchoid [d l] 0.35 2.79 120 f {:stroke (:found palette) :width 0.025})
     (svg/segment [0 (- d)] [cx cy] {:stroke (:given palette) :width 0.012})
     (svg/segment [rx 0] [cx cy] {:stroke (:found palette) :width 0.035})
     (svg/text [0 2.6] "the segment beyond the ruler always has the interval's length" {:colour (:ink palette) :size 0.12})]))

(doseq [stage [:trace :neusis]]
  (defmethod scene/draw [:delian/conchoid stage] [_ _ p ctx] (conchoid-figure p ctx)))

;; ---------------------------------------------------------------------------
;; Diocles' cissoid: circle of diameter 2 (r = 1), vertex at the origin

(defn- cissoid-figure [p {:keys [palette figures]}]
  (let [f (a/play p 0 0.85)
        circle-pts (mapv (fn [i] (let [t (* 2 Math/PI (/ i 120))] [(+ 1 (Math/cos t)) (Math/sin t)])) (range 121))]
    [:g (svg/circle [1 0] 1 {:stroke (:ink palette), :width 0.015})
     (svg/segment [-0.2 0] [2.3 0] {:stroke (:ink palette) :width 0.01})
     (draw-on figures :cissoid [1] 0.001 1.6 100 f {:stroke (:found palette) :width 0.025})
     (svg/text [1 1.4] "Diocles' curve: (2r - u) : h = h : u = u : y" {:colour (:ink palette) :size 0.12})]))

(doseq [stage [:trace :proportion]]
  (defmethod scene/draw [:delian/cissoid stage] [_ _ p ctx] (cissoid-figure p ctx)))
