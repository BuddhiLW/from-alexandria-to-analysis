(ns alexandria.apollonius.playground-widget
  "The conics playground of the Apollonius notebook: polar coordinates as a
   projection (widget brief, section 5).

   A right cone of half-angle alpha (apex at the origin, axis z up) is cut
   by the plane z = h + x tan(tilt). Every point X of the section is
   dropped straight down onto the plane z = 0 through the apex, at right
   angles to the axis, and its shadow X' traces

     rho = l / (1 - e cos theta),  l = h tan(alpha),  e = tan(tilt) tan(alpha)

   about the foot of the axis, which is the focus: Pappus' focus and
   directrix, read off a projection of Apollonius' cone. The derivation is
   Emmy's (alexandria.apollonius.projection); this namespace draws it.

   One shared state {:alpha :tilt :h :l :e} (degrees for the angles), every
   key a live Leva control, tied by the equations (brief section 5):

     tilt or h moves   alpha fixed, l and e re-solved (forward map)
     e or l moves      alpha fixed, tilt and h re-solved (inverse map)
     alpha moves       the plane (tilt, h) fixed, l and e re-solved

   Only the touched key is a source. The solve is one raster kernel (`tie`)
   that returns the whole state for each possible source, clamped so that
   no key leaves its slider's range (the source is held at the edge where
   a solved key would leave). The page pushes the solved keys back into the
   atom; emmy.leva's Controls watches the atom and calls Leva's `set`, so
   the panel follows. A push never re-fires the solver: the watch is off
   while the page pushes (origin guard), and a change of a key that comes
   back within half a slider step of the value just pushed is an echo of
   the push, not the reader (epsilon guard).

   Every curve, surface and number is an Emmy function compiled by
   emmy.viewer.compile/*backend* :raster (bound here, whatever the
   caller's): MathBox and Mafs plots through emmy-viewers, and three
   kernels the page calls itself: `tie`, `readout` (the formula's numbers)
   and `point` (the moving X, its shadow X', theta and rho). The point runs
   round the section with alexandria.medium.anim (play, linear).

   No 3D text: the scene carries no labels; the letters and numbers are in
   the HTML beside it."
  (:require [alexandria.apollonius.projection :as pj]
            [alexandria.raster :as raster]
            [clojure.walk :as walk]
            [emmy.env :as e]
            [emmy.leva :as leva]
            [emmy.mafs :as mafs]
            [emmy.mathbox.plot :as plot]
            [emmy.viewer :as ev]
            [emmy.viewer.compile :as vc]
            [emmy.viewer.raster]
            [nextjournal.clerk :as clerk]
            [emmy.clerk :as ec]))

;; ---------------------------------------------------------------------------
;; The drawn region and the sliders

(def reach "The drawn radius about the axis." 3.0)
(def depth "The drawn half-height above and below the apex." 3.0)

(def ranges
  "Each key's slider range [min max step]; the tie keeps every key in it."
  {:alpha [10 60 0.5] :tilt [0 85 0.5] :h [0.2 2.5 0.01] :l [0.1 3 0.01] :e [0 3 0.01]})

(def tied [:alpha :tilt :h :l :e])

(def eps
  "How close to zero a clamped denominator may come."
  1e-3)

(def parabola-band "|e - 1| below this names the section a parabola." 0.005)

;; ---------------------------------------------------------------------------
;; Emmy helpers (numbers and symbols alike; branch-free for raster)

(defn- rad [d] (e/* d (/ Math/PI 180)))
(defn- deg [r] (e/* r (/ 180 Math/PI)))

(defn- at-least [x lo] (e/* 0.5 (e/+ x lo (e/abs (e/- x lo)))))
(defn- at-most [x hi] (e/* 0.5 (e/- (e/+ x hi) (e/abs (e/- x hi)))))
(defn- clamp [x lo hi] (at-most (at-least x lo) hi))

(defn- mask
  "1 where q >= 0, NaN where q < 0 (f64.sqrt of a negative): MathBox draws
   no segment there."
  [q]
  (e// (e/sqrt q) (e/abs (e/sqrt q))))

(defn- lo [k] (first (ranges k)))
(defn- hi [k] (second (ranges k)))

;; ---------------------------------------------------------------------------
;; The tie: the whole state for each source, by the forward and inverse maps

(defn- from-alpha
  "alpha moved, the plane (tilt, h) fixed: alpha held (in degrees, so an
   unclamped alpha comes back exactly) where l and e stay in range, then
   l and e forward."
  [alpha tilt h]
  (let [tt (e/tan (rad tilt))
        a (clamp alpha
                 (at-least (lo :alpha) (deg (e/atan (e// (lo :l) h))))
                 (at-most (at-most (hi :alpha) (deg (e/atan (e// (hi :l) h))))
                          (deg (e/atan (e// (hi :e) (at-least tt 1e-9))))))
        [l ecc] (pj/forward (rad a) (rad tilt) h)]
    [a tilt h l ecc]))

(defn- from-tilt
  "tilt moved, alpha and h fixed: tilt held (in degrees, so an unclamped
   tilt comes back exactly) where e stays in range, then l and e forward."
  [alpha tilt h]
  (let [ta (e/tan (rad alpha))
        t (at-most tilt (deg (e/atan (e// (hi :e) ta))))
        [l ecc] (pj/forward (rad alpha) (rad t) h)]
    [alpha t h l ecc]))

(defn- from-h
  "h moved, alpha and tilt fixed: h held where l stays in range, then l
   and e forward."
  [alpha tilt h]
  (let [ta (e/tan (rad alpha))
        h' (clamp h (at-least (lo :h) (e// (lo :l) ta)) (at-most (hi :h) (e// (hi :l) ta)))
        [l ecc] (pj/forward (rad alpha) (rad tilt) h')]
    [alpha tilt h' l ecc]))

(defn- from-l
  "l moved, alpha and e fixed: l held where h stays in range, then h and
   tilt by the inverse map."
  [alpha l ecc]
  (let [ta (e/tan (rad alpha))
        l' (clamp l (at-least (lo :l) (e/* (lo :h) ta)) (at-most (hi :l) (e/* (hi :h) ta)))
        [h t] (pj/inverse (rad alpha) l' ecc)]
    [alpha (deg t) h l' ecc]))

(defn- from-e
  "e moved, alpha and l fixed: e held where tilt stays in range, then h
   and tilt by the inverse map."
  [alpha l ecc]
  (let [ta (e/tan (rad alpha))
        e' (clamp ecc (lo :e) (at-most (hi :e) (e/* (e/tan (rad (hi :tilt))) ta)))
        [h t] (pj/inverse (rad alpha) l e')]
    [alpha (deg t) h l e']))

(def source-block
  "Where each source's state starts in the output of `tie`."
  {:alpha 0 :tilt 5 :h 10 :l 15 :e 20})

(defn tie
  "The tie kernel: for the state [alpha tilt h l e], the solved state for
   each source in the order of source-block (25 numbers). The page keeps
   the block of the key the reader touched."
  [alpha tilt h l ecc]
  (fn [_]
    (vec (concat (from-alpha alpha tilt h) (from-tilt alpha tilt h) (from-h alpha tilt h)
                 (from-l alpha l ecc) (from-e alpha l ecc)))))

(defn readout
  "The numbers of the formula beside the sliders: [tan(alpha) tan(tilt) h
   l e (h tan alpha) (tan tilt tan alpha) l/(1+e) (the near vertex) -l/e
   (the directrix)]."
  [alpha tilt h l ecc]
  (fn [_]
    (let [ta (e/tan (rad alpha)) tt (e/tan (rad tilt))]
      [ta tt h l ecc (e/* h ta) (e/* tt ta) (e// l (e/+ 1 ecc)) (e/- 0 (e// l (at-least ecc eps)))])))

(def point-index
  "Where each quantity sits in the output of `point`: the 3D side (rho x y
   z plane-z, from the cone and the plane), `shown`, theta, and the 2D side
   (rho2 x2 y2, from l and e)."
  {:rho 0 :x 1 :y 2 :z 3 :plane-z 4 :shown 5 :theta 6 :rho2 7 :x2 8 :y2 9})

(defn point
  "The moving point at the angle theta (degrees), computed twice.

   3D, from the cone and the plane alone (alpha, tilt, h): along the ray at
   theta, rho is the root of Emmy's elimination (projection/elimination,
   solved by projection/solve-linear-in), X = (x, y, rho / tan alpha), and
   the plane's height h + x tan(tilt) over the same (x, y) (it equals z: X
   is on the plane). X' = (x, y, 0).

   2D, from the focal form alone (l, e): rho2 = l / (1 - e cos theta),
   (x2, y2) = rho2 (cos theta, sin theta).

   The two agree when the state is tied; the scene draws the 3D point, the
   Mafs panel the 2D one. `shown` = 1 when X lies in the drawn region, NaN
   otherwise (also where rho is infinite)."
  [alpha tilt h l ecc theta]
  (fn [_]
    (let [t (rad theta)
          c (e/cos t)
          rho (:root (pj/solve-linear-in (pj/elimination (rad alpha) (rad tilt) h c)))
          x (e/* rho c) y (e/* rho (e/sin t)) z (e// rho (e/tan (rad alpha)))
          rho2 (e// l (e/- 1 (e/* ecc c)))]
      [rho x y z (e/+ h (e/* x (e/tan (rad tilt))))
       (e/* (mask (e/- (* reach reach) (e/square rho))) (mask (e/- (* depth depth) (e/square z))))
       theta
       rho2 (e/* rho2 c) (e/* rho2 (e/sin t))])))

;; ---------------------------------------------------------------------------
;; The curves and surfaces

(defn- polar-rho
  "rho = l / (1 - e cos t) on one branch: the near branch (denominator held
   at eps or above) or the far one (held at -eps or below); far points land
   at l/eps, outside the drawn region."
  [l ecc t branch]
  (let [d (e/- 1 (e/* ecc (e/cos t)))]
    (e// l (if (= branch :near) (at-least d eps) (at-most d (e/- 0 eps))))))

(defn cone-surface
  "The double cone of half-angle alpha, u in [-1 1] (height u times the
   drawn half-height, cut at the drawn radius), v the angle."
  [alpha]
  (let [ta (e/tan (rad alpha))
        zc (at-most depth (e// reach ta))]
    (fn [[u v]]
      (let [z (e/* u zc)]
        [(e/* z ta (e/cos v)) (e/* z ta (e/sin v)) z]))))

(defn plane-surface
  "The cutting plane z = h + x tan(tilt) over (u, w) in [-1 1]^2, x cut
   where z leaves the drawn height."
  [tilt h]
  (let [tt (at-least (e/tan (rad tilt)) 1e-6)
        x0 (at-least (- reach) (e// (e/- (- depth) h) tt))
        x1 (at-most reach (e// (e/- depth h) tt))]
    (fn [[u w]]
      (let [x (e/+ x0 (e/* 0.5 (e/+ u 1) (e/- x1 x0)))]
        [x (e/* w reach) (e/+ h (e/* x (e/tan (rad tilt))))]))))

(defn- in-region
  "1 inside the drawn region (|rho| <= reach, |z| <= depth), NaN outside."
  [rho z]
  (e/* (mask (e/- (* reach reach) (e/square rho))) (mask (e/- (* depth depth) (e/square z)))))

(defn section
  "The section on the cone (branch :near, through the vertex nearest the
   axis' foot, or :far, the opposite branch on the other cone, I.14),
   theta = t in [-pi pi]; NaN outside the drawn region."
  [branch]
  (fn [alpha l ecc]
    (fn [t]
      (let [rho (polar-rho l ecc t branch)
            z (e// rho (e/tan (rad alpha)))
            m (in-region rho z)]
        [(e/* m rho (e/cos t)) (e/* m rho (e/sin t)) (e/* m z)]))))

(defn shadow
  "The section dropped onto z = 0: rho = l / (1 - e cos theta) in the
   base plane, one branch, where the section itself is drawn."
  [branch]
  (fn [alpha l ecc]
    (fn [t]
      (let [rho (polar-rho l ecc t branch)
            m (in-region rho (e// rho (e/tan (rad alpha))))]
        [(e/* m rho (e/cos t)) (e/* m rho (e/sin t)) 0]))))

(defn curtain
  "The vertical drop lines from the section to its shadow, as a surface
   (theta, s) -> (x, y, s z) whose theta-grid lines are the drop lines."
  [branch]
  (fn [alpha l ecc]
    (fn [[t s]]
      (let [rho (polar-rho l ecc t branch)
            z (e// rho (e/tan (rad alpha)))
            m (in-region rho z)]
        [(e/* m rho (e/cos t)) (e/* m rho (e/sin t)) (e/* m s z)]))))

(defn base-disk
  "The plane z = 0 through the apex as a polar grid about the axis."
  [[r phi]]
  [(e/* r (e/cos phi)) (e/* r (e/sin phi)) 0])

(defn conic-2d
  "The shadow in the Mafs panel: (rho cos t, rho sin t), one branch."
  [branch]
  (fn [l ecc]
    (fn [t]
      (let [rho (polar-rho l ecc t branch)]
        [(e/* rho (e/cos t)) (e/* rho (e/sin t))]))))

(defn directrix
  "x = -l/e (Mafs of-y); at e = 0 it sits at -l/eps, out of view."
  [l ecc]
  (fn [_y] (e/- 0 (e// l (at-least ecc eps)))))

;; ---------------------------------------------------------------------------
;; The starting state, by raster

(def init-plane "alpha, tilt (degrees) and h of the starting plane." [35 30 1.2])

(defn init
  "The starting state, every value from the tie's :tilt block run by
   raster on the JVM (l and e from the forward map)."
  []
  (let [[alpha tilt h] init-plane]
    (zipmap tied (raster/value from-tilt alpha tilt h))))

;; ---------------------------------------------------------------------------
;; The page

(def colours
  {:cone "#8A9BB0" :plane "#5AA0D0" :section "#C0392B" :shadow "#1F6FB4"
   :curtain "#7B5EA7" :grid "#3C4650" :point "#111111" :directrix "#E67E22"})

(def quote-pappus
  "Heath (1896), Introduction, on Pappus' lemma to Euclid's Surface-loci."
  "This lemma states, and gives a complete proof of, the proposition that the locus of a point whose distance from a given point is in a given ratio to its distance from a fixed line is a conic section, and is an ellipse, a parabola, or a hyperbola according as the given ratio is less than, equal to, or greater than, unity.")

(def quote-cone
  "Heath (1896), The Cone (Book I, the definitions)."
  "If a straight line indefinite in length, and passing always through a fixed point, be made to move round the circumference of a circle which is not in the same plane with the point, so as to pass successively through every point of that circumference, the moving straight line will trace out the surface of a double cone, or two similar cones lying in opposite directions and meeting in the fixed point, which is the apex of each cone.")

(def labels {:alpha "α (cone) °" :tilt "tilt τ °" :h "h" :l "ℓ" :e "e"})

(defn- schema []
  (into {} (map (fn [k] (let [[a b s] (ranges k)] [k {:min a :max b :step s :label (labels k)}])) tied)))

(defn- kernel-binding
  "[sym form]: f compiled by the current backend, called in the browser as
   (sym (array 0) (array ...params))."
  [nm f init-params]
  [(gensym nm) (vc/compiled-fn f init-params [0] {:simplify? false})])

(defn- fmt
  "Browser form: x to n places with a true minus sign."
  ([x] (fmt x 2))
  ([x n] (list 'let ['v x] (list 'if '(js/isFinite v) (list '.replace (list '.toFixed 'v n) "-" "−") "∞"))))

(defn- state-array [!p]
  (list* 'array (map #(ev/get !p %) tied)))

(defn- tie-solve
  "Browser form: solve from the touched key `src` with the tie kernel. The
   exact state !s takes the reader's value of `src` (from the panel atom
   !p) and keeps every other key exact; the tie kernel's block for `src`
   becomes the new !s, and the keys of !p that differ are pushed into the
   panel. The push waits for the current notification to finish
   (setTimeout 0): pushed from inside the watch, Leva's own watch on the
   same atom would run after it with the stale outer state and put the
   old values back. Counts solves in globalThis.__playgroundSolves (the
   e2e checks one solve per edit)."
  [tie-sym !p !s]
  (list 'js/setTimeout
        (list 'fn []
              (list 'let ['cur (list 'assoc (list 'deref !s) 'src (list 'get (list 'deref !p) 'src))
                          'out (list tie-sym '(array 0) (list* 'array (map (fn [k] (list 'get 'cur k)) tied)))
                          'off (list 'get source-block 'src)
                          'solved (list 'zipmap tied (list 'map '(fn [j] (aget out (+ off j))) '(range 5)))
                          'shown (list 'deref !p)
                          'push '(into {} (filter (fn [[k v]] (> (js/Math.abs (- v (get shown k))) 1e-12)) solved))]
                    '(set! (.-__playgroundSolves js/globalThis) (inc (or (.-__playgroundSolves js/globalThis) 0)))
                    (list 'reset! !s 'solved)
                    (list 'when '(seq push)
                          '(reset! pushing true)
                          (list 'swap! !p 'merge 'push)
                          '(reset! pushing false)
                          '(swap! pushed merge push)
                          '(js/setTimeout (fn [] (swap! pushed (fn [m] (apply dissoc m (keys push))))) 600))))
        0))

(defn- tie-watch
  "Browser form: the watch on the panel atom !p that re-solves the exact
   state !s from the touched key. Origin guard: nothing while the page
   itself pushes (`pushing`). Epsilon guard: Leva answers a push by
   calling onChange with the pushed value rounded to its slider step; a
   change of a key that lands within half a step of the value just pushed
   (`pushed`) is that echo, not the reader, and solves nothing (the exact
   value stays in !s)."
  [tie-sym !p !s]
  (let [half (into {} (map (fn [[k [_ _ s]]] [k (/ s 2)]) ranges))]
    (list 'add-watch !p ::tie
          (list 'fn ['_ '_ 'old 'new]
                (list 'when-not '@pushing
                      (list 'let ['src (list 'first (list 'filter '(fn [k] (not= (get old k) (get new k))) tied))
                                  'echo? (list 'and 'src '(contains? @pushed src)
                                               (list '<= '(js/Math.abs (- (get new src) (get @pushed src)))
                                                     (list '+ 1e-9 (list 'get half 'src))))]
                            (list 'when (list 'and 'src '(not echo?)) (tie-solve tie-sym !p !s))))))))

(defn- formula
  "The formula with the current values, from the readout kernel `r`."
  []
  (let [n (fn [i] (fmt (list 'aget 'r i) 3))]
    [:div {:style {:font-size "0.95em" :line-height 1.5}}
     (list 'let ['tex (list 'str "\\rho = \\frac{\\ell}{1 - e\\cos\\theta} = \\frac{" (fmt '(aget r 3) 3)
                            "}{1 - " (fmt '(aget r 4) 3) "\\,\\cos\\theta}")]
           '[:div {:data-formula tex} [:f> nextjournal.clerk.render/render-katex tex {:inline? false}]])
     [:div {:style {:font-family "ui-monospace, monospace" :font-size "0.92em"}}
      [:div "ℓ = h·tan α = " (n 2) " × " (n 0) " = " [:span {:data-l '(str (aget r 5))} (n 5)]]
      [:div "e = tan τ·tan α = " (n 1) " × " (n 0) " = " [:span {:data-e '(str (aget r 6))} (n 6)]]
      [:div "vertex ρ(180°) = ℓ/(1+e) = " (n 7) ";  directrix x = −ℓ/e = " (n 8)]]
     (list 'let ['ecc '(aget r 6)
                 'kind (list 'cond (list '< '(js/Math.abs (- ecc 1)) parabola-band) "parabola"
                             '(< ecc 1) "ellipse" :else "hyperbola")]
           [:div {:style {:font-size "1.25em" :font-weight 600 :margin "0.3em 0"}}
            [:span {:data-conic-name 'kind} 'kind]
            '(case kind "ellipse" " (e < 1): ἔλλειψις" "parabola" " (e = 1): παραβολή"
                   " (e > 1): ὑπερβολή, both branches, the far one on the opposite cone (I.14)")])]))

(defn- x-readout
  "Browser form: theta, rho, X and X' from the point kernel's output `q`.
   data-x3 carries X' from the cone and the plane, data-x2 X' from l and e
   (the e2e compares them)."
  []
  (let [n (fn [k] (fmt (list 'aget 'q (point-index k)) 2))]
    [:div {:style {:font-family "ui-monospace, monospace" :font-size "0.9em" :margin-top "0.4em"}
           :data-x-theta '(str (aget q 6)) :data-x-rho '(str (aget q 0))
           :data-x3 '(str (aget q 1) " " (aget q 2)) :data-x2 '(str (aget q 8) " " (aget q 9))}
     [:div "θ = " (n :theta) "°,  ρ = " (n :rho)]
     [:div "X = (" (n :x) ", " (n :y) ", " (n :z) ") on the cone and the plane (h + x tan τ = " (n :plane-z) ")"]
     [:div "X′ = (" (n :x) ", " (n :y) ", 0), straight below; from ℓ and e: (" (n :x2) ", " (n :y2) ")"]]))

(def scene-range
  "The scene's Cartesian range."
  [[(- reach) reach] [(- reach) reach] [(- depth) depth]])

(defn- scene
  "The MathBox scene on the exact state !s; `x3d` is the moving point's
   component. Its root lands in the atom `box` for the HTML letters."
  [!s x3d]
  (let [pf (fn [params f] (ev/with-params {:atom !s :params params} f))
        sec [:alpha :l :e]]
    (plot/scene
     {:range scene-range
      :scale [1 1 1]
      :ref '(fn [b] (reset! box b))
      :camera [1.1 -2.2 0.9]
      :axes [] :grids []
      :container {:style {:height "520px" :width "100%"}}}
     (plot/parametric-surface {:f base-disk :u [0 reach] :v [0 (* 2 Math/PI)]
                               :u-samples 7 :v-samples 73
                               :color (colours :grid) :opacity 0.06 :shaded? false
                               :grid-u 6 :grid-v 12 :grid-opacity 0.35})
     (plot/parametric-surface {:f (pf [:alpha] cone-surface) :u [-1 1] :v [0 (* 2 Math/PI)]
                               :u-samples 32 :v-samples 64
                               :color (colours :cone) :opacity 0.25
                               :grid-u 8 :grid-v 12 :grid-opacity 0.2 :simplify? false})
     (plot/parametric-surface {:f (pf [:tilt :h] plane-surface) :u [-1 1] :v [-1 1]
                               :u-samples 12 :v-samples 12
                               :color (colours :plane) :opacity 0.2 :shaded? false :simplify? false})
     (into [:<>]
           (for [b [:near :far]]
             [:<>
              (plot/parametric-surface {:f (pf sec (curtain b)) :u [(- Math/PI) Math/PI] :v [0 1]
                                        :u-samples 256 :v-samples 2
                                        :color (colours :curtain) :opacity 0.05 :shaded? false
                                        :grid-u 36 :grid-v 2 :grid-opacity 0.45 :simplify? false})
              (plot/parametric-curve {:f (pf sec (section b)) :t [(- Math/PI) Math/PI]
                                      :samples 720 :width 6 :color (colours :section) :simplify? false})
              (plot/parametric-curve {:f (pf sec (shadow b)) :t [(- Math/PI) Math/PI]
                                      :samples 720 :width 5 :color (colours :shadow) :simplify? false})]))
     [x3d])))

(defn- mafs-panel
  "The Mafs panel: the shadow about the focus, both branches, the
   directrix, the focus, and the moving X' (`x2d`)."
  [!p x2d]
  (let [pf (fn [f] (ev/with-params {:atom !p :params [:l :e]} f))]
    (mafs/mafs
     {:view-box {:x [(- reach) reach] :y [(- reach) reach]} :height 420}
     (mafs/polar {:lines 0.5 :subdivisions 2})
     (mafs/of-y {:x (pf directrix) :color (colours :directrix) :style "dashed"})
     (mafs/parametric {:xy (pf (conic-2d :near)) :t [(- Math/PI) Math/PI] :color (colours :shadow)})
     (mafs/parametric {:xy (pf (conic-2d :far)) :t [(- Math/PI) Math/PI] :color (colours :shadow)})
     (mafs/point {:x 0 :y 0 :color :red})
     (mafs/text "F" {:x 0 :y 0 :attach "sw" :color :red})
     [x2d])))

(defn- moving-point
  "Browser forms [x3d x2d xr labels]: the reagent components of the moving
   point in the scene (X and X' from the cone and the plane), in the Mafs
   panel (X' from l and e), in the readout, and the HTML letters of the
   scene, each calling the point kernel `psym` on the exact state !s and
   theta."
  [psym !s !x]
  (let [q-form (list psym '(array 0)
                     (list* 'array (concat (map #(ev/get !s %) tied) [(ev/get !x :theta)])))
        X ['(aget q 1) '(aget q 2) '(aget q 3)]
        X' ['(aget q 1) '(aget q 2) 0]
        style {:halo "rgba(255,255,255,0.95)" :size 16 :offset [10 12]}]
    [(list 'fn []
           (list 'let ['q q-form]
                 (list 'when '(js/isFinite (aget q 5))
                       [:<>
                        (plot/point {:coords X :size 14 :color (colours :point)})
                        (plot/point {:coords X' :size 12 :color (colours :shadow)})
                        (plot/line {:coords [X X'] :color (colours :point) :width 3})
                        (plot/line {:coords [[0 0 0] X'] :color (colours :shadow) :width 2 :opacity 0.7})])))
     (list 'fn []
           (list 'let ['q q-form]
                 (list 'when '(and (js/isFinite (aget q 8)) (< (js/Math.abs (aget q 7)) 1e3))
                       [:<>
                        (mafs/segment {:point1 [0 0] :point2 ['(aget q 8) '(aget q 9)] :color :blue :opacity 0.6})
                        (mafs/point {:x '(aget q 8) :y '(aget q 9) :color :pink})
                        (mafs/text "X′" {:x '(aget q 8) :y '(aget q 9) :attach "ne"})])))
     (list 'fn [] (list 'let ['q q-form] (x-readout)))
     (list 'fn []
           (list 'let ['q q-form 'on (list 'if '(js/isFinite (aget q 5)) 1 0)]
                 [(assoc style :text "X" :at X :colour (colours :point) :opacity 'on)
                  (assoc style :text "X′" :at X' :colour (colours :shadow) :opacity 'on)
                  (assoc style :text "F" :at [0 0 0] :colour (colours :section) :offset [-12 -14])]))]))

(def period "Milliseconds for X to run once round theta." 12000)

(defn- runner
  "Browser form: a requestAnimationFrame loop that, while :play is on,
   runs X round the section: its own phase in [0 1) advances by the
   elapsed time over `period` and theta = -180 + 360 play(phase) (eased
   by alexandria.medium.anim, linear). The phase is kept here, not read
   back from the atom, because Leva rounds theta to its slider step and
   echoes it; only a change of theta by more than a step from the value
   written last (the reader dragging it) moves the phase. The frame id
   is kept in `raf`."
  [!x]
  (list 'let ['written '(atom nil)
              'phase (list 'atom (list '/ (list '+ (ev/get !x :theta) 180) 360))
              'step (list 'fn 'step ['now]
                          (list 'let ['th (ev/get !x :theta)]
                                '(when (and @written (> (js/Math.abs (- th @written)) 1))
                                   (reset! phase (/ (+ th 180) 360))))
                          (list 'when (list 'and '@t-last (ev/get !x :play))
                                (list 'swap! 'phase (list 'fn ['p] (list 'mod (list '+ 'p (list '/ '(- now @t-last) period)) 1)))
                                (list 'let ['th '(+ -180 (* 360 (alexandria.medium.anim/play @phase 0 1 alexandria.medium.anim/linear)))]
                                      '(reset! written th)
                                      (list 'swap! !x 'assoc :theta 'th)))
                          '(reset! t-last now)
                          '(reset! raf (js/requestAnimationFrame step)))]
        '(reset! raf (js/requestAnimationFrame step))))

(defn- template-body
  "The page around the three kernels. Two atoms hold the tied state: !p is
   the Leva panel's (Leva rounds what it shows to the slider step and
   echoes that back), !s the exact solved state every kernel and plot
   reads, so the scene, the Mafs panel and the formula agree to the last
   digit whatever the panel shows. !x holds theta and play."
  [[tsym tform] [rsym rform] [psym pform] !p !s !x]
  (let [[x3d x2d xr labels] (moving-point psym !s !x)]
    (list 'reagent.core/with-let
          [tsym tform rsym rform psym pform
           'pushing '(atom false)
           'pushed '(atom {})
           '_ (tie-watch tsym !p !s)
           'box '(atom nil)
           'raf '(atom nil)
           't-last '(atom nil)
           '_ (runner !x)
           'x3d x3d 'x2d x2d 'xr xr 'labels labels]
          (list 'let ['r (list rsym '(array 0) (state-array !s))]
                [:div {:data-figure "conics-playground"}
                 [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
                  [:div {:style {:flex "1 1 420px" :min-width "300px" :position "relative"}}
                   ['alexandria.medium.html-labels/overlay {:box 'box :range scene-range :labels 'labels}]
                   (scene !s 'x3d)]
                  [:div {:style {:flex "0 1 300px" :min-width "260px"}}
                   (leva/sub-panel {:fill true :flat true :title-bar false}
                                   (leva/controls {:atom !p :schema (schema)})
                                   (leva/controls {:atom !x :schema {:theta {:min -180 :max 180 :step 0.5 :label "θ of X °"}
                                                                     :play {:label "run X"}}}))
                   (formula)
                   '[xr]]]
                 [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start" :margin-top "0.6em"}}
                  [:div {:style {:flex "1 1 360px" :min-width "300px"}} (mafs-panel !s 'x2d)]
                  [:div {:style {:flex "0 1 300px" :min-width "260px" :font-size "0.9em"}}
                   [:p "Above: the cone, the plane, the section (red), the drop lines (violet) and the shadow (blue) on the plane through the apex, with its polar grid about the axis. Left: the shadow alone, about the foot of the axis F, with the directrix. X runs round the section; X′ is straight below it, at the same θ in both views."]
                   [:blockquote {:style {:font-style "italic" :opacity 0.85 :margin "0.4em 0"}}
                    quote-pappus " (Heath 1896, Introduction.)"]
                   [:p {:style {:opacity 0.6}} "drag: rotate · scroll: zoom · right-drag: pan"]]]])
          (list 'finally
                (list 'remove-watch !p ::tie)
                '(some-> @raf js/cancelAnimationFrame)))))

(defn playground
  "The conics playground (polar coordinates as a projection), an
   emmy-viewers fragment for a Clerk notebook mounted after
   (history-of-math.widgets/install!). Compiled on the raster backend
   whatever the caller's binding. `start` (default (init)) is the tied
   state {:alpha :tilt :h :l :e}."
  ([] (playground (init)))
  ([start]
   (binding [vc/*backend* :raster]
     (let [s (mapv start tied)]
       (ev/with-let [!p start !s start !x {:theta 120 :play true}]
         (template-body (kernel-binding "tie" tie s)
                        (kernel-binding "readout" readout s)
                        (kernel-binding "point" point (conj s 120))
                        !p !s !x))))))

;; ---------------------------------------------------------------------------
;; The derivation, for the page

(defn derivation
  "Emmy's derivation of the tie (alexandria.apollonius.projection) as a
   Clerk column: Heath's sentence on the cone, each step's equations
   rendered by emmy.clerk/->TeX (already simplified by Emmy in the
   derivation, so ->TeX is asked not to simplify again), and the grades."
  []
  (apply clerk/col
         (concat
          [(clerk/html [:blockquote {:style {:font-style "italic" :opacity 0.85}}
                        quote-cone " (Heath 1896, The Cone.)"])]
          (for [{:keys [label eqs]} (pj/equations)]
            (apply clerk/col
                   (clerk/html [:div {:style {:font-size "0.85em" :opacity 0.7 :margin-top "0.5em"}} label])
                   (for [q eqs] (ec/->TeX q :simplify? true))))
          [(clerk/html
            (into [:ul {:style {:font-size "0.9em"}}]
                  (for [{:keys [label grade]} (pj/grades)]
                    [:li {:data-grade (name grade)} [:code (name grade)] " " label])))])))

(defn kernel-forms
  "Every (js/Function. \"fb\" glue) form in a built fragment: the compiled
   kernels it ships."
  [form]
  (let [acc (volatile! [])]
    (walk/postwalk (fn [x]
                     (when (and (seq? x) (= 'js/Function. (first x)) (= "fb" (second x)))
                       (vswap! acc conj (nth x 2)))
                     x)
                   form)
    @acc))
