(ns alexandria.apollonius.cone-widget-test
  (:require [alexandria.apollonius.cone-widget :as w]
            [alexandria.apollonius.conics :as conics]
            [alexandria.apollonius.conics-view :as conics-view]
            [alexandria.medium.figure :as figure]
            [alexandria.medium.kernel-oracle :as ko]
            [clojure.test :refer [deftest is testing]]
            [emmy.viewer.compile :as vc]))

(def ^:private t-par (w/parabola-tilt-deg))

(def ^:private h (:h w/defaults))

(defn- fig
  "A widget function of the sliders as a figure {:f :params :state :opts}
   for alexandria.medium.kernel (state always a vector)."
  [f params state & {:keys [scalar?]}]
  {:f (if scalar?
        (fn [& ps] (let [g (apply f ps)] (fn [[t]] (g t))))
        f)
   :params params :state state :opts {:simplify? false}})

(defn- jvm [f params states & opts]
  (figure/points (figure/->FnFigure (:f (apply fig f params (first states) opts))) params states))

(deftest the-widget-compiles-every-kernel-on-raster
  (testing "built under a :js binding, the widget still ships raster kernels only"
    (let [frag (binding [vc/*backend* :js] (w/cone))
          ks (w/kernel-forms frag)]
      (is (= 15 (count ks)) "cone, two bases, plane, PM, DE, D'E', PL, AF, two branches, HK circle, HK, QQ', letters")
      (is (every? #(re-find #"WebAssembly" %) ks) "each glue loads a wasm module (no :js-only kernel)"))))

(deftest the-defaults
  (is (< h (:h conics-view/view)) "P sits lower on the cone than the proof player's")
  (is (< (Math/abs (- t-par 59.036)) 1e-3) "the first tilt is I.11's, PM parallel to AC")
  (is (= {"A" [0.0 0.0 0.0] "B" [1.44 0.0 2.4] "C" [-1.44 0.0 2.4] "B'" [-1.44 0.0 -2.4] "C'" [1.44 0.0 -2.4]}
         (update-vals (w/fixed-letters) #(mapv (fn [x] (/ (Math/round (* 1e6 x)) 1e6)) %)))))

(def ^:private tilts
  "An ellipse closing above the base, one cut by the base, the parabola, a
   hyperbola whose opposite branch is beyond B'C', and one inside it."
  [10.0 52.0 t-par 65.0 80.0])

(def ^:private ts (mapv vector [-1.0 -0.6 -0.13 0.0 0.4 0.9 1.0]))

(deftest every-curve-kernel-matches-its-emmy-oracle
  (testing "each kernel run as wasm in node against the same Emmy function on the JVM"
    (doseq [tilt tilts
            [id f states] [[:plane w/plane-surface [[0 -1] [0.3 0.5] [1 1]]]
                           [:diameter w/diameter [[0.0] [0.5] [1.0]]]
                           [:near w/section-near ts]
                           [:de w/trace-de (if (> tilt 30) ts [])]
                           [:pl w/parameter-pl [[0.0] [1.0]]]
                           [:af w/line-af (if (> tilt 56) [[0.5] [1.0]] [])]
                           [:opposite w/section-opposite (if (= tilt 80.0) ts [])]]
            :when (seq states)]
      (let [{:keys [wasm max-error] :as dev} (ko/deviation (fig f [tilt h] (first states) :scalar? (not= id :plane))
                                                           [tilt h] states)]
        (when dev
          (is wasm (str id " " tilt))
          ;; raster's atan2 (~3e-5) sets the arc's end angle; on a steep
          ;; hyperbola dZ/dphi near the base is large, which carries the
          ;; error to ~1.3e-4 there (80 deg)
          (is (< max-error 3e-4) (str id " at " tilt ": " max-error)))))
    (doseq [tilt tilts
            [id f] [[:circle w/circle-hk] [:hk w/chord-hk] [:qq w/chord-qq]]]
      (when-let [{:keys [wasm max-error]} (ko/deviation (fig f [tilt h 0.55] [0.0] :scalar? true)
                                                        [tilt h 0.55] [[0.0] [0.7] [-1.0] [1.0]])]
        (is wasm (str id " " tilt))
        (is (< max-error 1e-4) (str id " at " tilt ": " max-error))))))

(defn- on-cone? [[x y z]] (< (Math/abs (- (+ (* x x) (* y y)) (* w/k w/k z z))) 1e-9))

(defn- on-plane?
  "On the cutting plane through P = (k h, 0, h) with normal (sin, 0, cos)."
  [tilt [x _ z]]
  (let [th (Math/toRadians tilt)]
    (< (Math/abs (+ (* (Math/sin th) (- x (* w/k h))) (* (Math/cos th) (- z h)))) 1e-9)))

(deftest the-section-is-the-plane-cone-intersection-and-ends-on-the-base
  (doseq [tilt tilts]
    (let [pts (jvm w/section-near [tilt h] ts :scalar? true)
          [_ _ z0] (first pts) [_ _ z1] (last pts)]
      (is (every? on-cone? pts) (str tilt))
      (is (every? (partial on-plane? tilt) pts) (str tilt))
      (if (< tilt 30)
        (is (< (Math/abs (- (first (first pts)) (first (last pts)))) 1e-9) (str "the ellipse closes at P' (" tilt ")"))
        (do (is (< (Math/abs (- z0 w/zb)) 1e-9) (str "D on the base at " tilt))
            (is (< (Math/abs (- z1 w/zb)) 1e-9) (str "E on the base at " tilt))))))
  (testing "I.14: the opposite branch lies on the opposite cone and ends on B'C'"
    (let [pts (jvm w/section-opposite [80.0 h] ts :scalar? true)]
      (is (every? on-cone? pts))
      (is (every? (partial on-plane? 80.0) pts))
      (is (every? (fn [[_ _ z]] (neg? z)) pts))
      (is (< (Math/abs (+ (nth (first pts) 2) w/zb)) 1e-9)))))

(defn- run-letters [tilt v]
  (let [{:keys [points]} (ko/run-kernel (fig w/letters [tilt h v] [0]) [tilt h v] [[0]])]
    (first points)))

(deftest the-letters-carry-apollonius-construction
  (when @ko/node?
    (doseq [tilt tilts v [0.2 0.55 0.9]]
      (let [out (run-letters tilt v)
            at (fn [l] (subvec (vec out) (w/letter-index l) (+ 3 (w/letter-index l))))
            n (fn [key] (nth out (w/number-index key)))
            [px _ pz] (at "P") [hx _ hz] (at "H") [kx _ kz] (at "K") [vx _ vz] (at "V")]
        (testing (str "tilt " tilt " v " v)
          (is (every? some? (concat (at "P") (at "V") (at "Q") (at "H") (at "K") (at "L"))) "the I.11 letters exist")
          (is (and (pos? hx) (neg? kx) (pos? px)) "H on AB (the side of P), K on AC")
          (is (< (Math/abs (- hz vz)) 1e-9) "HK through V parallel to the base")
          (is (< (Math/abs (- kz vz)) 1e-9))
          (is (< (Math/abs (- (n :qv2) (n :hv-vk))) 1e-4) "QV^2 = HV.VK (the circle HQK)")
          (is (< (Math/abs (- (n :qv2) (n :pv-vr))) 1e-4) "QV^2 = PV.VR (I.11-13)")
          (is (< (Math/abs (- (n :c) (conics/excess w/k (Math/toRadians tilt)))) 1e-5))
          (is (< (Math/abs (- vx (- (* w/k h) (* (n :pv) (Math/cos (Math/toRadians tilt)))))) 1e-4)
              "V on PM at abscissa PV")
          (is (< (Math/abs (- pz h)) 1e-12)))))
    (testing "letters that do not exist are NaN, so the page does not draw them"
      (let [out (run-letters 10.0 0.5) at (fn [l] (subvec (vec out) (w/letter-index l) (+ 3 (w/letter-index l))))]
        (is (every? #(some nil? (at %)) ["M" "D" "E" "M'" "D'" "E'"]) "an ellipse above the base: no DME, no D'M'E'")
        (is (every? some? (at "P'")) "P' on AC"))
      (let [out (run-letters t-par 0.5) at (fn [l] (subvec (vec out) (w/letter-index l) (+ 3 (w/letter-index l))))]
        (is (nil? (first (at "P'"))) "the parabola has no P'")
        (is (every? some? (concat (at "D") (at "E") (at "M"))) "I.11: DME on the base"))
      (let [out (run-letters 80.0 0.5) at (fn [l] (subvec (vec out) (w/letter-index l) (+ 3 (w/letter-index l))))]
        (is (every? some? (concat (at "P'") (at "D'") (at "E'") (at "M'"))) "I.14: P' and D'M'E' on the opposite cone")
        (is (pos? (nth out (w/number-index :opposite))))
        (is (neg? (nth out (w/number-index :pp'))) "PP' crosses the apex")))))

(deftest the-opposite-branch-is-masked-where-it-does-not-exist
  (when @ko/node?
    (doseq [tilt [10.0 52.0 t-par 65.0]]
      (let [{:keys [points]} (ko/run-kernel (fig w/section-opposite [tilt h] [0.0] :scalar? true) [tilt h] ts)]
        (is (every? (fn [p] (some nil? p)) points) (str "no opposite branch drawn at " tilt))))))
