(ns alexandria.apollonius.areas-widget-test
  "The application-of-areas widget: its numbers come from raster kernels and
   satisfy Apollonius' symptoma; its fragment carries the raster kernel glue."
  (:require [alexandria.apollonius.areas-widget :as areas]
            [alexandria.apollonius.conics :as conics]
            [alexandria.raster :as raster]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [emmy.viewer.compile :as vc]))

(defn- close? [a b] (< (Math/abs (- (double a) (double b))) 1e-9))

(deftest symptoma-to-scale
  (doseq [[e kind] [[0.6 :ellipse] [1.0 :parabola] [1.4 :hyperbola] [0.0 :ellipse]]]
    (testing (str "e = " e)
      (let [{:keys [x y y2 px cx2 px+cx2 p c vr] :as n} (areas/numbers {:l 1.2 :e e :x 0.8})]
        (is (close? p 2.4) "PL = p = 2 l")
        (is (close? c (- (* e e) 1)) "c = e^2 - 1")
        (is (close? vr (+ p (* c x))) "VR = p + c x")
        (is (close? y2 (* y y)) "the square on QV")
        (is (close? y2 px+cx2) "QV^2 = PV.PL + c PV^2 (I.11-13)")
        (is (close? px+cx2 (+ px cx2)))
        (is (close? (:x+y n) (+ x y)))
        (is (close? (:-p n) (- p)))
        (is (close? (:-vr n) (- vr)))
        (is (= kind (areas/kind c)) "the name follows the sign of c")))))

(deftest transverse-and-names
  (let [{:keys [p c P'x x-max]} (areas/numbers {:l 1 :e 0.5 :x 0.3})]
    (is (pos? P'x) "the ellipse's P' lies beyond V on PM")
    (is (close? P'x (/ (- p) c)))
    (is (close? x-max (* 0.98 P'x))))
  (is (neg? (:P'x (areas/numbers {:l 1 :e 1.5 :x 0.3}))) "the hyperbola's P' lies behind P")
  (is (= :parabola (areas/kind (:c (areas/numbers {:l 1 :e 1 :x 2}))))))

(deftest agrees-with-the-cone
  (testing "the cone's p and c (conics/parameter, conics/excess) give the same
            areas through figure: the widget's symptoma is the shelf's"
    (let [k 0.6 theta 0.7 h 0.7
          [p c] (raster/value (fn [k th h] [(conics/parameter k th h) (conics/excess k th)]) k theta h)
          ;; l and e with 2 l = p and e^2 - 1 = c
          l (/ p 2) ecc (Math/sqrt (+ 1 c))
          n (areas/numbers {:l l :e ecc :x 0.4})
          [y2] (raster/value (fn [p c x] ((conics/symptoma-figure p c) [x])) p c 0.4)]
      (is (close? (:y2 n) y2)))))

(deftest the-section-is-the-symptoma
  (doseq [e [0.0 0.6 1.0 1.4] u [0.3 1.0 4.0]]
    (let [l 1.1 p (* 2 l) c (- (* e e) 1)
          [x y] (raster/value (fn [l e u] ((areas/section l e) u)) l e u)]
      (is (close? (* y y) (+ (* p x) (* c x x))) (str "near piece, e = " e " u = " u))
      (when-not (= e 1.0)
        (let [[x' y'] (raster/value (fn [l e u] ((areas/reflected l e) u)) l e u)]
          (is (close? (* y' y') (+ (* p x') (* c x' x'))) (str "reflected piece, e = " e)))))))

(deftest compiles-on-raster
  (let [form (pr-str (areas/areas))]
    (is (str/includes? form "WebAssembly") "the kernel ships as a wasm module")
    (is (str/includes? form "emmy.viewer.kernel/bind-1d") "the panel calls the kernel")
    (is (str/includes? form "leva.core/Controls"))
    (is (str/includes? form "mafs.core/MovablePoint"))
    (is (= 3 (count (re-seq #"typeof WebAssembly" form)))
        "three kernels (the areas, the two pieces of the curve), all raster"))
  (testing "panel binds :raster itself, whatever the root backend"
    (binding [vc/*backend* :js]
      (is (str/includes? (pr-str (areas/areas {:l 2 :e 1.2})) "WebAssembly")))))
