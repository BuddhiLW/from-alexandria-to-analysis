(ns alexandria.apollonius.symptom-widget-test
  (:require [alexandria.apollonius.symptom :as symptom]
            [alexandria.apollonius.symptom-scene :as scene]
            [alexandria.apollonius.symptom-widget :as widget]
            [alexandria.medium.figure :as figure]
            [alexandria.medium.kernel-oracle :as ko]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(def ^:private fig (figure/->FnFigure symptom/symptom-figure))

(defn- solve
  "The kernel's Emmy oracle, as the scene calls the wasm kernel (doubles,
   as in the browser: an exact 0 would make Emmy divide exactly)."
  [deg yin x]
  (zipmap symptom/outputs (first (figure/points fig [0.6 0.7 2.2 (double deg) (double yin)] [[(double x)]]))))

(def ^:private par-deg
  "A hair off the parabola's tilt: at exactly it d = PP' divides by zero
   (Emmy throws on the JVM; the browser's slider, step 0.1, never lands on it)."
  (+ 1e-9 (* (/ 180 Math/PI) (Math/atan2 1 0.6))))

(defn- on-curve? [{:keys [tilt x y]}]
  (let [{:keys [p c]} (solve tilt 0 x)]
    (< (Math/abs (- (* y y) (+ (* p x) (* c x x)))) 1e-9)))

(deftest the-tie
  (let [s0 (scene/settle solve 35 0.85 :init)]
    (testing "every state lies on the section"
      (is (on-curve? s0)))
    (testing "x moves: tilt held, y follows"
      (let [s (scene/tie solve s0 :x 0.4)]
        (is (= [35 0.4 :x] [(:tilt s) (:x s) (:origin s)]))
        (is (on-curve? s))))
    (testing "y moves: tilt held, x solved on V's half of the ellipse"
      (let [near (scene/tie solve s0 :y 0.5)
            far (scene/tie solve (assoc s0 :x 1.3) :y 0.5)]
        (is (= [35 0.5 :y] [(:tilt near) (:y near) (:origin near)]))
        (is (< (:x near) (:half (solve 35 0 0.5)) (:x far)))
        (is (on-curve? near))
        (is (on-curve? far))))
    (testing "a y above the ellipse's greatest ordinate puts V at the centre and is pushed back"
      (let [s (scene/tie solve s0 :y 1.5)
            {:keys [b half]} (solve 35 0 0.5)]
        (is (= :pushed (:origin s)))
        (is (< (Math/abs (- (:x s) half)) 1e-9))
        (is (< (Math/abs (- (:y s) b)) 1e-6))))
    (testing "tilt moves: x held, y follows, through all three sections"
      (doseq [[deg kind] [[25 :ellipse] [35 :ellipse] [par-deg :parabola] [80 :hyperbola]]]
        (let [s (scene/tie solve s0 :tilt deg)]
          (is (= 0.85 (:x s)))
          (is (= kind (scene/kind (:c (solve deg 0 0.85)))))
          (is (on-curve? s)))))
    (testing "on the hyperbola and the parabola y solves to the near root"
      (doseq [deg [par-deg 70 80]]
        (let [s (scene/tie solve (scene/settle solve deg 0.85 :init) :y 0.6)]
          (is (on-curve? s) deg)
          (is (< (Math/abs (- 0.6 (:y s))) 1e-9)))))
    (testing "a pushed or unchanged value is no source"
      (is (identical? s0 (scene/tie solve s0 :x (+ (:x s0) 1e-12)))))
    (testing "x is clamped to the reach"
      (let [s (scene/tie solve s0 :x 9)]
        (is (<= (:x s) (scene/reach (solve 35 0 0.85))))
        (is (on-curve? s))))))

(deftest the-live-tex
  (let [t (widget/template)]
    (is (= "{\\mathsf{YYY}}^{2} = \\mathsf{PPP}\\,\\mathsf{XXX} + \\mathsf{CCC}\\,{\\mathsf{XXX}}^{2}" t))
    (is (= "{0.657}^{2} = 0.977\\,0.850 + (-0.553)\\,{0.850}^{2}"
           (scene/fill t {"YYY" (scene/num-tex 0.6568) "PPP" (scene/num-tex 0.97717)
                          "XXX" (scene/num-tex 0.85) "CCC" (scene/num-tex -0.5526)})))))

(deftest the-names
  (is (= #{"παραβολή" "ὑπερβολή" "ἔλλειψις"} (set (map :greek (vals scene/names)))))
  (is (= #{"parabole" "hyperbole" "elleipsis"} (set (map :latin (vals scene/names))))))

(def ^:private oracle-cases
  {:symptom [[[0.6 0.7 2.2 35 0.5] [[0.1] [0.85] [1.4]]]
             [[0.6 0.7 2.2 80 0.6] [[0.1] [0.85] [1.4]]]
             [[0.6 0.7 2.2 50 0.3] [[0.3] [1.0]]]]
   :curve [[[0.6 0.7 35 1.7] [[0] [0.3] [0.7] [1]]]
           [[0.6 0.7 80 1.5] [[0] [0.5] [1]]]]})

(deftest every-kernel-matches-its-emmy-oracle
  (is (= (set (keys symptom/figures)) (set (keys oracle-cases))))
  (doseq [[id cases] oracle-cases
          [params states] cases]
    (when-let [{:keys [wasm max-error nan-states]} (ko/deviation (symptom/figures id) params states)]
      (is wasm (str id))
      (is (empty? nan-states) (str id " " nan-states))
      (is (< max-error 1e-4) (str id " " max-error)))))

(deftest the-clerk-value
  (let [{:keys [kernels data]} (widget/value)]
    (is (= #{:cone :section :plane :segment :named :symptom :curve} (set (keys kernels))))
    (is (every? (comp string? :glue) (vals kernels)))
    (is (= symptom/outputs (:outputs data)))
    (is (str/includes? (:template data) "YYY"))
    (is (= :raster (:source data)))))
