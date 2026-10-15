(ns alexandria.apollonius.book-one-test
  (:require [alexandria.apollonius.book-one :as book]
            [alexandria.apollonius.book-one-scene :as scene]
            [alexandria.apollonius.book-one-view :as view]
            [alexandria.apollonius.symptom :as symptom]
            [alexandria.apollonius.symptom-scene :as sym]
            [alexandria.medium.figure :as figure]
            [alexandria.medium.kernel-oracle :as ko]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(defn- close? ([a b] (close? a b 1e-9)) ([a b tol] (< (Math/abs (- (double a) (double b))) tol)))

(def ^:private parabola (double (book/parabola-tilt)))

(defn- heath-at [deg & [x x2]]
  (scene/named (first (figure/points (figure/->FnFigure book/figure)
                                     [0.6 (double deg) 0.7 2.2 1.6 (or x 0.9) (or x2 0.9) 0.42 0.22] [[0]]))))

(deftest the-walk-is-heaths
  (let [{:keys [props]} (book/walk)]
    (is (= ["I.11" "I.12" "I.13" "I.14"] (mapv :id props)))
    (is (= [:parabola :hyperbola :ellipse :opposite] (mapv :case props)))
    (testing "each step's sentence introduces something, nothing twice within a proposition"
      (doseq [{:keys [id steps]} props]
        (is (every? (comp seq :introduces) steps) id)
        (is (apply distinct? (mapcat :introduces steps)) id)
        (is (every? #(or (nil? (:quote %)) (:cite %)) steps) id)))
    (testing "Heiberg's naming sentence for each"
      (is (= ["παραβολή" "ὑπερβολή" "ἔλλειψις" "ἀντικείμεναι"]
             (mapv #(last (str/split (:greek %) #" ")) props))))))

(def ^:private heath-text
  (io/file (System/getProperty "user.home") "Documents/feed/math-history/03-apollonius-conics/text/heath-1896-conics.txt"))

(defn- words
  "The words of s, lower case, Greek look-alike capitals of the scan read as
   Latin, letters only."
  [s]
  (-> s
      (str/escape {\Α \a \Β \b \Ε \e \Η \h \Ι \i \Κ \k \Μ \m \Ν \n \Ο \o \Ρ \p \Τ \t \Χ \x \ο \o \ν \v})
      str/lower-case (str/replace #"[^a-z]+" " ") str/trim (str/split #" +")))

(def ^:private ocr-damage
  "Spans of the Heath scan the OCR garbled, read back as printed."
  {"BG" "BC" "A F" "AF" "nieetiiuj" "meeting" "Ρ Μ" "PM" "draiun p)erpendicidar" "drawn perpendicular"
   "luill he" "will be" "tJie" "the" "Hght" "right"})

(deftest the-quotes-are-heaths-words
  ;; the scan is OCR: a quote is Heath's when most of its word triples
  ;; occur in the text, once the known damage is read back
  (when (.exists heath-text)
    (let [shingles (fn [ws] (set (map vec (partition 3 1 ws))))
          text (shingles (words (reduce-kv str/replace (slurp heath-text) ocr-damage)))]
      (doseq [{:keys [steps]} (:props (book/walk))
              {:keys [id quote]} steps :when quote
              :let [qs (shingles (words quote))
                    hit (count (filter text qs))]]
        (is (>= (/ hit (max 1 (count qs))) 0.6) (str id ": " hit "/" (count qs)))))))

(deftest every-step-of-heath-is-proved-by-emmy
  (let [g (book/graded)]
    (is (= #{:I.11 :I.12-13 :I.14} (set (keys g))))
    (is (= [7 8 3] (mapv (comp count g) [:I.11 :I.12-13 :I.14])))
    (doseq [[k rows] g {:keys [label grade]} rows]
      (is (= :grade/proved grade) (str k " " label)))))

(deftest the-kernel-agrees-with-heath
  (is (= (:points scene/outputs) book/points))
  (is (= (:scalars scene/outputs) book/scalars))
  (testing "I.11, PM parallel to AC: QV² = PL·PV = HV·VK, PL : PA = BC² : BA·AC, VR = PL"
    (let [{:keys [qv2 plpv hvvk rat-pa rat-bc overlap c]} (heath-at parabola)]
      (is (close? qv2 plpv 1e-12)) (is (close? qv2 hvvk 1e-12))
      (is (close? rat-pa rat-bc 1e-12)) (is (close? c 0 1e-12)) (is (close? overlap 0 1e-12))))
  (doseq [[deg kind] [[30 :ellipse] [35 :ellipse] [50 :ellipse] [65 :hyperbola] [70 :hyperbola] [78 :hyperbola]]
          x [0.2 0.9 1.4]]
    (testing (str "tilt " deg ", x " x)
      (let [{:keys [qv2 rect hvvk rat-bf rat-pp overlap p]} (heath-at deg x)]
        (is (close? qv2 rect 1e-12) "QV² = PV·VR")
        (is (close? qv2 hvvk 1e-12) "the circle HQK")
        (is (close? rat-bf rat-pp 1e-12) "PL : PP' = BF·FC : AF²")
        (is (= kind (scene/kind-of overlap p)))))))

(deftest i-14-the-second-branch
  (doseq [deg [65 72 78] x2 [0.4 0.9]]
    (let [{:keys [qv2' rect2 w2 p]} (heath-at deg 0.9 x2)]
      (is (close? qv2' rect2 1e-12) (str deg " " x2))
      (is (pos? qv2'))
      (is (pos? p)))))

(deftest the-tie-reuses-the-symptoma
  (let [solve (fn [deg yin x] (symptom/numbers {:k 0.6 :h 0.7 :zb 2.2 :deg deg :x x :yin yin}))
        prop (view/prop-of "I.12")]
    (testing "I.11 holds the parabola's tilt; I.12-14 stay in their case"
      (is (close? parabola (scene/case-tilt (view/prop-of "I.11") parabola 30)))
      (is (= 62 (scene/case-tilt prop parabola 10)))
      (is (= 56 (scene/case-tilt (view/prop-of "I.13") parabola 70))))
    (testing "x -> y -> x round-trips (y moved down: the ellipse's QV has a maximum), and the widget's numbers match the figure's"
      (doseq [[deg x] [[35 0.4] [35 0.9] [70 0.5] [70 1.3] [parabola 0.8]]]
        (let [s (sym/settle solve deg x :x)
              s' (sym/tie solve s :y (- (:y s) 0.05))
              s'' (sym/tie solve s' :y (:y s))]
          (is (close? (:x s'') x 1e-6) (str deg " " x))
          (is (close? (* (:y s) (:y s)) (:qv2 (heath-at deg x)) 1e-9)))))))

(deftest the-transport-and-steps
  (let [steps (:steps (view/prop-of "I.12"))
        n (count steps)]
    (is (= 6 n))
    (let [v (scene/progress-of steps {:step 2 :progress 1})]
      (is (== 1 (v :PL)) (is (== 1 (v :VR))))
      (is (zero? (v :circle))))
    (is (zero? (:PL (scene/progress-of steps {:step 0 :progress 1}))))
    (is (= [2 0 true] ((juxt :step :progress :playing?) (scene/goto {} 2 n))))
    (is (= 5 (:step (scene/goto {} 99 n))))
    (is (= 1 (:step (scene/toggle (scene/advance (scene/goto {} 0 n) 1e9 n) n))))))

(def ^:private oracle-cases
  ;; not at 59.04: near the parabola PP' is ~1e3 and the f32 kernel's
  ;; absolute error scales with it
  {:heath [[[0.6 35 0.7 2.2 1.6 0.9 0.9 0.42 0.22] [[0]]] [[0.6 70 0.7 2.2 1.6 0.5 0.4 0.42 0.22] [[0]]]
           [[0.6 52 0.7 2.2 1.6 1.2 0.9 0.42 0.22] [[0]]]]
   :section [[[0.6 35 0.7 0.42 0.22] [[0] [1] [2.5]]] [[0.6 70 0.7 0.42 0.22] [[0.3] [3]]]]
   :circle [[[0.6 2.2 0.42 0.22] [[0] [1] [4]]]]})

(deftest every-kernel-matches-its-emmy-oracle
  ;; f32 in the wasm kernel: lengths up to ~5 (PP', the far base) carry
  ;; ~2e-4 absolute error
  (is (= (set (keys book/figures)) (set (keys oracle-cases))))
  (doseq [[id cases] oracle-cases [params states] cases]
    (when-let [{:keys [wasm max-error nan-states]} (ko/deviation (book/figures id) params states)]
      (is wasm (str id))
      (is (empty? nan-states) (str id " " nan-states))
      (is (< max-error 5e-4) (str id " " max-error)))))

(deftest the-clerk-value
  (let [{:keys [prop kernels data]} (view/value "I.14")]
    (is (= "I.14" (:id prop)))
    (is (= #{:heath :section :circle :symptom} (set (keys kernels))))
    (is (every? (comp string? :glue) (vals kernels)))
    (is (= :raster (:source data)))
    (is (close? parabola (:parabola data) 1e-6)))
  (is (some? (view/steps :I.14))))
