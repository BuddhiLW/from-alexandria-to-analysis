(ns alexandria.apollonius.areas-steps-test
  (:require [alexandria.apollonius.areas-steps :as as]
            [alexandria.apollonius.areas-steps-view :as view]
            [alexandria.medium.figure :as figure]
            [alexandria.medium.kernel-oracle :as ko]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(def data (as/data))

(defn- close? ([a b] (close? a b 1e-9)) ([a b tol] (< (Math/abs (- (double a) (double b))) tol)))

(deftest ten-sentences-each-drawing-what-it-introduces
  (let [{:keys [steps]} (as/passage)
        drawn (mapv #(set (remove as/motions (keys %))) as/schedule)]
    (is (= (mapv #(str "A" %) (range 1 11)) (mapv :id steps)))
    (is (= as/step-count (count steps) (count as/durations)))
    (doseq [[i {:keys [id introduces]}] (map-indexed vector steps)]
      (is (= (set introduces) (drawn i)) id))
    (testing "nothing is introduced twice; everything that leaves was introduced earlier"
      (is (apply distinct? (mapcat :introduces steps)))
      (doseq [[key [j]] as/leaves
              :let [i (first (keep-indexed (fn [i s] (when (contains? s key) i)) as/schedule))]]
        (is (and i (< i j)) (str key))))
    (testing "at the end of step i every element of steps <= i is complete, none later has begun"
      (doseq [i (range as/step-count)
              :let [v (as/progress-of {:step i :progress 1})]]
        (doseq [j (range as/step-count) key (keys (as/schedule j))]
          (is (== (if (<= j i) 1 0) (v key)) (str "step " (inc i) " " key)))))
    (testing "each step's motion keeps the parts of the step inside [0 1]"
      (doseq [sched as/schedule [key [a b]] sched]
        (is (<= 0 a b 1) (str key))))))

(def ^:private heath
  "Heath's texts in the feed, when present."
  {:euclid-1 (io/file (System/getProperty "user.home") "Documents/feed/math-history/01-euclid-elements/text/heath-1908-v1.txt")
   :euclid-2 (io/file (System/getProperty "user.home") "Documents/feed/math-history/01-euclid-elements/text/heath-1908-v2.txt")
   :apollonius (io/file (System/getProperty "user.home") "Documents/feed/math-history/03-apollonius-conics/text/heath-1896-conics.txt")})

(defn- squash
  "Text for comparing a quote against an OCR'd page: letters only, lower
   case (the OCR drops and doubles spaces, misreads ² and quotes)."
  [s]
  (-> s str/lower-case (str/replace #"[^a-z]" "")))

(deftest the-quotes-are-heaths-words
  (let [{:keys [steps]} (as/passage)
        quoted (filter :quote steps)]
    (is (= #{"A1" "A2" "A5" "A6" "A7" "A9" "A10"} (set (map :id quoted))))
    (is (every? :cite quoted))
    (when (every? #(.exists ^java.io.File %) (vals heath))
      (let [texts (update-vals heath (comp squash slurp))
            ;; spans the scans garble, cut out of the comparison: the
            ;; OCR reads "apply, in" with a stray mark, y² as "y-", "p and d"
            ;; as "l)y ;j and d", "[i.e. Apollonius]" as "fi-e. Apollonius]"
            damaged {"A1" [", in"] "A7" ["y² is equal" "breadth x and"] "A9" ["by p and d"]
                     "A10" ["[i.e. Apollonius]"]}]
        (doseq [{:keys [id quote cite]} quoted
                :let [src (cond (str/includes? cite "1896") :apollonius
                                (str/includes? cite "vol. 1") :euclid-1
                                :else :euclid-2)
                      pieces (reduce (fn [ps cut] (mapcat #(str/split % (re-pattern (java.util.regex.Pattern/quote cut))) ps))
                                     [quote] (damaged id))]]
          (doseq [piece pieces :let [q (squash piece)] :when (seq q)]
            (is (str/includes? (texts src) q) (str id " is in " (name src) ": " piece))))))))

(deftest the-numbers-come-from-raster
  (let [{:keys [source r1 r2 y29 top h44 half residuals y2 a S sq5]} data]
    (is (= :raster source))
    (is (close? r1 3 1e-9) "VI.28 at a = 10, S = 21: y = 3")
    (is (close? r2 7 1e-9) "and y = 7")
    (is (close? top 25) "the top (a/2)^2 = 25 at y = a/2 = 5 (VI.27)")
    (is (close? half 5))
    (is (close? h44 2.1) "I.44's height S/a")
    (is (close? sq5 4 1e-9) "II.5 at y = 3: the small square (5 - 3)^2 = 4, and 21 + 4 = 25")
    (is (close? (* (+ a y29) y29) S 1e-9) "VI.29's root")
    (is (every? #(< (Math/abs (double %)) 1e-9) residuals))
    (is (= {:parabola 14.0 :ellipse 12.0 :hyperbola 16.0} y2) "p = 7, d = 14, x = 2: 14 -/+ 2")))

(deftest every-identity-is-proved-by-emmy
  (let [ids (as/identities)]
    (is (= #{:I.44 :VI.28 :VI.28-roots :VI.28-roots' :tie :II.5 :VI.29 :VI.29-root :ellipse :hyperbola :d-by-p}
           (set (map :id ids))))
    (doseq [{:keys [id grade]} ids]
      (is (= :grade/proved grade) (str id)))))

(defn- py [a S y]
  (as/named-map as/py-points as/py-scalars
                (first (figure/points (figure/->FnFigure as/py-figure) [a S y] [[0]]))))

(defn- cn [p d x k]
  (as/named-map as/cn-points as/cn-scalars
                (first (figure/points (figure/->FnFigure as/cn-figure) [p d x k] [[0]]))))

(defn- corners [f params]
  (figure/points (figure/->FnFigure f) params [[0 0] [1 0] [1 1] [0 1]]))

(defn- area [pts]
  (let [xs (map first pts) ys (map second pts)]
    (* (- (apply max xs) (apply min xs)) (- (apply max ys) (apply min ys)))))

(deftest euclids-figure
  (testing "y = 3 and y = 7 both apply S = 21 to a = 10; XB = y; the top is 25 at 5"
    (doseq [y [3 7]
            :let [{:keys [X B Xt amyy r1 r2 ay yy]} (py 10 21 y)]]
      (is (close? amyy 21))
      (is (close? (+ amyy yy) ay) "S + y^2 = a y: the strip")
      (is (close? (- (first B) (first X)) y) "XB = y")
      (is (close? (- (second Xt)) y) "the square's height is y")
      (is (close? r1 3) (str r1))
      (is (close? r2 7)))
    (is (close? (:amyy (py 10 21 5)) 25))
    (is (every? #(< (:amyy (py 10 21 %)) 25) [0.5 2 4.9 5.1 8])))
  (testing "VI.29: X' lies y29 past B"
    (let [{:keys [X' B y29 apyy29]} (py 10 21 3)]
      (is (close? (- (first X') (first B)) y29))
      (is (close? apyy29 21)))))

(deftest euclid-II-5-drawn
  (doseq [y [1.5 3 4.2 5 7 8.5]
          :let [{:keys [amyy sq5 half ym amym top]} (py 10 21 y)
                moved (corners as/moved-figure [10 ym 1])
                start (corners as/moved-figure [10 ym 0])
                mx (corners as/box-figure [half amym 0 ym 1])
                small (corners as/box-figure [half amym ym half 1])
                whole (corners as/box-figure [half 10 0 half 1])]]
    (testing (str "y = " y)
      (is (close? ym (min y (- 10 y))) "the shorter piece")
      (is (close? (area start) (* half ym)) "the part on AM")
      (is (close? (area moved) (area start)) "moved, it keeps its area")
      (is (close? (+ (area mx) (area start)) amyy) "the two parts are the applied rectangle")
      (is (close? (area small) sq5) "the small square on a/2 - y")
      (is (close? (area whole) top) "the square on the half")
      (is (close? (+ (area mx) (area moved) (area small)) (area whole))
          "II.5: rectangle and small square fill the square on the half")
      (testing "the pieces tile it: the moved column stands at the far end, over the small square's side"
        (is (close? (ffirst moved) (- 10 ym)))
        (is (close? (- (second (nth moved 2))) half))))))

(deftest the-conic
  (let [{:keys [p d x]} data]
    (testing "k = 0: applied exactly, y^2 = p x"
      (let [{:keys [yy px E w]} (cn p d x 0)]
        (is (close? yy px)) (is (close? E p)) (is (close? w 1))))
    (testing "k = -1: falls short by the d x p rectangle; k = 1 exceeds by it"
      (let [{:keys [yy px wx E]} (cn p d x -1)]
        (is (close? yy (- px wx))) (is (close? E 6)))
      (let [{:keys [yy px wx E]} (cn p d x 1)]
        (is (close? yy (+ px wx))) (is (close? E 8))))
    (testing "p stands upright at P, the rectangle x wide on it"
      (let [{:keys [P0 Pe Pt0]} (cn p d x 0)]
        (is (close? (first P0) (first Pe))) (is (close? (- (second Pe)) p))
        (is (close? (first Pt0) x))))
    (testing "Q lies on the section and QV = y"
      (let [{:keys [Q V y]} (cn p d x 1)]
        (is (close? (- (second V) (second Q)) y))))))

(deftest the-scene-state
  (testing "A5 sweeps y from the first root up to the top, on to the second, back to the first"
    (let [y-at (fn [q] (as/sweep-y q data))]
      (is (close? 3 (y-at 0)))
      (is (close? 5 (y-at (/ 1 3))))
      (is (close? 7 (y-at (/ 2 3))))
      (is (close? 3 (y-at 1))))
    (is (close? 3 (:y (as/scene-state {:step 4 :progress 0.181} {:y 4 :S 21} data)) 1e-3))
    (is (= 4 (:y (as/scene-state {:step 4 :progress 0.9} {:y 4 :S 21} data))) "after the sweep, the slider's")
    (is (= 4 (:y (as/scene-state {:step 3 :progress 0.5} {:y 4 :S 21} data)))))
  (testing "k: 0 (parabola) through A8, -1 at the end of A9, 1 at the end of A10"
    (is (== 0 (:k (as/scene-state {:step 7 :progress 1} {:y 3 :S 21} data))))
    (is (== -1 (:k (as/scene-state {:step 8 :progress 1} {:y 3 :S 21} data))))
    (is (== 1 (:k (as/scene-state {:step 9 :progress 1} {:y 3 :S 21} data)))))
  (testing "the graph turns from (a - y) y to (a + y) y over A6"
    (is (== -1 (:s (as/scene-state {:step 4 :progress 1} {:y 3 :S 21} data))))
    (is (== 1 (:s (as/scene-state {:step 5 :progress 1} {:y 3 :S 21} data)))))
  (testing "I.44's rectangle is gone once VI.28 is drawn; Euclid's figure once the line is p"
    (let [al (as/alpha-of {:step 6 :progress 1})]
      (is (zero? (:rect44 al))) (is (zero? (:strip al))) (is (zero? (:graph al)))
      (is (zero? (:small-sq al)))
      (is (== 1 (:xstrip al))))))

(deftest the-tie-maps-are-inverse
  (testing "S from y by (a - y) y, y back from S by the root on y's side of a/2"
    (doseq [y [0.5 2 3 4.99 5 6 7 9.5]
            :let [{:keys [amyy]} (py 10 21 y)
                  {:keys [r1 r2]} (py 10 amyy y)]]
      (is (close? (if (<= y 5) r1 r2) y 1e-6) (str y))))
  (testing "above (a/2)^2 there is no real y (VI.27): the oracle refuses a complex root"
    (is (thrown? ClassCastException (:r1 (py 10 27 3))))
    (is (close? (:r1 (py 10 25 3)) 5))))

(deftest the-transport
  (let [st (as/start)]
    (is (= [0 1 false] ((juxt :step :progress :playing?) (as/advance st 1e9))))
    (is (= 1 (:step (as/toggle (as/advance st 1e9)))))
    (is (= 9 (:step (as/goto st 99))))
    (is (= [0.4 false] ((juxt :progress :playing?) (as/seek st 0.4))))))

(deftest the-sentences-parse
  (is (= [[:t "So "] [:m "AX = a - y"] [:t ", "] [:b "square"]]
         (as/segments "So $AX = a - y$, **square**")))
  (is (= [[:i "parabolē"]] (as/segments "*parabolē*"))))

(def ^:private oracle-cases
  {:py [[[10 21 3] [[0]]] [[10 25 5] [[0]]] [[10 12.5 8.4] [[0]]]]
   :cn [[[7 14 2 0] [[0]]] [[7 14 2 -1] [[0]]] [[7 14 2 0.37] [[0]]]]
   :rect [[[0 10 3 1 0.4] [[0 0] [1 0] [1 1] [0 1]]] [[10 11.78 1.78 0.3 1] [[1 1]]]]
   :box [[[5 7 3 5 1] [[0 0] [1 0] [1 1] [0 1]]] [[0 2 0 7 0.4] [[1 1]]]]
   :moved [[[10 3 0] [[0 0] [1 1]]] [[10 3 0.5] [[1 0] [0 1]]] [[10 2.2 1] [[1 1]]]]
   :graph [[[10 -1] [[0] [0.25] [0.5] [1]]] [[10 0.2] [[0.1] [0.4]]]]
   :conic [[[7 14 0] [[0] [0.5] [1]]] [[7 14 -1] [[0.2] [1]]] [[7 14 1] [[0.7]]]]})

(deftest every-kernel-matches-its-emmy-oracle
  (is (= (set (keys as/figures)) (set (keys oracle-cases))))
  (doseq [[id cases] oracle-cases
          [params states] cases]
    (when-let [{:keys [wasm max-error nan-states]} (ko/deviation (as/figures id) params states)]
      (is wasm (str id))
      (is (empty? nan-states) (str id " " nan-states))
      (is (< max-error 1e-6) (str id " " max-error)))))

(deftest the-clerk-value
  (let [{:keys [steps kernels data]} (view/value)]
    (is (= 10 (count steps)))
    (is (= (set (keys as/figures)) (set (keys kernels))))
    (is (every? (comp string? :glue) (vals kernels)))
    (is (= :raster (:source data))))
  (is (some? (view/identities))))
