(ns alexandria.cauchy.limit-widgets-test
  (:require [alexandria.cauchy.analysis :as cauchy]
            [alexandria.cauchy.analysis-view :as view]
            [alexandria.cauchy.limit :as lim]
            [alexandria.cauchy.limit-scenes]
            [alexandria.cauchy.limit-widget :as lw]
            [alexandria.cauchy.ratio :as ra]
            [alexandria.cauchy.ratio-widget :as rw]
            [alexandria.medium.scene :as scene]
            [alexandria.palette :as palette]
            [alexandria.proofs :as proofs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.walk :as walk]))

(def corpus
  (str (System/getProperty "user.home")
       "/Documents/feed/math-history/18-cauchy-cours-d-analyse/text/cours-d-analyse-oeuvres-wikisource.txt"))

(defn- squash [s] (-> s (str/replace #"[’']" "'") (str/replace #"\s+" " ") (str/replace #"[−-]" "-")))

(deftest the-new-quotes-are-cauchys-words
  (let [text (try (squash (slurp corpus)) (catch Exception _ nil))]
    (if-not text
      (is true "corpus not on this machine")
      (doseq [id [:cauchy/limit :cauchy/continuity-ch2]
              {:keys [fr]} (:quotes (view/part id))]
        (is (str/includes? text (str/replace (squash fr) "f(x + α) - f(x)" "f(x+α)-f(x)")) fr)))))

(deftest every-sentence-is-a-step
  (doseq [[id n] {:cauchy/limit 6 :cauchy/continuity-ch2 6}]
    (is (= n (count (proofs/steps cauchy/proofs-resource id))))))

(deftest every-new-stage-is-drawn
  (doseq [[id data] {:cauchy/limit (view/limit-data) :cauchy/continuity-ch2 (view/continuity-ch2-data)}
          {:keys [stage]} (proofs/steps cauchy/proofs-resource id)
          p [0 0.37 1]]
    (let [h (scene/draw id stage p {:palette (palette/palette :deck) :data data})]
      (is (and (vector? h) (not= :text (first h))) (str id " " stage " at " p)))))

(deftest the-limit-data-close-on-their-limits
  (let [{:keys [xs ranks polygons pi]} (view/limit-data)]
    (is (< (Math/abs (- pi Math/PI)) 1e-6))
    (is (apply < (map :area polygons)))
    (is (< (- pi (:area (peek polygons))) 0.03))
    (doseq [{:keys [eps N]} ranks [n x] xs :when (>= n N)] (is (< (Math/abs (- x 1)) eps)))))

(deftest the-difference-decreases-with-alpha
  (let [{:keys [alphas jump]} (view/continuity-ch2-data)]
    (is (apply > (map :max alphas)))
    (is (< (:max (peek alphas)) 0.02))
    (testing "at the jump it does not"
      (is (every? #(> (- (:fxa %) (:fx %)) 0.79) (:alphas jump))))))

(defn- kernel-count [form]
  (let [acc (volatile! 0)]
    (walk/postwalk (fn [x] (when (and (seq? x) (= 'js/Function. (first x)) (= "fb" (second x))) (vswap! acc inc)) x) form)
    @acc))

(deftest the-eps-delta-widget-ships-raster-kernels
  (let [w (lw/widget)
        s (pr-str w)]
    (testing "a game kernel per function, two closed forms, a curve per function"
      (is (>= (kernel-count w) (+ 4 2 4))))
    (is (str/includes? s "WebAssembly") "raster wasm glue")
    (is (str/includes? s ":cauchy/tie"))
    (is (str/includes? s "data-figure"))
    (testing "the start is the raster search"
      (is (< (Math/abs (- (:delta (lw/init)) (- (Math/sqrt 1.2) 1))) 1e-9)))))

(deftest the-ratio-widget-carries-the-raster-table
  (let [d (rw/data)]
    (is (= (set (map #(get-in ra/series [% :label]) ra/order)) (set (keys d))))
    (doseq [[_ {:keys [root ratio u]}] d]
      (is (= ra/horizon (count root) (count ratio) (count u))))
    (is (= 13 (:N (rw/init))))
    (is (str/includes? (pr-str (rw/widget)) ":cauchy/ratio-tie"))))
