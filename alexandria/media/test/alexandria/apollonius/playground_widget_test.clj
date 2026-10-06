(ns alexandria.apollonius.playground-widget-test
  "The conics playground (polar coordinates as a projection): the tie
   re-solves the state by the forward and inverse maps for each source and
   keeps every key in its slider range; the moving point computed from the
   cone and the plane lands where the focal form puts it; the curves agree;
   every function compiles to a raster wasm kernel; and the numbers the
   page prints are computed by that wasm (run in node)."
  (:require [alexandria.apollonius.playground-widget :as pw]
            [alexandria.raster :as raster]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.walk :as walk]
            [emmy.viewer.compile :as vc]
            [emmy.viewer.raster]))

(defn- close? [a b tol]
  (<= (Math/abs (- (double a) (double b))) (* tol (max 1.0 (Math/abs (double a)) (Math/abs (double b))))))

(defn- rad [d] (* d (/ Math/PI 180)))

(defn- tie
  "The tie kernel at a state, by raster on the JVM: {source [alpha tilt h l e]}."
  [[alpha tilt h l ecc]]
  (let [v (vec (raster/value (fn [a t h l e] ((pw/tie a t h l e) 0)) alpha tilt h l ecc))]
    (into {} (map (fn [[k o]] [k (subvec v o (+ o 5))]) pw/source-block))))

(defn- tied? [[alpha tilt h l ecc]]
  (and (close? l (* h (Math/tan (rad alpha))) 1e-9)
       (close? ecc (* (Math/tan (rad tilt)) (Math/tan (rad alpha))) 1e-9)))

(defn- in-range? [state]
  (every? (fn [[k x]] (let [[lo hi] (pw/ranges k)] (<= (- lo 1e-9) x (+ hi 1e-9))))
          (map vector pw/tied state)))

(def start (mapv (pw/init) pw/tied))

(def ^:private alpha-25
  "The state after alpha moves to 25 degrees."
  ((tie (assoc start 0 25)) :alpha))

(deftest init-is-tied
  (is (tied? start))
  (is (in-range? start)))

(deftest each-source-re-solves-the-others
  (testing "tilt moves: alpha and h fixed, tilt kept, l and e forward"
    (let [[a t h l ecc :as s] ((tie (assoc start 1 60)) :tilt)]
      (is (= [35.0 60.0 1.2] (mapv double [a t h])))
      (is (tied? s))
      (is (close? ecc (* (Math/tan (rad 60)) (Math/tan (rad 35))) 1e-12))
      (is (close? l (nth start 3) 1e-12))))
  (testing "h moves: l follows, e unchanged"
    (let [s ((tie (assoc start 2 2.0)) :h)]
      (is (tied? s))
      (is (close? (nth s 2) 2.0 1e-12))
      (is (close? (nth s 4) (nth start 4) 1e-12))))
  (testing "e moves: alpha and l fixed, tilt and h by the inverse"
    (let [[a t _ l ecc :as s] ((tie (assoc start 4 1.0)) :e)]
      (is (tied? s))
      (is (= 1.0 (double ecc)))
      (is (close? l (nth start 3) 1e-12))
      (is (close? t (Math/toDegrees (Math/atan (/ 1 (Math/tan (rad a))))) 1e-9))))
  (testing "l moves: h follows by the inverse, tilt unchanged"
    (let [s ((tie (assoc start 3 1.5)) :l)]
      (is (tied? s))
      (is (close? (nth s 3) 1.5 1e-12))
      (is (close? (nth s 1) (nth start 1) 1e-9))))
  (testing "alpha moves: the plane fixed, l and e forward"
    (let [[a t h] alpha-25]
      (is (tied? alpha-25))
      (is (close? a 25 1e-12))
      (is (close? t (nth start 1) 1e-12))
      (is (close? h (nth start 2) 1e-12)))))

(deftest the-tie-clamps-at-the-sliders-edges
  (doseq [[src i v] [[:tilt 1 85] [:h 2 2.5] [:l 3 3.0] [:l 3 0.1] [:e 4 3.0] [:e 4 0.0] [:alpha 0 60] [:alpha 0 10]]
          :let [s ((tie (assoc start i v)) src)]]
    (is (in-range? s) (str src "=" v " -> " s))
    (is (tied? s) (str src "=" v " -> " s))))

(deftest the-tie-is-idempotent
  (testing "solving a tied state from any source gives it back"
    (doseq [[src s] (tie start)]
      (is (every? true? (map #(close? %1 %2 1e-9) s start)) (str src " " s)))))

(defn- point [state theta]
  (vec (raster/value (fn [a t h l e th] ((pw/point a t h l e th) 0))
                     (state 0) (state 1) (state 2) (state 3) (state 4) theta)))

(deftest the-shadow-is-the-focal-conic
  (testing "X' from the cone and the plane = X' from rho = l/(1 - e cos theta), X on the plane"
    (doseq [s [start ((tie (assoc start 1 60)) :tilt) ((tie (assoc start 4 1.0)) :e) alpha-25]
            theta (range -170 180 17)
            :let [q (point s theta)
                  {:keys [x y z plane-z x2 y2]} (into {} (map (fn [[k i]] [k (q i)]) pw/point-index))]
            :when (and (Double/isFinite x) (< (Math/abs (double x)) 1e3))]
      (is (< (Math/hypot (- x x2) (- y y2)) 1e-9) (str s " theta=" theta))
      (is (close? z plane-z 1e-9) "X is on the plane"))))

(deftest section-sits-over-its-shadow
  (testing "the section and its shadow share x and y; the section is on the cone (Emmy on doubles)"
    (let [[alpha _ _ l ecc] start]
      (doseq [b [:near :far] t (range -3.1 3.11 0.3)
              :let [sec (vec (((pw/section b) alpha l ecc) t))
                    sh (vec (((pw/shadow b) alpha l ecc) t))]
              :when (Double/isFinite (sec 0))]
        (is (close? (sec 0) (sh 0) 1e-12))
        (is (close? (sec 1) (sh 1) 1e-12))
        (is (zero? (sh 2)))
        (is (close? (sec 2) (/ (Math/hypot (sec 0) (sec 1)) (Math/tan (rad alpha))) 1e-9))))))

;; ## The compiled widget

(defn- built [backend]
  (binding [vc/*backend* backend] (pw/playground)))

(deftest every-function-compiles-to-a-raster-kernel
  (let [fragment (built :js)
        ks (pw/kernel-forms (built :raster))]
    (testing "fifteen kernels, each raster's wasm glue (base64 module, loaded synchronously)"
      (is (= 15 (count ks)))
      (doseq [glue ks]
        (is (str/includes? glue "new WebAssembly.Instance"))
        (is (re-find #"bytes\(\"[A-Za-z0-9+/=]{40,}\"\)" glue))))
    (testing "the playground binds the raster backend whatever the caller's"
      (is (= 15 (count (pw/kernel-forms fragment)))))
    (testing "the HTML letters ride over the scene"
      (is (str/includes? (pr-str fragment) "alexandria.medium.html-labels/overlay")))))

(defn- kernel-pairs
  "The ((js/Function. \"fb\" glue) (js/Function. ...)) pairs inside a form."
  [form]
  (let [acc (atom [])]
    (walk/postwalk (fn [x]
                     (when (and (seq? x) (seq? (first x)) (= 'js/Function. (ffirst x))
                                (= "fb" (second (first x))))
                       (swap! acc conj x))
                     x)
                   form)
    @acc))

(def ^:private node?
  (try (zero? (:exit (sh/sh "node" "--version"))) (catch java.io.IOException _ false)))

(defn- run-node
  "Evaluates kernel pair `kf` in node at state [0] for each params in `pss`."
  [kf pss]
  (let [[[_ _ glue] [_ & fb]] kf
        program (str "const fb = new Function(" (json/write-str (vec (butlast fb))) ".join(','), "
                     (json/write-str (last fb)) ");\n"
                     "const f = new Function('fb', " (json/write-str glue) ")(fb);\n"
                     "const pss = " (json/write-str pss) ";\n"
                     "console.log(JSON.stringify({wasm: typeof f.wasm === 'function' ? f.wasm() : null,"
                     " out: pss.map((ps) => { try { return Array.from(f([0], ps)).map((v) => isFinite(v) ? v : String(v)); } catch (e) { return null; } })}));\n")
        file (java.io.File/createTempFile "playground-widget" ".js")]
    (spit file program)
    (let [{:keys [exit out err]} (sh/sh "node" (str file))]
      (io/delete-file file true)
      (if (zero? exit)
        (json/read-str out :key-fn keyword)
        (throw (ex-info (str "node failed: " err) {}))))))

(deftest the-tie-runs-in-wasm
  (if-not node?
    (println "[playground-widget-test] node not on the path; skipping the wasm run")
    (let [pss [start (assoc start 1 60) (assoc start 4 1.0) (assoc start 0 25)]
          runs (map #(run-node % pss) (kernel-pairs (built :raster)))
          {:keys [wasm out]} (first (filter #(= 25 (count (first (:out %)))) runs))]
      (is (true? wasm) "the tie kernel's wasm module loaded in V8")
      (testing "wasm agrees with the JVM to raster's transcendental accuracy (its tan and atan: measured
                1.3e-5 relative at worst, the inverse branch's tilt 29.9996 for 30)"
        (doseq [[ps got] (map vector pss out)
                :let [want (vec (raster/value (fn [a t h l e] ((pw/tie a t h l e) 0)) (ps 0) (ps 1) (ps 2) (ps 3) (ps 4)))]]
          (is (every? true? (map #(close? %1 %2 5e-5) got want))
              (str ps ": wasm " got ", raster JVM " want)))))))
