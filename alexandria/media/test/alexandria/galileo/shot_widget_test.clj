(ns alexandria.galileo.shot-widget-test
  "Galileo's tied shot: each source re-solves (v, theta, R, H) by the
   range and height laws, keeps every key in range and is idempotent; 45
   degrees gives the greatest range; the fall kernel gives 1 : 3 : 5 : 7;
   every function compiles to a raster wasm kernel."
  (:require [alexandria.galileo.shot-widget :as sw]
            [alexandria.raster :as raster]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.walk :as walk]
            [emmy.viewer.compile :as vc]
            [emmy.viewer.raster]))

(defn- close? [a b tol]
  (<= (Math/abs (- (double a) (double b))) (* tol (max 1.0 (Math/abs (double a)) (Math/abs (double b))))))

(defn- tied? [[v deg R H]]
  (let [th (Math/toRadians deg)]
    (and (close? R (/ (* 2 v v (Math/sin th) (Math/cos th)) sw/g) 1e-9)
         (close? H (/ (Math/pow (* v (Math/sin th)) 2) (* 2 sw/g)) 1e-9))))

(defn- in-range? [s]
  (every? (fn [[k x]] (let [[lo hi] (sw/ranges k)] (<= (- lo 1e-9) x (+ hi 1e-9)))) (map vector sw/tied s)))

(def start (mapv (sw/init) sw/tied))

(deftest each-source-re-solves-the-others
  (is (tied? start))
  (testing "R moves: theta fixed, v solved back"
    (let [[v deg R :as s] ((sw/solve (assoc start 2 50.0)) :R)]
      (is (tied? s)) (is (close? R 50.0 1e-12)) (is (close? deg 30.0 1e-12))))
  (testing "H moves: theta fixed, v solved back"
    (let [[_ deg _ H :as s] ((sw/solve (assoc start 3 5.0)) :H)]
      (is (tied? s)) (is (close? H 5.0 1e-12)) (is (close? deg 30.0 1e-12))))
  (testing "theta moves: v fixed"
    (let [[v _ R :as s] ((sw/solve (assoc start 1 45.0)) :theta)]
      (is (tied? s)) (is (close? v 18.0 1e-12)) (is (close? R (/ (* 18 18) sw/g) 1e-9) "45 degrees: R = v^2/g")))
  (testing "v moves: theta fixed"
    (is (tied? ((sw/solve (assoc start 0 25.0)) :v)))))

(deftest clamps-and-idempotence
  (doseq [[src i x] [[:v 0 40] [:v 0 1] [:R 2 500] [:R 2 0] [:H 3 400] [:theta 1 95]]
          :let [s ((sw/solve (assoc start i x)) src)]]
    (is (in-range? s) (str src "=" x " " s))
    (is (tied? s) (str src "=" x " " s)))
  (doseq [[src s] (sw/solve start)]
    (is (every? true? (map #(close? %1 %2 1e-9) s start)) (str src))))

(deftest forty-five-is-the-greatest-range
  (let [Rs (for [d (range 1 90)] [d (((sw/solve (assoc start 1 (double d))) :theta) 2)])]
    (is (= 45 (first (apply max-key second Rs))))))

(deftest the-fall-is-in-the-odd-numbers
  (let [[_ & steps] (raster/value (fn [D] ((sw/fall-readout D) 0)) 37.0)]
    (is (every? true? (map #(close? %1 %2 1e-9) steps [1 3 5 7 9 11 13 15])))))

(deftest every-function-compiles-to-a-raster-kernel
  (let [acc (volatile! [])]
    (walk/postwalk (fn [x] (when (and (seq? x) (= 'js/Function. (first x)) (= "fb" (second x))) (vswap! acc conj (nth x 2))) x)
                   (binding [vc/*backend* :js] (sw/widget)))
    (is (= 5 (count @acc)))
    (doseq [k @acc] (is (str/includes? k "new WebAssembly.Instance")))))
