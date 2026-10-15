(ns alexandria.euler.playgrounds-test
  (:require [alexandria.euler.playgrounds :as pg]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [emmy.expression.compile]))

(defn- close? [a b tol] (< (Math/abs (- (double a) (double b))) tol))

(defn- shape-ok [frag id]
  (let [s (pr-str frag)]
    (is (str/includes? s (str "\"" id "\"")) "data-playground id")
    (is (str/includes? s "MovablePoint") "draggable objects on the figure")
    (is (str/includes? s "Try this") "try-this prompts")
    (is (str/includes? s "Reset") "reset to the author's configuration")
    (is (<= 3 (count (re-seq #":data-preset" s))) "at least two presets and a reset")
    (is (str/includes? s "data-case") "a live case name")
    (is (str/includes? s "data-hypothesis") "a hypothesis badge")
    (is (not (str/includes? s "http")) "no remote script")))

(deftest basel-kernel
  (testing "S_N, zeta(s), the tail inside the integral-test bounds, the product"
    (let [v ((pg/basel-values 10.0 2.0 2.0) nil)]
      (is (close? (v 0) 1.5497677311665408 1e-12))
      (is (close? (v 1) (/ (* Math/PI Math/PI) 6) 1e-9))
      (is (< (v 4) (v 2) (v 3)))
      (is (close? (v 9) (- (/ (reduce + (map #(/ 1.0 (* % %)) (range 1 11))) (* Math/PI Math/PI))) 1e-12))))
  (testing "s = 4 gives pi^4/90 and the product at 40 factors is close to sin x / x"
    (let [v ((pg/basel-values 40.0 4.0 2.0) nil)]
      (is (close? (v 1) (/ (Math/pow Math/PI 4) 90) 1e-9))
      (is (< (Math/abs (v 8)) 0.01)))))

(deftest basel-shape
  (shape-ok (pg/basel-playground) "euler-basel"))

(defn- kb [counts]
  ((apply pg/kb-values (concat (map pg/kb-places [:ax :ay :bx :by :cx :cy :dx :dy])
                               (map (comp double counts first) pg/kb-pairs))) nil))

(deftest koenigsberg-kernel
  (testing "Euler's table of par. 14: 5, 3, 3, 3 bridges; 9 letters for a walk of 8"
    (let [v (kb pg/kb-init)]
      (is (= [5.0 3.0 3.0 3.0] (subvec v 0 4)))
      (is (= 4.0 (Math/rint (v 8))))
      (is (= [7.0 9.0 8.0 1.0] (mapv #(Math/rint (v %)) [9 10 11 12])))))
  (testing "an eighth bridge B-C: two odd regions, letters = bridges + 1"
    (let [v (kb (assoc pg/kb-init :BC 1))]
      (is (= 2.0 (Math/rint (v 8))))
      (is (= 0.0 (Math/rint (v 12))))))
  (testing "split city: the reach matrix sees A and C apart"
    (let [v (kb {:AB 2 :AC 0 :AD 0 :BC 0 :BD 0 :CD 2})]
      (is (= 0.0 (Math/rint (v 8))))
      (is (zero? (v (+ 17 2))) "(I + A)^3 at A, C"))))

(deftest koenigsberg-shape
  (shape-ok (pg/koenigsberg-playground) "euler-koenigsberg"))

(defn- vef
  "The kernel twice, as the page calls it: once for the angles, the faces
   traced by the page's own form (evaluated here), then for the counts."
  [st]
  (let [trace (eval @#'pg/vef-trace) pieces (eval @#'pg/vef-pieces)
        s (pg/vef-solids (:solid st)) es (vec (concat (:edges s) (:diagonals s)))
        on (mapv #(if % 1.0 0.0) (:on st))
        f (pg/vef-values s)
        np (double (pieces es (:on st))) ou (if (:outer st) 1.0 0.0)
        r0 ((apply f (concat (:pts st) on [0.0 np ou])) nil)
        faces (trace es (:on st) (fn [k sd] (r0 (+ 9 (long (r0 0)) (* 2 k) sd))))]
    (mapv #(Math/rint (double %)) (subvec ((apply f (concat (:pts st) on [(double (count faces)) np ou])) nil) 0 9))))

(deftest vef-kernel
  (testing "V - E + F = 2 on the Schlegel maps of the tetrahedron, cube, octahedron"
    (is (= [[4.0 6.0 4.0 2.0] [8.0 12.0 6.0 2.0] [6.0 12.0 8.0 2.0]]
           (for [i [0 1 2]] (let [v (vef (pg/vef-start i))] [(v 0) (v 1) (v 5) (v 6)])))))
  (let [cube (pg/vef-start 1)]
    (testing "Cauchy's steps 2-3: diagonals drawn, outer face away, V - E + F = 1"
      (let [v (vef (assoc cube :on (vec (repeat 17 true)) :outer true))]
        (is (= [8.0 17.0 10.0 1.0] [(v 0) (v 1) (v 7) (v 8)]))))
    (testing "the map split in two: 2 pieces, V - E + F = 3"
      (let [v (vef (assoc cube :on (vec (concat (repeat 8 true) (repeat 9 false)))))]
        (is (= [2.0 3.0] [(v 3) (v 6)]))))
    (testing "a corner pulled across: crossings counted"
      (is (pos? ((vef (assoc cube :pts (assoc (:pts cube) 8 2.8 9 0.4))) 2))))))

(deftest vef-first-load
  ;; Until a large kernel's wasm has loaded asynchronously, the page calls the
  ;; :js fallback Emmy compiles beside it. That fallback writes atan2 as
  ;; Math.atan(y, x), which drops x: on first load the half-edges sorted
  ;; wrong and the cube showed F = 4, V - E + F = 0. The angle keys must
  ;; mean the same in JS as in the kernel.
  (testing "the fallback the page runs before the wasm loads has no two-argument Math.atan"
    (doseq [s pg/vef-solids]
      (let [n (+ (* 2 (count (:pts s))) (count (:edges s)) (count (:diagonals s)) 3)
            args (vec (concat (map double (mapcat identity (:pts s))) (repeat n 1.0)))
            js (pr-str (emmy.expression.compile/compile-state-fn
                        (pg/vef-values s) (subvec args 0 n)
                        [0] {:mode :js :simplify? false :cache? false}))]
        (is (not (re-find #"Math\.atan\(\s*[^,()]+,[^,()]+\)" js)) (:name s)))))
  (testing "the angle key orders directions round a vertex cyclically, as atan2 does"
    (let [ks (map #(pg/pseudo-angle (Math/cos %) (Math/sin %)) (range -3.1 3.1 0.05))
          drops (count (filter (fn [[a b]] (>= a b)) (partition 2 1 ks)))]
      (is (= 1 drops) "one cut, every other step rising")
      (is (> (first ks) (last ks)) "the cut closes the circle")))
  (testing "the cube as first loaded: the faces traced from the keys give V - E + F = 2"
    (let [v (vef (pg/vef-start 1))]
      (is (= [8.0 12.0 6.0 2.0] [(v 0) (v 1) (v 5) (v 6)])))))

(deftest vef-shape
  (shape-ok (pg/vef-playground) "euler-vef"))
