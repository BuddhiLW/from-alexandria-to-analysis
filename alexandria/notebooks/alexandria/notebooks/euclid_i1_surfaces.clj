(ns alexandria.notebooks.euclid-i1-surfaces
  "Euclid I.1, carried by Alexandria's Surface port on three constant-curvature surfaces."
  (:require [alexandria.ctx :as ctx]
            [alexandria.ctx.sphere]
            [alexandria.ctx.hyperbolic]
            [alexandria.ctx.plane]
            [alexandria.measure :as measure]
            [emmy.structure :as s]
            [nextjournal.clerk :as clerk]
            [hive-dsl.result :as r]))

(def samples 96)

(defn close? ([a b] (close? a b 1.0e-6)) ([a b tol] (<= (Math/abs (- (double a) (double b))) tol)))

(def surfaces
  [{:id :sphere
    :title "Sphere, K = +1"
    :context (ctx/make :sphere {})
    :tangent-a (s/up 0 0)
    :tangent-b (s/up 0.65 0)}
   {:id :plane
    :title "Euclidean plane, K = 0"
    :context (ctx/make :plane {})
    :tangent-a (s/up 0 0)
    :tangent-b (s/up 1.0 0)}
   {:id :hyperbolic
    :title "Poincaré disk, K = -1"
    :context (ctx/make :hyperbolic {:model :poincare})
    :tangent-a (s/up 0 0)
    :tangent-b (s/up 0.65 0)}])

(defn embed2 [ctx p]
  (let [e (vec (ctx/embed ctx p))]
    (if (= 2 (count e)) e [(first e) (second e)])))

(defn- ok!
  "Unwrap a Result at the notebook boundary; an Err stops the build with its data."
  [res]
  (if (r/ok? res) (:ok res) (throw (ex-info "construction failed" (:error res)))))

(defn construction
  "Carry Euclid I.1 out from the same data: A, B, two equal circles, left meet C,
   then the three geodesic sides."
  [{:keys [context tangent-a tangent-b] :as surface}]
  (let [A (ctx/point context tangent-a)
        B (ctx/point context tangent-b)
        r (ctx/distance context A B)
        alpha {:circle [A r]}
        beta {:circle [B r]}
        C (:point (first (ctx/meet context alpha beta)))
        sides {:AB {:geodesic [A B]}
               :BC {:geodesic [B C]}
               :CA {:geodesic [C A]}}
        lengths {:AB (ctx/distance context A B)
                 :BC (ctx/distance context B C)
                 :CA (ctx/distance context C A)}
        angles (ok! (r/let-ok [a (ctx/angle context A B C)
                               b (ctx/angle context B C A)
                               c (ctx/angle context C A B)]
                      (r/ok {:A a :B b :C c})))
        angle-sum (reduce + (vals angles))
        excess (- angle-sum Math/PI)
        K (double (ctx/curvature context A))
        curvature-integral (measure/geodesic-triangle-integral context A B C)]
    (assoc surface
           :A A :B B :C C :radius r :sides sides :lengths lengths
           :angles angles :angle-sum angle-sum :excess excess
           :curvature K :curvature-integral curvature-integral)))

(def constructions (mapv construction surfaces))

(defn sample-curve [f]
  (mapv f (map #(/ % (double (dec samples))) (range samples))))

(defn sample-circle [f]
  (mapv f (map #(* 2 Math/PI (/ % (double samples))) (range (inc samples)))))

(defn scatter2 [ctx label pts mode]
  (let [xy (mapv #(embed2 ctx %) pts)]
    {:type "scatter" :mode mode :name label
     :x (mapv first xy) :y (mapv second xy)}))

(defn scatter3 [ctx label pts mode]
  (let [xyz (mapv #(vec (ctx/embed ctx %)) pts)]
    {:type "scatter3d" :mode mode :name label
     :x (mapv first xyz) :y (mapv second xyz) :z (mapv #(nth % 2) xyz)}))

(defn figure [{:keys [id title context A B C radius sides]}]
  (let [s3? (= id :sphere)
        scatter (if s3? scatter3 scatter2)
        circle-a (sample-circle (ctx/circle context A radius))
        circle-b (sample-circle (ctx/circle context B radius))
        side (fn [[_ {:keys [geodesic]}]] (sample-curve (apply ctx/geodesic context geodesic)))
        data (concat
              [(scatter context "circle at A" circle-a "lines")
               (scatter context "circle at B" circle-b "lines")]
              (map-indexed (fn [i entry] (scatter context (str "side " (name (key entry))) (side entry) "lines")) sides)
              [(scatter context "A, B, C" [A B C] "markers+text")])
        layout (merge {:title title :showlegend true}
                      (if s3?
                        {:scene {:aspectmode "data"}}
                        {:xaxis {:scaleanchor "y" :scaleratio 1}
                         :yaxis {:scaleanchor "x" :scaleratio 1}}))]
    (clerk/with-viewer clerk/plotly {:data (vec data) :layout layout})))

(defn summary-row [{:keys [title lengths angle-sum excess curvature-integral]}]
  [title
   (format "%.6f, %.6f, %.6f" (double (:AB lengths)) (double (:BC lengths)) (double (:CA lengths)))
   (format "%.6f" (double angle-sum))
   (format "%.6f" (double excess))
   (format "%.6f" (double curvature-integral))])

(def heath-statement
  "On a given finite straight line to construct an equilateral triangle. — Euclid, Elements I.1, trans. T. L. Heath (1908)")

;; # Euclid I.1 on three surfaces

(clerk/md (str "**Statement.** " heath-statement "\n\n"
               "We keep Euclid's construction data fixed: choose A and B, draw the circle centered at A through B, draw the circle centered at B through A, take the left crossing C, and join AB, BC, CA. The meaning of `circle`, `meet`, `geodesic`, and `distance` comes from the current Alexandria context."))

(clerk/table
 {:head ["surface" "side lengths AB, BC, CA" "angle sum" "angle sum − π" "independent ∫ K dA"]
  :rows (mapv summary-row constructions)})

(clerk/md "The Gauss-Bonnet columns compare two independently computed quantities: angle excess from `ctx/angle`, and a Heron-subdivision integral of `ctx/curvature` from `alexandria.measure/geodesic-triangle-integral`.")

(clerk/md "The Plotly figures below are static-Clerk friendly. The spherical picture uses the context embedding in $\\mathbb{R}^3$; the plane and Poincare disk are 2D plots.")

(clerk/row (mapv figure constructions))
