(ns alexandria.archimedes.method
  "Archimedes, The Method, Proposition 1: the parabolic segment weighed
   against a triangle on a balance (Heath 1912, pp. 15-18).

   Heath's letters on the segment of y = 1 - x^2 cut by the chord AC from
   (-1, 0) to (1, 0): D the middle of AC, B on the curve above it, E where
   the tangent at C meets DB, F where it meets the parallel AF, K where CB
   meets AF (the fulcrum, the middle of the bar CH), W the centre of gravity
   of the triangle ACF.

     points      Heath's points, exact
     slice       the line through x = t: O on AC, P on the curve, N on CK,
                 M on CF
     figures     Emmy functions of one point, for media to compile
     identities  the lever law at every slice, and the areas, graded"
  (:require [alexandria.grade :as grade]
            [alexandria.proofs :as proofs]
            [emmy.env :as e]
            [hive-dsl.result :as r]
            [alexandria.raster :as raster]))

(def points
  {:A [-1 0] :C [1 0] :D [0 0] :B [0 1] :E [0 2]
   :F [-1 4] :K [-1 2] :H [-3 4] :W [-1/3 4/3]})

(defn curve [t] (e/- 1 (e/square t)))

(defn slice
  "The points the line through x = t meets: O on AC, P on the curve, N on
   CK, M on the tangent CF."
  [t]
  {:O [t 0] :P [t (curve t)] :N [t (e/- 1 t)] :M [t (e/- 2 (e/* 2 t))]})

(defn- clamp01 [x] (e// (e/+ 1 (e/- (e/abs x) (e/abs (e/- x 1)))) 2))

(def spread
  "How far apart the hung lines sit at H, so the eye can tell them apart;
   Archimedes puts every one exactly at H."
  0.22)

(defn hang
  "End e (0 the foot O, 1 the head P) of the slice of the segment through
   x = t (-1 <= t <= 1) while all slices are carried to H, left to right:
   at sweep s = 0 every slice is in place, at s = 1 every slice hangs with
   its middle at H (spread sideways by `spread`)."
  [s]
  (fn [[t end]]
    (let [len (curve t)
          u (clamp01 (e// (e/- (e/* s 1.6) (e/* 0.3 (e/+ t 1))) 1.0))
          [hx hy] (points :H)
          x (e/+ t (e/* u (e/- (e/+ hx (e/* spread t)) t)))
          y-place (e/* end len)
          y-hung (e/+ hy (e/* len (e/- end 1/2)))]
      [x (e/+ y-place (e/* u (e/- y-hung y-place)))])))

(def figures
  "Every moving figure, by name: {:f figure :params initial-params :state initial-state}."
  {:hang {:f hang :params [0] :state [0 0]}})

;; ---------------------------------------------------------------------------
;; What the balance says, graded

(defn- length [[_ y0] [_ y1]] (e/- y1 y0))

(def identities
  "[label difference] pairs, each zero."
  (let [t 't
        {:keys [O P N M]} (slice t)
        mo (length O M)
        op (length O P)
        kn (e/- t (first (points :K)))
        hk (e/- (first (points :K)) (first (points :H)))]
    [["N is the middle of MO" (e/- (length O N) (e// mo 2))]
     ["the parabola: MO : OP = CA : AO" (e/- (e/* mo (e/+ t 1)) (e/* op 2))]
     ["the lever at every slice: MO x KN = OP x HK" (e/- (e/* mo kn) (e/* op hk))]
     ["W divides KC so that CK = 3 KW" (e/- (e/- (first (points :C)) (first (points :K)))
                                          (e/* 3 (e/- (first (points :W)) (first (points :K)))))]]))

(defn- triangle-area [[ax ay] [bx by] [cx cy]]
  (e/abs (e// (e/- (e/* (e/- bx ax) (e/- cy ay)) (e/* (e/- cx ax) (e/- by ay))) 2)))

(defn areas
  "The three areas of the proposition: triangle ABC, triangle ACF (exact),
   and the segment, the curve integrated over AC by raster's quadrature
   (alexandria.raster/integral: {:value :error :source :raster})."
  []
  (let [{:keys [A B C F]} points]
    {:ABC (triangle-area A B C)
     :ACF (triangle-area A C F)
     :segment (raster/integral curve -1 1)}))

(defn graded
  "[{:label :grade}]: the identities, and the conclusion segment = 4/3 ABC
   with the segment computed by raster (that entry carries :source :raster)."
  []
  (let [{:keys [ABC ACF segment]} (areas)
        g (fn [kind diffs] (let [res (grade/grade kind diffs)]
                             (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails)))]
    (conj (mapv (fn [[label diff]] {:label label :grade (g :symbolic [diff])}) identities)
          {:label "triangle ACF = 4 triangle ABC" :grade (g :symbolic [(e/- ACF (e/* 4 ABC))])}
          {:label "segment ABC = 4/3 triangle ABC" :source (:source segment)
           :grade (g :numeric [(e/- (:value segment) (e/* 4/3 ABC))])})))

;; ---------------------------------------------------------------------------
;; The proposition as data

(def proofs-resource "alexandria/archimedes/the_method.proofs.edn")

(defn proof [] (proofs/read-proofs proofs-resource))
