(ns alexandria.euler.opera-view
  "Euler's four results as proof players for a Clerk notebook, and the
   Latin passages as data a page can quote. Each player plays the steps of
   alexandria/euler/opera.proofs.edn over the scenes of
   alexandria.euler.*-scenes."
  (:require [alexandria.euler.graphs :as graphs]
            [alexandria.euler.polyhedra :as polyhedra]
            [alexandria.euler.series :as series]
            [alexandria.medium.clerk :as medium]
            [alexandria.proofs :as proofs]
            [hive-dsl.result :as r]
            [clojure.walk :as walk]))

(def ^:private resource graphs/proofs-resource)

(defn passages
  "{:latin {:passages :locus :source} :english [...]} of proposition id."
  [id]
  (let [res (proofs/read-proofs resource)]
    (when (r/ok? res) (select-keys (get-in res [:ok id]) [:latin :english]))))

(defn- steps [id] (proofs/steps resource id))

(defn- plain [x] (walk/postwalk #(if (number? %) (double %) %) x))

(def ^:private fifteen-layout
  "Where the regions of E53 par. 15 (his Figure 3) sit in the player: the
   islands A and B in the middle, C above, D and F below, E to the right."
  {:A [-0.9 0.15] :B [0.6 0.15] :C [-0.15 1.25] :D [-1.9 -0.6] :E [1.9 -0.1] :F [-0.15 -1.0]})

(defn- node-points [g] (into {} (map (juxt key (comp (fn [[x y]] [(double x) (double y)]) val))) g))

(defn koenigsberg-data
  "What the Koenigsberg scenes read: the city (coordinates as doubles), the
   bridges, the degrees, the table of par. 14 (counts as integers); for par.
   15 Euler's fifteen-bridge graph with his route and table, for par. 18 the
   doubled bridges with a walk over all fourteen crossings."
  []
  {:city (plain graphs/city)
   :bridges (:bridges graphs/koenigsberg)
   :degrees (graphs/degrees graphs/koenigsberg)
   :table (graphs/euler-table graphs/koenigsberg)
   :fifteen {:nodes (node-points fifteen-layout)
             :bridges (:bridges graphs/fifteen-bridges)
             :steps (graphs/route->steps graphs/route-15)
             :odd (graphs/odd-regions graphs/fifteen-bridges)
             :table (graphs/euler-table graphs/fifteen-bridges)}
   :doubled {:nodes (into {} (map (fn [[k {:keys [node]}]] [k node])) (:regions (plain graphs/city)))
             :bridges (:bridges (graphs/doubled graphs/koenigsberg))
             :steps graphs/twice-walk}})

(defn koenigsberg []
  (medium/player 'alexandria.euler.graphs-scenes/render :euler/koenigsberg
                 (steps :euler/koenigsberg) {} (koenigsberg-data)
                 {:window [[-2.75 2.75] [-2.1 1.7]] :height 400
                  :durations {:city 5000 :collapse 6000 :letters 6000 :degree 6000 :count 7000
                              :even 9000 :table 7000 :fifteen 15000 :handshake 6000
                              :doubled 14000 :rule 6000}}))

(defn basel-data
  "Euler's partial sums 1, 1 + 1/4, ..., forty of them, each a raster loop
   kernel (alexandria.euler.series/partial-sums-raster), and pi^2/6."
  []
  {:partial-sums (series/partial-sums-raster 2 (range 1 41))
   :target (/ (* Math/PI Math/PI) 6)})

(defn basel []
  (medium/player 'alexandria.euler.series-scenes/render :euler/basel
                 (steps :euler/basel) (select-keys series/figures [:product]) (basel-data)
                 {:window [[-10.5 10.5] [-1.2 2.2]] :height 380
                  :durations {:product 9000 :match 7000 :partial-sums 8000 :fourth 7000}}))

(defn exponential []
  (medium/player 'alexandria.euler.series-scenes/render :euler/exponential
                 (steps :euler/exponential) (select-keys series/figures [:spiral]) {}
                 {:window [[-4.3 2.4] [-1.5 3.6]] :height 420
                  :controls [{:id :x :label "x" :min 0.2 :max 3.14159 :step 0.01 :init 2.0}]
                  :durations {:spiral 9000 :split 7000 :circle 7000 :pi 7000}}))

(defn polyhedra-data []
  {:cube (plain polyhedra/cube-vertices)
   :faces (conj (:faces polyhedra/schlegel-cube) (:outer polyhedra/schlegel-cube))
   :steps (mapv #(select-keys % [:move :faces :counts]) (polyhedra/cauchy-steps))
   :solids (mapv (fn [s] (assoc (polyhedra/counts s) :name (:name s))) polyhedra/platonic)})

(defn polyhedra []
  (medium/player 'alexandria.euler.polyhedra-scenes/render :euler/polyhedra
                 (steps :euler/polyhedra) polyhedra/figures (polyhedra-data)
                 {:window [[-3.2 3.2] [-2.1 2.1]] :height 400
                  :durations {:cube 5000 :solids 7000 :flatten 6000 :triangulate 6000 :peel 9000}}))
