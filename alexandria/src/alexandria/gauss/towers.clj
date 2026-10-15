(ns alexandria.gauss.towers
  "Data of Gauss' period towers for the widgets: for every prime p up to a
   bound, each level of the tower (alexandria.gauss.heptadecagon/periods)
   with its period values (raster), the degree of the equation that splits
   the level above into it, and the period each root r^k (k = 1..p-1)
   belongs to. Plain data: the browser only colours and prints it."
  (:require [alexandria.gauss.heptadecagon :as h]))

(defn tower
  "{:p :g :levels [{:e :f :degree :values [..] :member [i ..] :exps}]}.
   :degree is e / e-before (2 for every level of a Fermat prime); the first
   level (e = 1, the sum of all roots, -1) has degree 1."
  [p]
  (let [levels (h/tower-values p)]
    {:p p :g (h/primitive-root p)
     :levels (vec (map-indexed
                   (fn [i {:keys [e exps] :as lvl}]
                     (let [owner (into {} (for [[j ks] (map-indexed vector exps) k ks] [k j]))]
                       (assoc lvl
                              :degree (if (zero? i) 1 (quot e (:e (nth levels (dec i)))))
                              :member (mapv #(owner (min % (- p %))) (range 1 p)))))
                   levels))}))

(def towers
  "{p (tower p)} for the odd primes up to n (memoized)."
  (memoize (fn [n] (into (sorted-map) (for [p (range 3 (inc n)) :when (h/prime? p)] [p (tower p)])))))

(defn classes
  "{n (classify n)} for 3 <= n <= max, with the names the page prints."
  [max]
  (into (sorted-map) (for [n (range 3 (inc max))] [n (h/classify n)])))
