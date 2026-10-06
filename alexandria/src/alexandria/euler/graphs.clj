(ns alexandria.euler.graphs
  "Euler, Solutio problematis ad geometriam situs pertinentis (E53, 1736):
   the seven bridges of Koenigsberg.

     multigraph  regions are letters, bridges are named edges {:id :ends};
                 Euler's own letters (A the island Kneiphof, B and C the
                 banks, D the land between the two branches; bridges a..g)
     degree      the number of bridges that lead into a region (E53 par. 8)
     the rule    a walk crossing every bridge once exists only if at most
                 two regions have an odd number of bridges (par. 20);
                 decided by Euler's count of letters (par. 9, 14) and,
                 for the record, by trying every walk, which agree
     par. 15     Euler's second example, fifteen bridges and six regions,
                 with the route he wrote out, checked as a walk

   Reuse searched: Emmy has no graph module; desargues' boards are geometric
   constructions, not multigraphs; no graph library is on the classpath. The
   functions here are plain Clojure over maps. Grades come from
   alexandria.grade on exact integer differences."
  (:require [alexandria.grade :as grade]
            [alexandria.proofs :as proofs]
            [hive-dsl.result :as r]))

;; ---------------------------------------------------------------------------
;; The multigraphs, as data

(def koenigsberg
  "E53 par. 2 and 6: A-B by a and b, A-C by c and d, A-D by e, B-D by f,
   C-D by g."
  {:regions [:A :B :C :D]
   :bridges [{:id :a :ends [:A :B]} {:id :b :ends [:A :B]}
             {:id :c :ends [:A :C]} {:id :d :ends [:A :C]}
             {:id :e :ends [:A :D]} {:id :f :ends [:B :D]} {:id :g :ends [:C :D]}]})

(def route-15
  "E53 par. 15, Euler's route over fifteen bridges as he printed it: regions
   (capitals) alternate with the bridges crossed (small letters)."
  "EaFbBcFdAeFfCgAhCiDkAmEnApBoElD")

(defn route->steps
  "[[from bridge to] ...] of a route written in Euler's letters."
  [route]
  (mapv (fn [[from bridge to]] [(keyword from) (keyword bridge) (keyword to)])
        (partition 3 2 (map str route))))

(def fifteen-bridges
  "E53 par. 15: the multigraph Euler's route crosses, each bridge once."
  (let [steps (route->steps route-15)]
    {:regions (vec (sort (distinct (mapcat (fn [[u _ v]] [u v]) steps))))
     :bridges (mapv (fn [[u b v]] {:id b :ends [u v]}) steps)}))

;; ---------------------------------------------------------------------------
;; Degrees and Euler's count of letters

(defn degrees
  "{region number-of-bridges-leading-into-it}."
  [{:keys [regions bridges]}]
  (reduce (fn [m {[u v] :ends}] (-> m (update u inc) (update v inc)))
          (zipmap regions (repeat 0))
          bridges))

(defn odd-regions [g] (vec (sort (keep (fn [[k d]] (when (odd? d) k)) (degrees g)))))

(defn letter-count
  "E53 par. 8 and 12: how often a region's letter appears in the record of a
   walk crossing every bridge once, when the walk does not start there:
   (d + 1)/2 for odd d, d/2 for even d."
  [d]
  (if (odd? d) (quot (inc d) 2) (quot d 2)))

(defn euler-table
  "E53 par. 14: the number of bridges, the number prefixed (bridges + 1),
   the rows {:region :degree :letters :even?} (Euler stars the even ones)
   and the sum of the letters column."
  [g]
  (let [ds (degrees g)
        rows (mapv (fn [k] (let [d (ds k)] {:region k :degree d :letters (letter-count d) :even? (even? d)}))
                   (:regions g))]
    {:bridges (count (:bridges g))
     :prefixed (inc (count (:bridges g)))
     :rows rows
     :sum (reduce + (map :letters rows))}))

(defn euler-rule
  "E53 par. 20: :none when more than two regions have an odd number of
   bridges, :from-odd when exactly two (start at one of them), :anywhere when
   none."
  [g]
  (case (count (odd-regions g)) 0 :anywhere 2 :from-odd :none))

(defn successions
  "E53 par. 6: {#{u v} n}, how often the letters u and v must stand next to
   each other in the record of a walk crossing every bridge once: once per
   bridge joining u and v (AB twice at Koenigsberg, AC twice, AD, BD, CD
   once)."
  [g]
  (frequencies (map (comp set :ends) (:bridges g))))

(defn letter-count-from
  "E53 par. 8, 11 and 12: how often a region's letter appears when d bridges
   lead into it and the walk starts there (start? true) or elsewhere. Odd d:
   (d + 1)/2 either way (par. 8). Even d: d/2 + 1 from there, d/2 from
   elsewhere (par. 11-12: two bridges, twice or once; four, three or two;
   six, four or three)."
  [d start?]
  (cond (odd? d) (quot (inc d) 2)
        start? (inc (quot d 2))
        :else (quot d 2)))

(defn excess
  "E53 par. 13, 18, 19: the sum of the letters column of par. 14 minus the
   number prefixed (bridges + 1). Euler: -1 when every count is even (par.
   18, a walk from any region), 0 when two are odd (par. 19, a walk from
   either odd region), 1, 2, 3, ... when four, six, eight, ... are odd (no
   walk). The letters column adds up to (sum of degrees + odd regions)/2 =
   bridges + (odd regions)/2, so the excess is (odd regions)/2 - 1."
  [g]
  (let [{:keys [sum prefixed]} (euler-table g)] (- sum prefixed)))

(defn doubled
  "E53 par. 18: every bridge of g crossed twice, as if each were split in
   two; every region then has an even count."
  [g]
  (update g :bridges #(vec (mapcat (fn [{:keys [id ends]}]
                                     [{:id id :ends ends}
                                      {:id (keyword (str (name id) "'")) :ends ends}])
                                   %))))

(def twice-walk
  "E53 par. 18: a walk over Koenigsberg crossing every bridge twice (b' the
   second crossing of b), from A back to A; graded in `graded`."
  [[:A :a :B] [:B :a' :A] [:A :b :B] [:B :b' :A] [:A :c :C] [:C :c' :A]
   [:A :d :C] [:C :d' :A] [:A :e :D] [:D :f :B] [:B :f' :D] [:D :g :C]
   [:C :g' :D] [:D :e' :A]])

;; ---------------------------------------------------------------------------
;; Exhaustion (the method par. 3 sets aside), for the record

(defn- incident [g region]
  (keep (fn [{:keys [id] [u v] :ends}]
          (cond (= u region) [id v] (= v region) [id u]))
        (:bridges g)))

(defn longest-trail
  "The greatest number of bridges any walk crosses without crossing one
   twice, found by trying every walk from every region."
  [g]
  (letfn [(go [region used]
            (reduce max (count used)
                    (for [[id to] (incident g region) :when (not (used id))]
                      (go to (conj used id)))))]
    (reduce max 0 (map #(go % #{}) (:regions g)))))

(defn walk?
  "True when steps [[from bridge to] ...] is a walk of g crossing every
   bridge exactly once."
  [g steps]
  (let [ends (into {} (map (juxt :id (comp set :ends))) (:bridges g))]
    (and (= (count steps) (count (:bridges g)))
         (apply distinct? (map second steps))
         (every? (fn [[u b v]] (= (ends b) #{u v})) steps)
         (every? (fn [[[_ _ v] [u]]] (= v u)) (partition 2 1 steps)))))

;; ---------------------------------------------------------------------------
;; Graded

(defn- variant [kind diffs]
  (let [res (grade/grade kind diffs)]
    (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails)))

(defn graded
  "[{:label :grade}]: the claims of E53 in the order of his paragraphs, each
   an exact difference that is 0. Euler's route of par. 15 is the witness for
   the counts of par. 5, 8, 11 and 12: its letters are counted, not assumed."
  []
  (let [k-table (euler-table koenigsberg)
        f-table (euler-table fifteen-bridges)
        sum-degrees (fn [g] (reduce + (vals (degrees g))))
        steps (route->steps route-15)
        record (keep #(when (Character/isUpperCase ^char %) (keyword (str %))) route-15)
        seen (frequencies record)
        start (first record)
        f-degrees (degrees fifteen-bridges)
        twice (doubled koenigsberg)]
    [{:label "par. 5: a walk over n bridges is written with n + 1 letters (Euler's route of par. 15: 15 bridges, 16 capitals)"
      :grade (variant :symbolic [(- (count record) (inc (count steps)))])}
     {:label "par. 6: in the record of a Koenigsberg walk AB and AC must each stand twice, AD, BD and CD once"
      :grade (variant :symbolic [(if (= (successions koenigsberg)
                                        {#{:A :B} 2 #{:A :C} 2 #{:A :D} 1 #{:B :D} 1 #{:C :D} 1}) 0 1)])}
     {:label "par. 8: an odd region with d bridges is written (d + 1)/2 times: in the route of par. 15, D (3 bridges) twice and E (5) three times"
      :grade (variant :symbolic [(- (seen :D) (letter-count-from (f-degrees :D) false))
                                 (- (seen :E) (letter-count-from (f-degrees :E) true))])}
     {:label "par. 9: the letters A, B, C, D must appear 3 + 2 + 2 + 2 = 9 times"
      :grade (variant :symbolic [(- (:sum k-table) 9)])}
     {:label "par. 9: seven bridges are recorded by 7 + 1 = 8 letters, one fewer than 9"
      :grade (variant :symbolic [(- (:sum k-table) (:prefixed k-table) 1)])}
     {:label "par. 11-12: an even region is written d/2 times when the walk starts elsewhere: A (8) four times, B and C (4) twice, F (6) three times"
      :grade (variant :symbolic (for [k [:A :B :C :F]] (- (seen k) (letter-count-from (f-degrees k) (= k start)))))}
     {:label "par. 14-15: the table for fifteen bridges: 4 + 2 + 2 + 2 + 3 + 3 = 16 = 15 + 1, so a walk exists, from D or E"
      :grade (variant :symbolic [(- (:sum f-table) (:prefixed f-table))])}
     {:label "par. 15: Euler's route EaFbBcFd...BoElD crosses each of the fifteen bridges once"
      :grade (variant :symbolic [(if (walk? fifteen-bridges steps) 0 1)])}
     {:label "par. 16: the bridges counted at every region add up to twice the bridges (Koenigsberg)"
      :grade (variant :symbolic [(- (sum-degrees koenigsberg) (* 2 (count (:bridges koenigsberg))))])}
     {:label "par. 16: the same for the fifteen bridges of par. 15"
      :grade (variant :symbolic [(- (sum-degrees fifteen-bridges) (* 2 (count (:bridges fifteen-bridges))))])}
     {:label "par. 17: the number of odd regions is even (4 at Koenigsberg, 2 in par. 15)"
      :grade (variant :symbolic [(mod (count (odd-regions koenigsberg)) 2)
                                 (mod (count (odd-regions fifteen-bridges)) 2)])}
     {:label "par. 18: every bridge of Koenigsberg crossed twice: all counts even, the letters one short of the number prefixed, and a walk over all 14 crossings (a' the second crossing of a): A a B a' A b B b' A c C c' A d C d' A e D f B f' D g C g' D e' A"
      :grade (variant :symbolic [(count (odd-regions twice)) (+ (excess twice) 1)
                                 (if (walk? twice twice-walk) 0 1)])}
     {:label "par. 19: the letters exceed the number prefixed by (odd regions)/2 - 1: by 1 at Koenigsberg (4 odd), by 0 in par. 15 (2 odd)"
      :grade (variant :symbolic [(- (excess koenigsberg) 1) (excess fifteen-bridges)])}
     {:label "par. 3, the long way: every walk of Koenigsberg tried, none crosses more than 6 of the 7 bridges"
      :grade (variant :symbolic [(- (longest-trail koenigsberg) 6)])}]))

;; ---------------------------------------------------------------------------
;; The city and its graph, for media

(def city
  "Koenigsberg after E53 Figure 1, in plane coordinates: each region's
   outline and the node it collapses to, each bridge's two feet. The river's
   two branches meet east of the island and flow west."
  {:regions {:A {:node [-0.6 0.0]
                 :outline [[-1.5 -0.35] [-0.4 -0.45] [0.25 -0.25] [0.4 0.0] [0.25 0.25]
                           [-0.4 0.45] [-1.5 0.35] [-1.75 0.0]]}
             :B {:node [-0.3 1.2]
                 :outline [[-2.6 0.8] [1.3 0.8] [1.9 0.95] [2.6 1.0] [2.6 1.6] [-2.6 1.6]]}
             :C {:node [-0.3 -1.2]
                 :outline [[-2.6 -0.8] [1.3 -0.8] [1.9 -0.95] [2.6 -1.0] [2.6 -1.6] [-2.6 -1.6]]}
             :D {:node [1.75 0.0]
                 :outline [[0.95 0.0] [1.3 0.45] [2.6 0.55] [2.6 -0.55] [1.3 -0.45]]}}
   :bridges {:a {:feet [[-1.2 0.38] [-1.2 0.8]]}
             :b {:feet [[-0.65 0.44] [-0.65 0.8]]}
             :c {:feet [[-1.2 -0.38] [-1.2 -0.8]]}
             :d {:feet [[-0.65 -0.44] [-0.65 -0.8]]}
             :e {:feet [[0.4 0.0] [0.95 0.0]]}
             :f {:feet [[1.7 0.5] [1.7 0.92]]}
             :g {:feet [[1.7 -0.5] [1.7 -0.92]]}}})

;; ---------------------------------------------------------------------------
;; The proofs as data

(def proofs-resource "alexandria/euler/opera.proofs.edn")

(defn proof [] (proofs/read-proofs proofs-resource))
