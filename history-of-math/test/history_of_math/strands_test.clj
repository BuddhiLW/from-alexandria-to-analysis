(ns history-of-math.strands-test
  "The strands: one vocabulary of fields shared by the registry, the
   timeline, the notebooks and the lenses, and one timeline per strand."
  (:require [clojure.test :refer [deftest is testing]]
            [alexandria.palette :as palette]
            [hive-dsl.result :as r]
            [history-of-math.build :as build]
            [history-of-math.lens :as lens]
            [history-of-math.strands :as strands]
            [history-of-math.timeline :as timeline]
            [malli.core :as m]))

(def the-fourteen
  "Pedro's cut, 2026-10-12: every subject he named has its own strand."
  [:geometry :differential-geometry :analysis :measure-theory :trigonometry
   :algebra :abstract-algebra :number-theory :probability :topology
   :graph-theory :set-theory :logic-computing :mechanics-physics])

(def retired
  "Ids of the nine-strand cut, split since: none may survive anywhere."
  [:topology-combinatorics :logic-foundations-computing])

(defn- series []
  (let [t (:ok (timeline/timeline))
        nbs (:ok (build/discover build/notebooks-dir))]
    {:t t :nbs nbs :joined (timeline/join t nbs)}))

(deftest the-registry-names-fourteen-strands-each-with-its-own-colour
  (is (m/validate strands/Registry (strands/registry)))
  (is (= the-fourteen strands/ids))
  (let [pal (palette/palette :series)
        colours (map #(get pal (:role %)) strands/strands)]
    (is (apply distinct? colours) "no two strands share a colour")
    (doseq [{:keys [id role scope]} strands/strands]
      (testing id
        (is (string? (get pal role)) "the role is a colour of the :series palette")
        (is (re-find #"\.$" scope) "the scope is one sentence")))
    (doseq [old retired]
      (is (nil? (get pal (keyword "strand" (name old)))) (str "no palette role for " old)))))

(deftest the-ids-of-earlier-branches-stay-valid
  (doseq [fs [#{:trigonometry :geometry} #{:algebra :geometry} #{:probability}]]
    (is (m/validate strands/Fields fs) (str fs))))

(deftest the-retired-ids-are-gone-everywhere
  (let [{:keys [t nbs]} (series)]
    (doseq [old retired]
      (testing old
        (is (not (m/validate strands/Fields #{old})) "the registry rejects it")
        (is (not (m/validate lens/Field old)) "a lens card rejects it")
        (is (not-any? #(contains? (:fields %) old) (:entries t)) "no timeline entry carries it")
        (is (not-any? #(contains? (:fields %) old) nbs) "no notebook carries it")
        (is (not-any? #(= old (:field %)) (mapcat #(:fields (lens/read-lens %)) (lens/ids)))
            "no lens card carries it")
        (is (not (r/ok? (timeline/validate (assoc-in t [:entries 0 :fields] #{old}))))
            "an entry on it is invalid")))))

(deftest works-on-several-strands-carry-each
  (let [{:keys [t]} (series)
        fields-of (fn [figure year work-prefix]
                    (:fields (first (filter #(and (= figure (:figure %)) (= year (:year %))
                                                  (clojure.string/starts-with? (:work %) work-prefix))
                                            (:entries t)))))]
    (is (every? (fields-of "Gauss" 1827 "Disquisitiones generales") [:differential-geometry :geometry]))
    (is (every? (fields-of "Gauss" 1801 "Disquisitiones arithmeticae") [:number-theory]))
    (is (every? (fields-of "Riemann" 1854 "Über die Hypothesen") [:geometry :differential-geometry]))
    (is (every? (fields-of "Cantor" 1874 "Über eine Eigenschaft") [:set-theory :analysis]))
    (is (every? (fields-of "Galois" 1832 "Mémoire") [:abstract-algebra :algebra]))
    (is (every? (fields-of "Klein" 1872 "Vergleichende") [:abstract-algebra :geometry :algebra]))
    (is (every? (fields-of "Euler" 1736 "Solutio problematis") [:graph-theory :topology]))))

(deftest every-planned-line-of-the-new-strands-is-on-the-timeline
  (let [{:keys [t]} (series)
        on? (fn [strand figure year]
              (some #(and (= year (:year %)) (clojure.string/includes? (:figure %) figure)
                          (contains? (:fields %) strand))
                    (:entries t)))]
    (doseq [[strand line] {:differential-geometry [["Euler" 1767] ["Monge" 1807] ["Gauss" 1827] ["Riemann" 1854]
                                                   ["Beltrami" 1868] ["Christoffel" 1869] ["Ricci" 1900]
                                                   ["Levi-Civita" 1917] ["Einstein" 1916]]
                           :measure-theory [["Jordan" 1892] ["Borel" 1898] ["Lebesgue" 1902] ["Lebesgue" 1904]
                                            ["Vitali" 1905] ["Radon" 1913] ["Carathéodory" 1914]
                                            ["Hausdorff" 1914] ["Kolmogorov" 1933]]
                           :abstract-algebra [["Galois" 1832] ["Cayley" 1854] ["Jordan" 1870] ["Dedekind" 1871]
                                              ["Dedekind" 1894] ["Weber" 1895] ["Frobenius" 1896]
                                              ["Steinitz" 1910] ["Noether" 1921]]
                           :graph-theory [["Pascal" 1654] ["Leibniz" 1666] ["Euler" 1736] ["Kirchhoff" 1847]
                                          ["Cayley" 1857] ["Hamilton" 1857] ["Kempe" 1879] ["Heawood" 1890]]
                           :set-theory [["Cantor" 1874] ["Cantor" 1883] ["Cantor" 1891] ["Cantor" 1895]
                                        ["Dedekind" 1888] ["Zermelo" 1904] ["Zermelo" 1908] ["Hausdorff" 1914]]
                           :algebra [["al-Khwarizmi" 820] ["Cardano" 1545] ["Abel" 1824]]}
            [figure year] line]
      (is (on? strand figure year) (str strand " " figure " " year)))))

(deftest fields-are-a-non-empty-set-of-strand-ids
  (is (m/validate strands/Fields #{:geometry}))
  (is (not (m/validate strands/Fields #{})) "the empty set tags nothing")
  (is (not (m/validate strands/Fields #{:physics})) "a field outside the registry")
  (is (not (m/validate strands/Fields [:geometry])) "a set, not a vector"))

(deftest every-timeline-entry-is-on-a-registered-strand
  (let [{:keys [t]} (series)]
    (is (<= 90 (count (:entries t))) "the planned line of every strand is on the timeline")
    (doseq [{:keys [figure year fields]} (:entries t)]
      (testing (str figure " " year)
        (is (seq fields))
        (is (every? (set strands/ids) fields))))
    (is (r/ok? (timeline/validate (update-in t [:entries 0] dissoc :fields)))
        "the schema leaves :fields to the series (trivium names arts); this test holds this series to it")
    (is (not (r/ok? (timeline/validate
                     (assoc-in t [:entries 0 :fields] #{:physics}))))
        "an entry on an unknown strand is invalid")))

(deftest every-notebook-declares-its-strands
  (let [{:keys [nbs]} (series)]
    (is (<= 20 (count nbs)))
    (doseq [{:keys [id fields]} nbs]
      (testing id (is (m/validate strands/Fields fields))))
    (let [bad {:path "nb/x.clj" :text "(ns x {:history/year 1 :history/title \"X\" :history/era \"E\" :history/fields #{:physics}})"}
          err (:error (build/discover [bad]))]
      (is (contains? (:invalid err) :history/fields) "an unknown strand on a notebook is invalid"))
    (let [bare {:path "nb/x.clj" :text "(ns x {:history/year 1 :history/title \"X\" :history/era \"E\"})"}]
      (is (r/ok? (build/discover [bare]))
          "the build leaves :history/fields to the series (trivium names arts); this test holds this series to it"))))

(deftest lens-fields-speak-the-strand-vocabulary
  (doseq [id (lens/ids)
          {:keys [field]} (:fields (lens/read-lens id))]
    (testing id (is (some #{field} strands/ids))))
  (is (not (m/validate lens/Field :physics)) "the old lens vocabulary is gone"))

(deftest every-strand-has-its-own-timeline-with-built-and-planned-works
  (let [{:keys [joined]} (series)]
    (doseq [id strands/ids
            :let [line (timeline/strand joined id)]]
      (testing id
        (is (<= 3 (count line)) "a strand shows a line, not a point")
        (is (= (map :year line) (sort (map :year line))) "in order of year")
        (is (some (complement :built?) line) "the rest of the line is planned")))
    (testing "the strands that exist today link their notebooks"
      (doseq [id [:geometry :analysis :trigonometry :mechanics-physics :algebra :number-theory]]
        (is (some :built? (timeline/strand joined id)) (str id))))
    (testing "a tag, never a partition: works on several strands appear on each"
      (let [multi (filter #(< 1 (count (:fields %))) joined)]
        (is (<= 30 (count multi)))
        (doseq [{:keys [fields] :as e} multi, f fields]
          (is (some #{e} (timeline/strand joined f))))))
    (is (< (count joined) (reduce + (map #(count (timeline/strand joined %)) strands/ids)))
        "the strand timelines overlap")))

(deftest a-planned-work-joins-a-notebook-once-built
  (let [{:keys [t]} (series)
        joined (timeline/join t [{:id "viete-isagoge" :year 1591 :title "Viète, In artem analyticem isagoge" :fields #{:algebra}}])
        viete (first (filter #(and (= "Viète" (:figure %)) (= 1591 (:year %))) joined))]
    (is (:built? viete))
    (is (= "viete-isagoge" (:notebook viete)))))

(deftest the-hub-data-carries-strands-chips-and-bridges
  (let [{:keys [t joined]} (series)
        data (timeline/interactive-data t joined)]
    (is (= strands/ids (mapv :id (:strands data))))
    (is (every? pos? (map :count (:strands data))) "every strand has at least one entry")
    (is (every? number? (map :x (:strands data))))
    (doseq [{:keys [fields rows] :as n} (:nodes data)]
      (is (= (set (cons :all fields)) (set (keys rows))) (:figure n))
      (is (every? number? (vals rows))))
    (testing "rows on a strand are consecutive from the top"
      (let [ys (sort (keep #(get-in % [:rows :probability]) (:nodes data)))]
        (is (= (count ys) (count (distinct ys))))
        (is (= (first ys) (:y (first (:nodes data)))))))
    (testing "bridges: lens links between works on different strands"
      (is (seq (:bridges data)))
      (is (some #(= #{:geometry :mechanics-physics} (set ((juxt :from :to) %))) (:bridges data))
          "Apollonius' conics -> Kepler's orbits")
      (doseq [{:keys [from to count notes x y target-x target-y]} (:bridges data)]
        (is (not= from to))
        (is (every? (set strands/ids) [from to]))
        (is (pos? count))
        (is (seq notes))
        (is (every? number? [x y target-x target-y]))))
    (testing "the strands sit on a ring inside the bridge map's frame"
      (let [{:keys [width height cx cy r]} (:ring data)]
        (doseq [{:keys [id x y]} (:strands data)]
          (is (< (Math/abs (- r (Math/hypot (- x cx) (- y cy)))) 1e-6) (str id " on the circle"))
          (is (and (< 0 x width) (< 0 y height)) (str id " inside the frame")))))))
