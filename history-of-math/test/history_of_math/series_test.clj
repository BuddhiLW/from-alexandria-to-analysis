(ns history-of-math.series-test
  "The series registry: its schema, its references to strands and
   notebooks, and that every notebook is reachable from the apex."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [history-of-math.apex :as apex]
            [history-of-math.build :as build]
            [history-of-math.lens :as lens]
            [history-of-math.series :as series]
            [history-of-math.strands :as strands]
            [history-of-math.timeline :as timeline]
            [malli.core :as m]))

(defn- site []
  (let [nbs (:ok (build/discover build/notebooks-dir))
        t (:ok (timeline/timeline))]
    {:reg (series/registry) :nbs nbs :joined (timeline/join t nbs)}))

(defn- notebook-files
  "The universe of notebooks, from the files themselves: notebooks/*.clj
   but the apex."
  []
  (->> (.listFiles (io/file build/notebooks-dir))
       (filter #(and (.isFile %) (str/ends-with? (.getName %) ".clj") (not= "index.clj" (.getName %))))
       (map #(build/notebook-id (.getName %)))
       set))

(deftest the-registry-is-valid-and-not-vacuous
  (let [{:keys [reg]} (site)]
    (is (m/validate series/Registry reg))
    (is (<= 12 (count (series/notebook-series reg))) "a series per strand family")
    (is (<= 4 (count (series/decks reg))))
    (is (= :alexandria-to-analysis (:id (first (series/notebook-series reg)))) "the original line comes first")
    (testing "the schema rejects what it should"
      (let [s (first (series/notebook-series reg))]
        (is (not (m/validate series/Series (assoc s :strands [:physics]))) "an unknown strand")
        (is (not (m/validate series/Series (assoc s :strands []))) "no strands")
        (is (not (m/validate series/Series (assoc s :slug "Bad Slug"))))
        (is (not (m/validate series/Series (dissoc s :scope))))
        (is (not (m/validate series/Series {:id :x :kind :deck :title "X" :status :live})) "a live deck needs a url")
        (is (not (m/validate series/Registry (update reg :series conj (assoc s :id :other)))) "slugs unique")
        (is (not (m/validate series/Registry (update reg :series conj s))) "ids unique")))))

(deftest live-decks-have-absolute-https-urls
  (let [live (filter #(= :live (:status %)) (series/decks (series/registry)))]
    (is (<= 3 (count live)) "not vacuous: the live decks are there")
    (doseq [d live]
      (is (str/starts-with? (:url d) "https://") (str (:id d) " url must be absolute: " (:url d))))))

(deftest slugs-are-unique
  (let [slugs (map :slug (series/notebook-series (:reg (site))))]
    (is (= (count slugs) (count (set slugs))))))

(deftest every-series-strand-is-a-registered-strand
  (doseq [{:keys [id strands]} (series/notebook-series (:reg (site)))
          s strands]
    (is (some #{s} strands/ids) (str id " " s))))

(deftest every-explicit-order-names-a-notebook-that-exists
  (let [universe (notebook-files)
        orders (keep :order (series/notebook-series (:reg (site))))]
    (doseq [o orders, id o] (is (contains? universe id) id))
    (testing "an explicit order is honoured, and checked by the same rule"
      (let [{:keys [nbs]} (site)
            s {:id :t :kind :notebooks :slug "t" :title "T" :scope "T." :strands [:geometry]
               :order ["klein-erlangen" "euclid-elements"]}]
        (is (= ["klein-erlangen" "euclid-elements"] (mapv :id (series/members s nbs))))))))

(deftest every-built-notebook-is-on-a-series
  (let [{:keys [reg nbs]} (site)
        universe (notebook-files)
        on (set (mapcat #(map :id (series/members % nbs)) (series/notebook-series reg)))]
    (is (<= 20 (count universe)))
    (is (= universe (set (map :id nbs))) "discovery sees every notebook file")
    (doseq [id universe] (is (contains? on id) (str id " is on no series")))))

(deftest members-follow-the-timeline-by-year
  (let [{:keys [reg nbs joined]} (site)]
    (doseq [s (series/notebook-series reg)
            :let [ms (series/members s nbs)]]
      (testing (:slug s)
        (is (= (map :year ms) (sort (map :year ms))))
        (is (every? #(series/on-strands? s %) ms))
        (is (pos? (count (series/entries s joined))) "a series has a timeline")))
    (let [a2a (first (series/notebook-series reg))
          ids (map :id (series/members a2a nbs))]
      (is (= "doubling-the-cube" (first ids)))
      (is (some #{"euclid-elements"} ids))
      (is (some #{"dedekind-cantor"} ids)))))

(deftest neighbours-walk-a-series
  (let [{:keys [reg nbs]} (site)
        trig (get (series/by-slug reg) "chords-to-fourier")
        ms (series/members trig nbs)]
    (is (<= 3 (count ms)))
    (is (nil? (:prev (series/neighbours ms (:id (first ms))))))
    (is (nil? (:next (series/neighbours ms (:id (last ms))))))
    (is (= (:id (second ms)) (:id (:next (series/neighbours ms (:id (first ms)))))))
    (is (nil? (series/neighbours ms "no-such")))))

(deftest a-notebook-navigation-names-every-series-it-is-on
  (let [{:keys [reg nbs]} (site)
        {:keys [series hub]} (apex/notebook-nav reg nbs "ptolemy-almagest")]
    (is (= "../" hub))
    (is (<= 3 (count series)) "Ptolemy: the original line, trigonometry, the heavens, geometry")
    (is (= "alexandria-to-analysis" (:slug (first series))) "the first series is the default")
    (doseq [{:keys [slug href prev next]} series]
      (is (= (str "../series/" slug "/") href))
      (doseq [l [prev next] :when l]
        (is (str/ends-with? (:href l) (str "/?series=" slug)))
        (is (str/starts-with? (:href l) "../"))))))

(deftest summaries-count-built-and-planned
  (let [{:keys [reg nbs joined]} (site)]
    (doseq [s (series/notebook-series reg)
            :let [{:keys [span built planned first]} (series/summary s nbs joined)]]
      (testing (:slug s)
        (is (<= (clojure.core/first span) (second span)))
        (is (= built (count (series/members s nbs))))
        (is (pos? (+ built planned)))
        (is (= first (clojure.core/first (series/members s nbs))))))))

(deftest lens-tangents-resolve-in-the-one-vocabulary
  (let [reg (lens/series)]
    (is (= :live (get-in reg [:erlangen :status])))
    (is (= "../series/curved-space/" (get-in reg [:curved-space :url])))
    (doseq [id (lens/ids), {:keys [series]} (:tangents (lens/read-lens id))]
      (is (contains? reg series) (str id " " series)))))

(deftest reader-strings-keep-the-language-rules
  (let [{:keys [reg nbs joined]} (site)
        texts (concat (map :title (:series reg)) (keep :scope (:series reg))
                      [(pr-str (apex/cards reg nbs joined)) (pr-str (apex/decks reg))
                       (slurp "notebooks/index.clj") (slurp "notebooks/series/series_page.clj")
                       (slurp "src/history_of_math/series_view.cljs")])]
    (doseq [t texts]
      (is (not (str/includes? t "—")) "no em dash")
      (is (not (re-find #"(?i)this (page|notebook) (shows|lists)" t)) "no page self-reference"))))

(deftest series-pages-and-roots
  (is (= "" (build/root-prefix "out" "out")))
  (is (= "../" (build/root-prefix "out" "out/euler")))
  (is (= "../../" (build/root-prefix "target/x" "target/x/series/curved-space")))
  (is (= "out/series/curved-space" (build/series-out "out" "curved-space"))))
