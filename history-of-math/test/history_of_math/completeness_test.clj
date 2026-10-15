(ns history-of-math.completeness-test
  "Every built notebook is reachable from the apex. The universe is the
   notebook FILES on disk, never the registry, so a notebook nobody
   registered fails here instead of vanishing. Reachable means: a member
   of, or a timeline entry of, at least one /series/<slug>/ page, or named
   in `exempt` with the reason it has no series."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [history-of-math.build :as build]
            [history-of-math.series :as series]
            [history-of-math.timeline :as timeline]))

(def exempt
  "Notebook id -> why it is on no series page. The apex links it directly."
  {"greek-closing" "closing page of the Greek notebooks: linked from the apex (notebooks/index.clj), not a series entry"})

(defn- universe
  "Notebook ids from the files: notebooks/*.clj but the apex."
  []
  (->> (.listFiles (io/file build/notebooks-dir))
       (filter #(and (.isFile %) (str/ends-with? (.getName %) ".clj")
                     (not= "index.clj" (.getName %))))
       (map #(build/notebook-id (.getName %)))
       set))

(defn- reachable
  "Ids on at least one notebook series page, as member or timeline entry."
  []
  (let [nbs (:ok (build/discover build/notebooks-dir))
        reg (series/registry)
        joined (timeline/join (:ok (timeline/timeline)) nbs)]
    (set (mapcat (fn [s] (concat (map :id (series/members s nbs))
                                 (keep :notebook (series/entries s joined))))
                 (series/notebook-series reg)))))

(deftest every-notebook-file-is-reachable-or-exempt
  (let [files (universe)
        on (reachable)]
    (testing "non-vacuity: the file walk found the notebooks"
      (is (seq files))
      (is (<= 30 (count files)) "the walk sees the whole shelf, not a stub")
      (is (every? files ["galois" "noether-1918" "hilbert-grundlagen"
                         "goedel-turing" "alhazen-optics"])))
    (doseq [id (sort files)]
      (is (or (contains? on id) (contains? exempt id))
          (str id " is on no /series/<slug>/ page and has no exemption")))
    (testing "exemptions are real, reasoned, and linked from the apex"
      (let [apex (slurp (io/file build/notebooks-dir "index.clj"))]
        (doseq [[id why] exempt]
          (is (contains? files id) (str id " is exempt but has no file"))
          (is (not (str/blank? why)))
          (is (str/includes? apex (str "\"" id "\"")) (str id " is not linked from the apex")))))))

(deftest new-era-notebooks-sit-on-their-topic-timelines
  (let [nbs (:ok (build/discover build/notebooks-dir))
        reg (series/registry)
        joined (timeline/join (:ok (timeline/timeline)) nbs)
        slugs-of (fn [id] (set (for [s (series/notebook-series reg)
                                     :when (some #{id} (keep :notebook (series/entries s joined)))]
                                 (:slug s))))]
    (is (contains? (slugs-of "galois") "equations-to-structures"))
    (is (contains? (slugs-of "noether-1918") "equations-to-structures"))
    (is (contains? (slugs-of "noether-1918") "law-of-nature"))
    (is (contains? (slugs-of "hilbert-grundlagen") "figures-and-groups"))
    (is (contains? (slugs-of "hilbert-grundlagen") "proof-and-machine"))
    (is (contains? (slugs-of "goedel-turing") "proof-and-machine"))
    (is (contains? (slugs-of "alhazen-optics") "figures-and-groups"))))
