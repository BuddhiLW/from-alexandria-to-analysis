(ns history-of-math.build-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-dsl.result :as r]
            [history-of-math.build :as build]
            [history-of-math.timeline :as timeline]))

;; ---------------------------------------------------------------------------
;; The notebooks of the series, read through the Directory port

(def series (build/directory build/notebooks-dir))

(deftest every-notebook-declares-year-title-and-era
  (let [fs (build/files series)]
    (is (seq fs) "notebooks/ holds at least one history notebook")
    (is (not-any? #(= build/index-file (build/file-name (:path %))) fs) "the hub is not discovered")
    (doseq [f fs]
      (testing (:path f)
        (let [res (r/let-ok [nb (build/read-notebook f)] (build/validate nb))]
          (is (r/ok? res) (some-> (:error res) build/error-message)))))))

(deftest series-ids-are-unique-and-sorted-by-year
  (let [res (build/discover build/notebooks-dir)
        nbs (:ok res)]
    (is (r/ok? res) (some-> (:error res) build/error-message))
    (is (= (count nbs) (count (distinct (map :id nbs)))))
    (is (= (map :year nbs) (sort (map :year nbs))))))

(deftest the-greeks-are-mounted
  (let [ids (set (map :id (:ok (build/discover build/notebooks-dir))))]
    (is (contains? ids "euclid-elements"))
    (is (contains? ids "archimedes"))))

;; ---------------------------------------------------------------------------
;; Discovery on a fixture: the port is a seq of {:path :text}

(defn- nb [path attrs]
  {:path path
   :text (str "^{:nextjournal.clerk/visibility {:code :hide}}\n(ns " (str/replace (build/notebook-id path) #"\.clj$" "")
              " " (pr-str attrs) "\n  (:require [nextjournal.clerk :as clerk]))\n\n(clerk/md \"never evaluated\")\n")})

(def fixture
  [(nb "nb/newton_principia.clj" {:history/year 1687 :history/title "Newton, Principia" :history/era "Calculus"
                                  :nextjournal.clerk/toc true})
   (nb "nb/euclid_i.clj" {:history/year -300 :history/title "Euclid" :history/era "Greek"})
   {:path "nb/index.clj" :text "(ns index)"}
   {:path "nb/leibniz.clj"
    :text "(ns ^{:history/year 1684 :history/title \"Leibniz, Nova methodus\"} leibniz\n  \"doc\"\n  {:history/era \"Calculus\"})"}])

(deftest discovery-reads-ns-metadata-and-sorts-chronologically
  (let [res (build/discover fixture)]
    (is (r/ok? res))
    (is (= [["euclid-i" -300 "Greek"] ["leibniz" 1684 "Calculus"] ["newton-principia" 1687 "Calculus"]]
           (map (juxt :id :year :era) (:ok res))))
    (is (= "Leibniz, Nova methodus" (:title (second (:ok res)))) "name metadata and attr-map merge")))

(deftest a-notebook-missing-metadata-is-an-err-naming-file-and-keys
  (let [bad (nb "nb/oresme.clj" {:history/title "Oresme"})
        res (build/discover (conj fixture bad))
        err (:error res)]
    (is (not (r/ok? res)))
    (is (= :notebook/metadata (:history/error err)))
    (is (= "nb/oresme.clj" (:path err)))
    (is (= [:history/year :history/era] (:missing err)))
    (is (str/includes? (build/error-message err) "nb/oresme.clj"))
    (is (str/includes? (build/error-message err) ":history/year"))))

(deftest a-malformed-year-is-invalid-not-missing
  (let [err (:error (build/discover [(nb "nb/x.clj" {:history/year "300 BC" :history/title "X" :history/era "E"})]))]
    (is (= [] (:missing err)))
    (is (contains? (:invalid err) :history/year))))

(deftest a-file-without-an-ns-form-is-an-err
  (is (= :notebook/no-ns-form
         (:history/error (:error (build/discover [{:path "nb/plain.clj" :text "(def x 1)"}]))))))

(deftest duplicate-ids-are-an-err
  (let [a (nb "a/euclid.clj" {:history/year -300 :history/title "E" :history/era "G"})
        b (nb "b/euclid.clj" {:history/year -300 :history/title "E" :history/era "G"})]
    (is (= {:history/error :notebook/duplicate-id :ids ["euclid"]}
           (:error (build/discover [a b]))))))

(deftest build-builds-hub-then-notebooks-in-order
  (let [calls (atom [])
        built (build/build-with fixture "nb/index.clj" "out" #(swap! calls conj %))]
    (is (= 3 (count built)))
    (is (= [{:paths ["nb/index.clj"] :out-path "out"}
            {:paths ["nb/euclid_i.clj"] :out-path "out/euclid-i"}
            {:paths ["nb/leibniz.clj"] :out-path "out/leibniz"}
            {:paths ["nb/newton_principia.clj"] :out-path "out/newton-principia"}]
           @calls))))

(deftest build-fails-loudly-naming-the-file-and-builds-nothing
  (let [calls (atom [])]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"nb/oresme.clj: notebook metadata missing :history/year, :history/title, :history/era"
                          (build/build-with (conj fixture (nb "nb/oresme.clj" {})) "nb/index.clj" "out"
                                            #(swap! calls conj %))))
    (is (empty? @calls))))

;; ---------------------------------------------------------------------------
;; The timeline

(deftest timeline-validates
  (let [res (timeline/timeline)]
    (is (r/ok? res) (pr-str (:error res)))
    (is (<= 20 (count (:entries (:ok res)))))))

(deftest a-timeline-entry-on-an-unknown-era-is-invalid
  (is (not (r/ok? (timeline/validate {:title "t" :eras [{:id :a :title "A" :role :era/a}]
                                      :entries [{:year 1 :figure "F" :work "W" :significance "S" :era :b}]})))))

(deftest join-links-built-notebooks-and-plans-the-rest
  (let [t (:ok (timeline/timeline))
        joined (timeline/join t [{:id "euclid-elements" :year -300 :title "Euclid, Elements"}
                                 {:id "newton-principia" :year 1687 :title "Newton, Principia"}])
        by-figure (group-by :figure joined)]
    (is (:built? (first (by-figure "Euclid"))))
    (is (= "newton-principia" (:notebook (first (by-figure "Newton")))) "joined by year and figure")
    (is (not (:built? (first (by-figure "Lebesgue")))))))

(deftest the-strip-draws-one-mark-per-entry-and-links-only-built-ones
  (let [t (:ok (timeline/timeline))
        joined (timeline/join t [{:id "euclid-elements" :year -300 :title "Euclid, Elements"}])
        svg (timeline/strip t joined)
        nodes (tree-seq vector? seq svg)
        links (filter #(and (vector? %) (= :a (first %))) nodes)]
    (is (= :svg (first svg)))
    (is (= ["euclid-elements/"] (map (comp :href second) links)))
    (is (every? #(= "true" (:data-ignore-anchor-click (second %))) links)
        "a link to another notebook's static app is a page load, not a Clerk route")
    (is (= "300 BC" (timeline/year-label {:year -300})))
    (is (= "c. 1350" (timeline/year-label {:year 1350 :circa true})))))

;; ---------------------------------------------------------------------------
;; Previous / next: the neighbours of a notebook in the build's order

(deftest neighbours-follow-the-chronological-order-of-discovery
  (let [nbs (:ok (build/discover fixture))]
    (is (= [nil "leibniz"] (map :id ((juxt :prev :next) (build/neighbours nbs "nb/euclid_i.clj")))))
    (is (= ["euclid-i" "newton-principia"]
           (map :id ((juxt :prev :next) (build/neighbours nbs "nb/leibniz.clj")))))
    (is (= ["leibniz" nil] (map :id ((juxt :prev :next) (build/neighbours nbs "nb/newton_principia.clj")))))
    (is (nil? (build/neighbours nbs "nb/index.clj")) "the hub has no neighbours")))

(deftest every-series-notebook-has-a-neighbour-and-the-ends-are-the-ends
  (let [nbs (:ok (build/discover build/notebooks-dir))]
    (doseq [{:keys [path]} nbs]
      (testing path
        (let [{:keys [prev next]} (build/neighbours nbs path)]
          (is (or prev next)))))
    (is (nil? (:prev (build/neighbours nbs (:path (first nbs))))))
    (is (nil? (:next (build/neighbours nbs (:path (last nbs))))))
    (is (= "archimedes" (:id (:next (build/neighbours nbs "notebooks/euclid_elements.clj")))))))

(deftest every-series-notebook-ends-with-its-own-prev-next-links
  (doseq [{:keys [path text]} (build/files series)]
    (testing path
      (is (str/includes? text (str "(page/prev-next \"notebooks/" (build/file-name path) "\")"))
          "the notebook passes its own path to page/prev-next"))))

;; ---------------------------------------------------------------------------
;; The scripts served from out-path/vendor and the d3-require shim agree

(deftest vendored-names-match-the-d3-require-shim
  (let [shim (slurp "js/d3_require_local.js")
        listed (some->> (re-find #"var VENDORED = \[([^\]]*)\]" shim) second
                        (re-seq #"\"([^\"]+)\"") (map second) set)]
    (is (= listed (set (map second build/vendor)))
        "every vendor script is a name the shim sends to vendor/, and no other")
    (is (every? #(.exists (java.io.File. "node_modules" %)) (map first build/vendor))
        "npm install has put each vendor script in node_modules")))
