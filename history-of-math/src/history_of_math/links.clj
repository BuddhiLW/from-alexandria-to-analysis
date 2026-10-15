(ns history-of-math.links
  "Where a link to a page of the series points, in each of the two ways the
   series is served.

     static (history-of-math.build)  each notebook is its own app at
                                     <out>/<id>/, the hub at <out>/: the
                                     hrefs are relative directories
     serve (history-of-math.dev)     Clerk's webserver: a notebook is at
                                     /notebooks/<file stem>, the hub at /

   The mode is a root value, not a thread binding: Clerk evaluates a
   notebook on whatever thread it likes. history-of-math.dev/serve! sets
   :serve; the static build never does. Either way the anchor carries
   data-ignore-anchor-click, so Clerk's router leaves it alone and the
   browser loads the page (a fresh SCI context per notebook)."
  (:require [clojure.string :as str]))

(defonce ^{:doc "The mode of the links: :static (default) or :serve."}
  !mode
  (atom :static))

(defn set-mode!
  "Make mode (:static or :serve) the mode of every link built from now on."
  [mode]
  {:pre [(#{:static :serve} mode)]}
  (reset! !mode mode))

(defn serve? [] (= :serve @!mode))

(defn file-stem
  "The file stem of notebook id (history-of-math.build/notebook-id
   inverted: - as _)."
  [id]
  (str/replace id "-" "_"))

(defn notebook-href
  "The href of notebook id. from is :hub (the page at <out>/) or :notebook
   (a page at <out>/<id>/); it matters only for static relative links."
  [from id]
  (if (serve?)
    (str "/notebooks/" (file-stem id))
    (str (when (= from :notebook) "../") id "/")))

(defn hub-href
  "The href of the hub from a notebook page."
  []
  (if (serve?) "/" "../"))

(defn up
  "The relative path from a page depth directories below the apex back to
   it: \"\" for the apex, \"../\" for a notebook, \"../../\" for a topic."
  [depth]
  (apply str (repeat depth "../")))

(defn apex-href
  "The href of the apex from a page depth directories below it."
  [depth]
  (if (serve?) "/" (let [u (up depth)] (if (= "" u) "./" u))))

(defn topic-href
  "The href of the timeline page of topic slug: <out>/series/<slug>/ in the
   static build, the topic template with ?series=<slug> under Clerk serve."
  [depth slug]
  (if (serve?)
    (str "/notebooks/series/series_page?series=" slug)
    (str (up depth) "series/" slug "/")))

(defn all-href
  "The href of the page with every work on one timeline: <out>/all/ in the
   static build, its notebook under Clerk serve."
  [depth]
  (if (serve?) "/notebooks/series/all" (str (up depth) "all/")))

(defn anchor
  "Anchor attributes for href: a real page load, not Clerk's router."
  [href]
  {:href href :data-ignore-anchor-click "true"})

(defn relation-label
  "The phrase a lens shows for a relation between two notebooks."
  [relation]
  (case relation
    :anticipates "leads to"
    :generalizes "generalizes"
    :proves "proves"
    :refutes "refutes"
    :uses "uses"
    (str/replace (name relation) "-" " ")))
