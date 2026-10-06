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

(defn anchor
  "Anchor attributes for href: a real page load, not Clerk's router."
  [href]
  {:href href :data-ignore-anchor-click "true"})
