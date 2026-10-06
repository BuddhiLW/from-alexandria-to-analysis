(ns history-of-math.widgets
  "The widgets of the series (Leva panels, Mafs and MathBox plots from
   emmy-viewers) on the raster backend. A notebook calls, in a hidden form
   at its top,

     ^{::clerk/visibility {:code :hide :result :hide}} (hom/install!)

   which installs emmy-viewers' Clerk viewers and makes :raster the backend
   of every emmy-viewers compile: Mafs of-x / parametric and MathBox of-xy /
   parametric-surface, with or without ev/with-params, then ship a wasm
   kernel (base64 inside the fragment) with Emmy's :js function only as the
   fallback for a browser without WebAssembly.

   The seam is emmy.viewer.compile/*backend* (PR mentat-collective/
   emmy-viewers#83). The viewers compile when the notebook form is
   evaluated, on whatever thread Clerk uses, so the backend is set as the
   var's root value rather than by a thread-local binding. A form that
   needs Emmy's JS compile (an operator raster cannot lower) still binds
   :js around itself.

   The JS side of these viewers is the bundle history-of-math.build
   compiles (history-of-math.sci-extensions); `css` names the stylesheets
   it needs, which the build copies from node_modules."
  (:require [emmy.clerk :as ec]
            [emmy.viewer.compile :as vc]
            [emmy.viewer.raster]))

(def backend
  "The computational backend of the series' plots."
  :raster)

(defn use-raster!
  "Make `backend` the root value of emmy.viewer.compile/*backend*. Returns
   the backend now in effect."
  []
  (alter-var-root #'vc/*backend* (constantly backend))
  vc/*backend*)

(defn install!
  "Install emmy-viewers' Clerk viewers (plus any `viewers`) and the raster
   backend. Call it at the top of every notebook with widgets."
  [& viewers]
  (use-raster!)
  (apply ec/install! viewers))

(def bundled-cljs
  "The namespaces history-of-math.sci-extensions compiles into the bundle
   (besides emmy-viewers'). Clerk must not ship their sources again."
  '#{alexandria.medium.anim alexandria.medium.figure alexandria.medium.html-labels alexandria.medium.math
     alexandria.medium.plane alexandria.medium.player alexandria.medium.scene
     alexandria.medium.svg alexandria.medium.timeline alexandria.palette})

(defn register-bundle!
  "Tell Clerk that the namespaces of `bundled-cljs` are already in the
   browser's SCI context, so a :require-cljs viewer ships only what the
   bundle lacks (a notebook's scene namespace). Only for pages that load
   the bundle; history-of-math.build calls it."
  []
  (swap! @(requiring-resolve 'nextjournal.clerk.cljs-libs/already-loaded-sci-namespaces)
         into bundled-cljs))

(def css
  "The widget stylesheets, as [file-in-node_modules published-name]: Mafs'
   core, MathBox, JSXGraph (the versions of package.json). Mafs' font.css
   is left out: it only @imports the Computer Modern webfont, which is not
   shipped, and Mafs falls back to the page's serif."
  [["mafs/core.css" "mafs-core.css"]
   ["mathbox/build/mathbox.css" "mathbox.css"]
   ["jsxgraph/distrib/jsxgraph.css" "jsxgraph.css"]])
