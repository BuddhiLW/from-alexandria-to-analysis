(ns history-of-math.bundle
  "The shadow-cljs release of the series' browser bundle. Loaded only by
   history-of-math.build under the :notebooks alias (shadow-cljs and
   mentat.clerk-utils are not on the :test classpath).

   The build is mentat.clerk-utils' Clerk viewer build with one change: the
   npm module \"d3-require\" resolves to `d3-require-shim`, which sends the
   two scripts Clerk's render loads at runtime (katex@0.16.4, plotly.js-dist
   @2.15.1) to <out-path>/vendor instead of cdn.jsdelivr.net."
  (:require [mentat.clerk-utils.build.shadow :as cu-shadow]
            [shadow.cljs.devtools.api :as shadow]))

(def d3-require-shim
  "The module the bundle uses for the npm \"d3-require\" (see the file's
   header); its VENDORED names are history-of-math.build/vendor's."
  "js/d3_require_local.js")

(defn config
  "The shadow-cljs build of nss."
  [nss]
  (assoc-in (cu-shadow/clerk-build-config nss)
            [:js-options :resolve "d3-require"]
            {:target :file :file d3-require-shim}))

(defn release!
  "npm install (as mentat.clerk-utils does), then release-compile nss.
   Returns the path of the compiled JS."
  [nss]
  (cu-shadow/install-npm-deps!)
  (shadow/with-runtime
    (shadow/release* (config nss) {}))
  cu-shadow/js-path)
