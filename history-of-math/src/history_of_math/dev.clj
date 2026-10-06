(ns history-of-math.dev
  "The series in Clerk's serve mode, live, on http://127.0.0.1:7777 (only
   under the :notebooks alias; README, 'Dev server'):

     (require 'history-of-math.dev)
     (history-of-math.dev/serve! {})

   The pages load the series' bundle (history-of-math.sci-extensions, built
   with history-of-math.bundle's config: the d3-require shim, the vendored
   katex / plotly, the widget CSS) compiled by shadow-cljs in WATCH mode
   into `out-dir`/_data and served by Clerk itself under /_fs/, instead of
   Clerk's CDN viewer (which lacks emmy-viewers' raster kernels and
   alexandria's players). What reloads without a restart:

     notebooks/*.clj           Clerk re-evaluates and shows the saved
                               notebook (Clerk has one document: every open
                               tab follows it)
     alexandria .clj/.cljc     load-file into this JVM, then the notebook
                               last shown is re-evaluated without the cache
     alexandria .cljs/.cljc    the scene namespaces Clerk ships as source:
                               the last notebook is re-shown with them; the
                               bundled ones (widgets/bundled-cljs, emmy-
                               viewers) are recompiled by shadow-cljs, then
                               every open page reloads

   What needs a restart (halt! then serve!, or a new JVM): deps.edn,
   package.json / node_modules, this namespace.

   Unlike the static build, Clerk's viewer.css is the Tailwind CDN script
   (nothing compiled per page)."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [history-of-math.build :as build]
            [history-of-math.bundle :as bundle]
            [history-of-math.widgets :as widgets]
            [mentat.clerk-utils.build :as cu-build]
            [mentat.clerk-utils.build.shadow :as cu-shadow]
            [mentat.clerk-utils.css :as cu-css]
            [nextjournal.beholder :as beholder]
            [nextjournal.clerk :as clerk]
            [nextjournal.clerk.config :as clerk-config]
            [nextjournal.clerk.webserver :as webserver]
            [shadow.cljs.devtools.api :as shadow]
            [shadow.cljs.devtools.server :as shadow-server]
            [history-of-math.links :as links]))

(def defaults
  {:host "127.0.0.1"
   :port 7777
   :browse false
   ;; Clerk watches the notebooks; `watch-sources!` the alexandria roots
   :watch-paths ["notebooks"]
   :source-paths ["../alexandria/src" "../alexandria/media/src"]})

(def out-dir
  "Where the dev bundle, its vendor scripts and the widget CSS are written,
   relative to the project (Clerk serves it as /_fs/<out-dir>/...). Apart
   from the static build's .clerk/shadow-cljs, so a release run in another
   JVM does not write over the watched bundle."
  ".clerk/dev")

(def build-id ::bundle)

(defn- fs-url "Clerk's URL of a project file." [path] (str "/_fs/" path))

;; ---------------------------------------------------------------------------
;; The bundle, in watch mode

(defn reload-browsers!
  "shadow-cljs build hook (stage :flush): after each compile, reload every
   page open on Clerk, so the recompiled bundle is the one running."
  {:shadow.build/stage :flush}
  [build-state & _]
  (doseq [ch @webserver/!clients]
    (try (webserver/send! ch {:type :render-eval :form '(.reload js/location)})
         (catch Exception _)))
  build-state)

(defn bundle-config
  "history-of-math.bundle's build of nss, written to `out-dir`/_data (the
   d3-require shim finds vendor/ beside the _data/ of the script's URL),
   shadow's devtools client off: pages reload whole (`reload-browsers!`)."
  [nss]
  (-> (bundle/config nss)
      (assoc :build-id build-id
             :output-dir (str out-dir "/_data")
             :devtools {:enabled false}
             :build-hooks [(list `reload-browsers!)])))

(defn copy-assets!
  "Copy the vendor scripts and the widget stylesheets under `out-dir`, as
   the static build does under its out-path. Returns the stylesheets' URLs."
  []
  (doseq [[src name] build/vendor]
    (let [f (io/file out-dir "vendor" (str name ".js"))]
      (io/make-parents f)
      (io/copy (io/file "node_modules" src) f)))
  (vec (for [[src published] widgets/css]
         (let [f (io/file out-dir "css" published)]
           (io/make-parents f)
           (io/copy (io/file "node_modules" src) f)
           (fs-url (str out-dir "/css/" published))))))

(defn watch-bundle!
  "Start shadow-cljs (its server and the watch of nss); blocks until the
   first compile is done. Returns the bundle's URL."
  [nss {:keys [npm-install?]}]
  (when npm-install? (cu-shadow/install-npm-deps!))
  (shadow-server/start! (cu-shadow/server-config {}))
  (when-not (shadow/worker-running? build-id)
    (shadow/with-runtime (shadow/watch (bundle-config nss))))
  (fs-url (str out-dir "/_data/main.js")))

;; ---------------------------------------------------------------------------
;; Notebooks: Clerk's list, in series order

(defn notebook-paths
  "The notebooks/*.clj files: the series' first, in its order (by year),
   then the rest (the hub, scratch notebooks) by name. Clerk's :paths-fn,
   so it is read again on every request and a new file appears at once."
  []
  (let [all (->> (.listFiles (io/file build/notebooks-dir))
                 (filter #(str/ends-with? (.getName ^java.io.File %) ".clj"))
                 (map #(str build/notebooks-dir "/" (.getName ^java.io.File %)))
                 sort)
        series (keep (fn [p]
                       (let [nb (build/read-notebook {:path p :text (slurp p)})]
                         (when (and (:ok nb) (:ok (build/validate (:ok nb)))) (:ok nb))))
                     (remove #(= build/index-file (build/file-name %)) all))
        ordered (mapv :path (sort-by (juxt :year :id) series))]
    (into ordered (remove (set ordered)) all)))

;; ---------------------------------------------------------------------------
;; Source changes outside notebooks/

(defonce ^:private !source-watcher (atom nil))
(defonce ^:private lock (Object.))

(defn- last-file [] @@#'nextjournal.clerk/!last-file)

(defn reshow-uncached!
  "Re-evaluate the notebook last shown, Clerk's cache (memory and disk)
   left aside for this one evaluation."
  []
  (when-let [f (last-file)]
    (swap! webserver/!doc dissoc :blob->result)
    (let [cached clerk-config/cache-disabled?]
      (alter-var-root #'clerk-config/cache-disabled? (constantly true))
      (try (clerk/show! {:nextjournal.clerk/skip-throw true} f)
           (catch Exception e (println "dev: re-show failed:" (ex-message e)))
           (finally (alter-var-root #'clerk-config/cache-disabled? (constantly cached)))))))

(defn source-changed!
  "A file under :source-paths changed: load a .clj/.cljc into this JVM,
   then re-show the last notebook (for .cljs too: Clerk reads the scene
   sources it ships again)."
  [{:keys [type path]}]
  (let [p (str path)
        file (.getName (io/file p))]
    (when (and (#{:modify :create} type)
               (re-matches #"(?!\.#).+\.(clj|cljc|cljs)$" file))
      (locking lock
        (println "dev: changed" p)
        (when (re-find #"\.cljc?$" p)
          (try (load-file p)
               (catch Throwable e (println "dev: load-file failed:" (ex-message e)))))
        (reshow-uncached!)))))

(defn watch-sources!
  "Watch paths (the existing directories among them) with `source-changed!`,
   replacing the previous watcher. [] stops it."
  [paths]
  (some-> @!source-watcher beholder/stop)
  (reset! !source-watcher
          (when-let [dirs (seq (filter #(.isDirectory (io/file %)) paths))]
            (apply beholder/watch #(source-changed! %) dirs))))

;; ---------------------------------------------------------------------------
;; Boundary

(defn serve!
  "Serve the notebooks live (see the ns doc), the hub (notebooks/index.clj)
   at /, every link of the series in serve mode (history-of-math.links).
   opts: Clerk's serve! options over `defaults`, plus :cljs-namespaces
   (default history-of-math.build/cljs-namespaces), :source-paths and
   :npm-install? (default false: node_modules is installed, and npm may be
   running for another build in this directory). Returns {:url :bundle}."
  [opts]
  (let [{:keys [host port source-paths] :as opts} (merge defaults opts)
        nss (get opts :cljs-namespaces build/cljs-namespaces)
        js (watch-bundle! nss opts)
        index (str build/notebooks-dir "/" build/index-file)]
    (links/set-mode! :serve)
    (widgets/install!)
    (widgets/register-bundle!)
    (apply cu-css/set-css! (copy-assets!))
    (cu-build/set-viewer-js! js)
    (watch-sources! source-paths)
    (clerk/serve! (cond-> (-> opts
                              (dissoc :cljs-namespaces :source-paths :npm-install?)
                              (assoc :paths-fn `notebook-paths))
                    (.exists (io/file index)) (assoc :index index)))
    (let [url (str "http://" host ":" port "/")]
      (println "history-of-math dev:" url "(bundle" js ")")
      {:url url :bundle js})))

(defn halt!
  "Stop Clerk, its watcher, the source watcher and shadow-cljs."
  []
  (watch-sources! [])
  (clerk/halt-watcher!)
  (clerk/halt!)
  (shadow/stop-worker build-id)
  (shadow-server/stop!))
