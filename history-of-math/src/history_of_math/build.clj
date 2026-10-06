(ns history-of-math.build
  "Static Clerk build of the series, one directory per notebook so a
   notebook's URL does not change as notebooks are added:

     clojure -J-Xmx2g -X:notebooks :out-path '\"<dist>/history-of-math\"'

   writes <out-path>/index.html (the hub, notebooks/index.clj) and
   <out-path>/<id>/index.html per notebook.

   Collect   `discover`: the notebooks/*.clj files (behind the Notebooks
             port), each read only up to its first form, the ns form, with
             edamame. Nothing is evaluated.
   Promote   `promote`: each notebook's ns metadata validated with malli
             (:history/year int, negative = BC; :history/title;
             :history/era), ids checked unique, sorted by year.
   Boundary  `build!`: clerk/build! per notebook, then the index.

   A notebook whose metadata is missing or malformed is an Err naming the
   file and the keys; build! throws it, so the build fails loudly.

   Reuse searched: set-theory.branch-build and alexandria.notebooks-build
   (fixed lists of paths, no metadata); Clerk reads ns metadata only while it
   evaluates. edamame (Clerk's own reader) parses the ns form."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [edamame.core :as edamame]
            [hive-dsl.result :as r]
            [malli.core :as m]
            [malli.error :as me]))

;; ---------------------------------------------------------------------------
;; Port: where the notebook files come from

(defprotocol Notebooks
  (files [this] "The notebook files, a seq of {:path string :text string}."))

(def index-file
  "The hub's file name: it is not a history notebook and is never discovered."
  "index.clj")

(defn file-name "The last segment of path." [path] (last (str/split path #"/")))

(defrecord Directory [dir]
  Notebooks
  (files [_]
    (->> (.listFiles (io/file dir))
         (filter #(and (.isFile ^java.io.File %)
                       (str/ends-with? (.getName ^java.io.File %) ".clj")
                       (not= index-file (.getName ^java.io.File %))))
         (sort-by #(.getName ^java.io.File %))
         (map (fn [^java.io.File f] {:path (str dir "/" (.getName f)) :text (slurp f)})))))

(extend-protocol Notebooks
  clojure.lang.Sequential
  (files [xs] (remove #(= index-file (file-name (:path %))) xs)))

(defn directory "The notebooks/*.clj files of dir." [dir] (->Directory dir))

;; ---------------------------------------------------------------------------
;; Collect

(defn notebook-id
  "The id of the notebook at path: its file stem with _ as -."
  [path]
  (-> (file-name path) (str/replace #"\.clj$" "") (str/replace "_" "-")))

(def ^:private reader-opts
  (edamame/normalize-opts
   {:all true
    ;; ::clerk/visibility and friends: the alias stands for itself, the
    ;; metadata we read is fully qualified
    :auto-resolve (fn [alias] (if (= :current alias) 'user alias))
    :readers (fn [_tag] identity)}))

(defn ns-metadata
  "Result of the metadata of the ns form that opens text: the name symbol's
   metadata merged with the attr-map."
  [path text]
  (r/let-ok [form (r/try-effect* :notebook/unreadable
                    (edamame/parse-next (edamame/reader text) reader-opts))]
    (if (and (seq? form) (= 'ns (first form)) (symbol? (second form)))
      (let [[_ nm & more] form
            more (if (string? (first more)) (rest more) more)
            attr (when (map? (first more)) (first more))]
        (r/ok (merge (dissoc (meta nm) :line :column :end-line :end-column) attr)))
      (r/err {:history/error :notebook/no-ns-form :path path}))))

(defn read-notebook
  "Result of {:id :path :year :title :era} read from one file."
  [{:keys [path text]}]
  (r/let-ok [md (ns-metadata path text)]
    (r/ok {:id (notebook-id path)
           :path path
           :year (:history/year md)
           :title (:history/title md)
           :era (:history/era md)})))

;; ---------------------------------------------------------------------------
;; Promote

(def Notebook
  [:map
   [:id [:re #"^[a-z0-9][a-z0-9-]*$"]]
   [:path string?]
   [:year int?]
   [:title [:string {:min 1}]]
   [:era [:string {:min 1}]]])

(def ^:private key-of {:year :history/year :title :history/title :era :history/era})

(defn validate
  "Result of notebook nb, or an Err naming its file and what is missing or
   malformed (as the ns metadata keys)."
  [nb]
  (if (m/validate Notebook nb)
    (r/ok nb)
    (let [explain (me/humanize (m/explain Notebook nb))
          missing (vec (for [k [:year :title :era] :when (nil? (get nb k))] (key-of k)))]
      (r/err {:history/error :notebook/metadata
              :path (:path nb)
              :missing missing
              :invalid (into {} (for [[k v] explain :when (some? (get nb k))] [(key-of k k) v]))}))))

(defn promote
  "Result of the notebooks read from the port, validated and sorted by year
   (then id); the first failure is the Err."
  [notebooks]
  (r/let-ok [nbs (reduce (fn [acc file]
                           (r/let-ok [xs acc
                                      nb (read-notebook file)
                                      nb (validate nb)]
                             (r/ok (conj xs nb))))
                         (r/ok [])
                         (files notebooks))]
    (let [dups (keep (fn [[id n]] (when (> n 1) id)) (frequencies (map :id nbs)))]
      (if (seq dups)
        (r/err {:history/error :notebook/duplicate-id :ids (vec dups)})
        (r/ok (vec (sort-by (juxt :year :id) nbs)))))))

(defn discover
  "Result of the series' notebooks in dir (or from any Notebooks port)."
  [source]
  (promote (if (string? source) (directory source) source)))

(defn error-message
  "One line naming the file and what is wrong."
  [{:keys [path missing invalid] :as err}]
  (case (:history/error err)
    :notebook/metadata
    (str path ": notebook metadata "
         (str/join "; " (cond-> []
                          (seq missing) (conj (str "missing " (str/join ", " missing)))
                          (seq invalid) (conj (str "invalid " (pr-str invalid))))))
    (str (or path "notebooks") ": " (pr-str err))))

(defn neighbours
  "{:prev :next} of the notebook at path among nbs (in series order, as
   `discover` returns them); either is nil at an end, and the whole is nil
   when path is not one of nbs. Matched by notebook id, so any spelling of
   the path to the same file works."
  [nbs path]
  (let [v (vec nbs)
        id (notebook-id path)]
    (when-let [i (first (keep-indexed (fn [i nb] (when (= id (:id nb)) i)) v))]
      {:prev (get v (dec i)) :next (get v (inc i))})))

;; ---------------------------------------------------------------------------
;; Boundary

(def notebooks-dir "notebooks")

(defn build-with
  "Discover the notebooks of port `notebooks`, then call (build-one {:paths
   :out-path}) for the hub at index (when given) and for each notebook into
   out-path/<id>. Throws, naming the file, when a notebook's metadata is
   wrong; nothing is built then. Returns the notebooks built."
  [notebooks index out-path build-one]
  (let [res (discover notebooks)]
    (when-not (r/ok? res)
      (throw (ex-info (error-message (:error res)) (:error res))))
    (when index
      (println "index <-" index)
      (build-one {:paths [index] :out-path out-path}))
    (doseq [{:keys [id path year]} (:ok res)]
      (println "notebook" id (str "(" year ")") "<-" path)
      (build-one {:paths [path] :out-path (str out-path "/" id)}))
    (:ok res)))

;; ---------------------------------------------------------------------------
;; Boundary: the browser bundle

(def cljs-namespaces
  "The ClojureScript compiled into the series' bundle (Clerk's viewer,
   emmy-viewers, alexandria's players)."
  '[history-of-math.sci-extensions])

(defn root-prefix
  "The path from the page built into page-out back to out-path: \"\" for
   the hub, \"../\" for a notebook at out-path/<id>."
  [out-path page-out]
  (if (= out-path page-out) "" "../"))

(def vendor
  "The scripts Clerk's render loads at runtime through d3-require, as
   [file-in-node_modules d3-require-name]: render-katex's katex@0.16.4 and
   render-plotly's plotly.js-dist@2.15.1 (the -min build of the same
   release). Each is copied to <out-path>/vendor/<name>.js; the names are
   the VENDORED list of history-of-math.bundle/d3-require-shim."
  [["clerk-katex/dist/katex.min.js" "katex@0.16.4"]
   ["plotly.js-dist-min/plotly.min.js" "plotly.js-dist@2.15.1"]])

(def viewer-css
  "Clerk's tailwind stylesheet, compiled by `compile-viewer-css!`, relative
   to out-path. Without it Clerk's pages run the Tailwind CDN script."
  "css/viewer.css")

(def favicon "favicon.ico")

(defn release-bundle!
  "Release-compile nss with shadow-cljs (history-of-math.bundle, npm deps from
   ./package.json) and store under out-path the bundle, the widget CSS and
   the `vendor` scripts. Returns {:js \"_data/<hash>.js\" :css [\"css/<f>\"
   ...]}, paths relative to out-path."
  [nss out-path]
  (let [store! (requiring-resolve 'nextjournal.clerk.viewer/store+get-cas-url!)
        css (deref (requiring-resolve 'history-of-math.widgets/css))
        js-path ((requiring-resolve 'history-of-math.bundle/release!) nss)
        js (store! {:out-path out-path :ext "js"}
                   (java.nio.file.Files/readAllBytes (.toPath (io/file js-path))))]
    (io/make-parents (io/file out-path "css" "x"))
    (io/make-parents (io/file out-path "vendor" "x"))
    (doseq [[src name] vendor]
      (io/copy (io/file "node_modules" src) (io/file out-path "vendor" (str name ".js"))))
    {:js js
     :css (vec (for [[src published] css]
                 (do (io/copy (io/file "node_modules" src) (io/file out-path "css" published))
                     (str "css/" published))))}))

(def ^:private !head-links
  "The <link> attribute maps put at the top of the page's head while a
   page is built (the favicon)."
  (atom []))

(defonce ^:private head-links-hook
  (delay
    (alter-var-root (requiring-resolve 'nextjournal.clerk.view/include-viewer-css)
                    (fn [include]
                      (fn [& xs] (concat (map (fn [attrs] [:link attrs]) @!head-links)
                                         (apply include xs)))))))

(defn- with-resource-url
  "Call f with Clerk's resource k (as /css/viewer.css) set to url, then
   restore it."
  [k url f]
  (let [!r @(requiring-resolve 'nextjournal.clerk.config/!resource->url)
        old (get @!r k ::none)]
    (try (swap! !r assoc k url)
         (f)
         (finally (if (= ::none old) (swap! !r dissoc k) (swap! !r assoc k old))))))

(defn with-bundle
  "build-one wrapped so each page loads `bundle` (as release-bundle!
   returns it), the `viewer-css` stylesheet and the `favicon` by paths
   relative to that page, instead of Clerk's CDN viewer and the Tailwind
   CDN script. One bundle, released once, serves the hub and every
   notebook."
  [bundle out-path build-one]
  (let [with-viewer-js (requiring-resolve 'mentat.clerk-utils.build/with-viewer-js)
        set-css! (requiring-resolve 'mentat.clerk-utils.css/set-css!)]
    ((requiring-resolve 'history-of-math.widgets/register-bundle!))
    @head-links-hook
    (fn [{page-out :out-path :as opts}]
      (let [prefix (root-prefix out-path page-out)]
        (apply set-css! (map #(str prefix %) (:css bundle)))
        (reset! !head-links [{:rel "icon" :href (str prefix favicon)}])
        (try
          (with-resource-url "/css/viewer.css" (str prefix viewer-css)
            #(with-viewer-js (str prefix (:js bundle)) (fn [] (build-one opts))))
          (finally (reset! !head-links [])))))))

(defn compile-viewer-css!
  "Compile Clerk's viewer.css (its stylesheets/viewer.css and
   tailwind.config.js, the content globs pointed at the built pages' edn
   and the bundle) with node_modules' tailwindcss into out-path/viewer-css,
   as Clerk's own compile-css! does for one build. Run after every page is
   built, so the classes of all of them are kept. The work directory is
   .clerk/tailwind, where the config's require of @tailwindcss/typography
   finds ./node_modules."
  [out-path]
  (let [dir (io/file ".clerk" "tailwind")
        out (.getAbsolutePath (io/file out-path))
        config (io/file dir "tailwind.config.cjs")
        input (io/file dir "input.css")
        content (str "[" (str/join ", " (map pr-str [(str out "/**/*.edn")
                                                     (str out "/**/*.html")
                                                     (str out "/_data/*.js")]))
                     "]")]
    (.mkdirs dir)
    (spit config (str/replace (slurp (io/resource "stylesheets/tailwind.config.js"))
                              #"content:\s*\[[^\]]*\]" (str "content: " content)))
    (spit input (slurp (io/resource "stylesheets/viewer.css")))
    (io/make-parents (io/file out-path viewer-css))
    (let [{:keys [exit out err]} (shell/sh
                                  "node_modules/.bin/tailwindcss"
                                  "--config" (str config) "--input" (str input)
                                  "--output" (str (io/file out-path viewer-css)) "--minify")]
      (doseq [f (reverse (file-seq dir))] (.delete ^java.io.File f))
      (when-not (zero? exit)
        (throw (ex-info (str "tailwindcss failed\n" out err) {:exit exit})))
      (println "viewer.css <-" (str/trim (str err))))))

(defn write-favicon!
  "Write out-path/favicon: a 32x32 PNG (an ellipse) in an ICO container."
  [out-path]
  (System/setProperty "java.awt.headless" "true")
  (let [img (java.awt.image.BufferedImage. 32 32 java.awt.image.BufferedImage/TYPE_INT_ARGB)
        g (.createGraphics img)
        png (java.io.ByteArrayOutputStream.)]
    (.setRenderingHint g java.awt.RenderingHints/KEY_ANTIALIASING
                       java.awt.RenderingHints/VALUE_ANTIALIAS_ON)
    (.setColor g (java.awt.Color. 0x1f 0x3a 0x5f))
    (.setStroke g (java.awt.BasicStroke. 3.0))
    (.draw g (java.awt.geom.Ellipse2D$Double. 3 8 26 16))
    (.dispose g)
    (javax.imageio.ImageIO/write img "png" png)
    (let [bytes (.toByteArray png)
          head (doto (java.nio.ByteBuffer/allocate 22)
                 (.order java.nio.ByteOrder/LITTLE_ENDIAN)
                 ;; ICONDIR: reserved, type 1 (icon), one image
                 (.putShort (short 0)) (.putShort (short 1)) (.putShort (short 1))
                 ;; ICONDIRENTRY: 32x32, no palette, 1 plane, 32 bpp, size, offset
                 (.put (byte 32)) (.put (byte 32)) (.put (byte 0)) (.put (byte 0))
                 (.putShort (short 1)) (.putShort (short 32))
                 (.putInt (int (alength bytes))) (.putInt (int 22)))]
      (with-open [o (io/output-stream (io/file out-path favicon))]
        (.write o (.array head))
        (.write o bytes)))))

(defn build!
  "Build the hub into out-path/index.html and each notebook of dir into
   out-path/<id>/index.html with Clerk. Every page loads the bundle of
   cljs-namespaces (default `cljs-namespaces`), released once into
   out-path/_data, and every script from out-path: the `vendor` scripts,
   Clerk's `viewer-css` compiled once, the `favicon`. Pass
   :cljs-namespaces [] for Clerk's CDN viewer (and its CDN scripts).
   :only, a collection of notebook ids, builds just those (and the hub)."
  [{:keys [out-path dir only] :or {out-path "public/history-of-math" dir notebooks-dir} :as opts}]
  (let [index (str dir "/" index-file)
        nss (get opts :cljs-namespaces cljs-namespaces)
        clerk-build! (requiring-resolve 'nextjournal.clerk/build!)
        port (if (seq only)
               (filterv #(contains? (set (map str only)) (notebook-id (:path %)))
                        (files (directory dir)))
               (directory dir))]
    (if (seq nss)
      (let [built (build-with port
                              (when (.exists (io/file index)) index)
                              out-path
                              (with-bundle (release-bundle! nss out-path) out-path clerk-build!))]
        (compile-viewer-css! out-path)
        (write-favicon! out-path)
        built)
      (build-with port (when (.exists (io/file index)) index) out-path clerk-build!))))
