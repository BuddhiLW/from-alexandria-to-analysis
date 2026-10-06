(ns alexandria.medium.kernel-oracle
  "Test helper: a figure's raster kernel against its Emmy oracle.

   `deviation` compiles a figure {:f :params :state} with
   alexandria.medium.kernel (raster backend, WebAssembly), runs the kernel's
   batch call in node (V8's real wasm) on the given states and params, and
   returns the largest absolute difference from FnFigure, the same Emmy
   function evaluated on the JVM. A test asserts it under raster's libm
   tolerance: 1e-4 where the figure has trig, tighter where it has none.

   `wasm?` reports whether the module loaded (false would mean the browser
   ran Emmy's :js fallback, which is not what we test). Without node on the
   path both return nil and the caller skips."
  (:require [alexandria.medium.figure :as figure]
            [alexandria.medium.kernel :as kernel]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]))

(def node?
  (delay (try (zero? (:exit (sh/sh "node" "--version")))
              (catch java.io.IOException _ false))))

(defn run-kernel
  "{:wasm bool :points [[x y] ...]} of the kernel of `fig` on `states` at
   `params`, run in node through one batch call.

   Modules over 4 KB (emmy.viewer.raster.glue sync-limit) compile
   asynchronously: until f.ready() is true, f.batch answers null. The
   program therefore runs in an async function that polls f.ready() up to
   200 times 5 ms apart and exits non-zero with a clear message if the
   module never becomes ready."
  [fig params states]
  (let [{:keys [glue fallback]} (kernel/kernel fig)
        program (str "const fb = new Function(" (json/write-str (vec (butlast fallback)))
                     ".join(','), " (json/write-str (last fallback)) ");\n"
                     "const f = new Function('fb', " (json/write-str glue) ")(fb);\n"
                     "(async () => {\n"
                     "  const ready = () => typeof f.ready !== 'function' || f.ready();\n"
                     "  for (let i = 0; i < 200 && !ready(); i++) await new Promise(r => setTimeout(r, 5));\n"
                     "  if (!ready()) { console.error('kernel module not ready after 200 x 5 ms'); process.exit(2); }\n"
                     "  const states = " (json/write-str states) ";\n"
                     "  const xs = new Float64Array(states.flat());\n"
                     "  const res = f.batch(xs, states.length, " (json/write-str params) ");\n"
                     "  if (res == null) { console.error('f.batch returned null although f.ready() is true'); process.exit(3); }\n"
                     "  const out = Array.from(res);\n"
                     "  const d = (f.dims && f.dims.outputs) || 2;\n"
                     "  const pts = []; for (let i = 0; i < states.length; i++) pts.push(out.slice(d*i, d*i+d));\n"
                     "  console.log(JSON.stringify({wasm: typeof f.wasm === 'function' ? f.wasm() : null, points: pts}));\n"
                     "})().catch(e => { console.error(String(e && e.stack || e)); process.exit(1); });\n")
        file (java.io.File/createTempFile "kernel-oracle" ".js")]
    (spit file program)
    (let [{:keys [exit out err]} (sh/sh "node" (str file))]
      (io/delete-file file true)
      (if (zero? exit)
        (json/read-str out :key-fn keyword)
        (throw (ex-info (str "node failed: " err) {:program program}))))))

(defn deviation
  "{:wasm bool :max-error double :nan-states [...]} of fig's kernel against
   its FnFigure, or nil without node. A NaN from the kernel (JSON null, e.g.
   raster's polynomial acos at exactly 1) counts as an infinite error and
   its state is listed under :nan-states."
  ([fig states] (deviation fig (:params fig) states))
  ([fig params states]
   (when @node?
     (let [{:keys [wasm points]} (run-kernel fig params states)
           oracle (figure/points (figure/->FnFigure (:f fig)) params states)
           ;; every output of the kernel, not only the first two: a kernel
           ;; may return several numbers per state (its dims.outputs)
           err (fn [ks os]
                 (if (or (not= (count ks) (count os)) (some nil? ks))
                   ##Inf
                   (reduce max 0.0 (map (fn [a c] (Math/abs (- (double a) c))) ks os))))
           errs (map err points oracle)]
       {:wasm wasm
        :max-error (reduce max 0.0 errs)
        :nan-states (vec (keep (fn [[s e]] (when (= ##Inf e) s)) (map vector states errs)))}))))
