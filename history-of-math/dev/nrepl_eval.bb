#!/usr/bin/env bb
;; Evaluate a file of Clojure forms in a running nREPL and print out, err and
;; values: bb dev/nrepl_eval.bb <port> <file>. For an inner loop
;; when no editor eval bridge is available.
(require '[bencode.core :as b])
(import '[java.net Socket])

(let [[port file] *command-line-args*
      s (Socket. "localhost" (Integer/parseInt port))
      out (.getOutputStream s)
      in (java.io.PushbackInputStream. (.getInputStream s))
      id (str (random-uuid))
      txt (fn [x] (if (bytes? x) (String. ^bytes x "UTF-8") x))]
  (b/write-bencode out {"op" "eval" "code" (slurp file) "id" id})
  (loop []
    (let [m (b/read-bencode in)
          get* (fn [k] (some-> (get m k) txt))]
      (some-> (get* "out") print)
      (some-> (get* "err") print)
      (some->> (get* "value") (println "=>"))
      (some->> (get* "ex") (println "EX:"))
      (flush)
      (when-not (some #(= "done" (txt %)) (get m "status"))
        (recur)))))
