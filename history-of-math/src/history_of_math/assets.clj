(ns history-of-math.assets
  "Classpath-backed media links that work in the live Clerk server and in
   its self-contained static pages. Only use for small published assets."
  (:require [clojure.java.io :as io]))

(defn data-url
  "Embed one published classpath resource in a Clerk page. The data URL
   survives both the live server and a static build without a server route."
  [file mime]
  (let [path (str "history_of_math/design/" file)
        resource (or (io/resource path)
                     (throw (ex-info "Missing published design asset" {:path path})))
        bytes (with-open [in (io/input-stream resource)] (.readAllBytes in))]
    (str "data:" mime ";base64,"
         (.encodeToString (java.util.Base64/getEncoder) bytes))))
