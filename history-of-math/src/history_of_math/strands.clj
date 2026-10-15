(ns history-of-math.strands
  "The strands of the series: one timeline per field of mathematics
   (resources/history_of_math/strands.edn).

   A field is a TAG, never a partition: timeline entries (:fields), notebooks
   (:history/fields) and lens cards (:field) name strands by these ids, so
   there is one vocabulary of fields and a work on two strands appears on
   both.

   Collect   `registry`: the EDN resource, validated with malli.
   Promote   `Field`: the malli schema of one strand id, built from the
             registry, used by history-of-math.build (notebook metadata),
             history-of-math.timeline (entries) and history-of-math.lens.
   Boundary  none: colours are the :strand/* roles of alexandria.palette's
             :series palette."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [malli.core :as m]
            [malli.error :as me]))

(def resource "history_of_math/strands.edn")

(def Strand
  [:map {:closed true}
   [:id keyword?]
   [:title [:string {:min 1}]]
   [:scope [:string {:min 1}]]
   [:role qualified-keyword?]])

(def Registry
  [:and
   [:map {:closed true} [:strands [:vector {:min 1} Strand]]]
   [:fn {:error/message "strand ids are unique"}
    (fn [{:keys [strands]}] (apply distinct? (map :id strands)))]])

(defn registry
  "The strand registry read from res (default the series'); throws, with
   malli's explanation, when it is malformed."
  ([] (registry resource))
  ([res]
   (let [url (or (io/resource res) (throw (ex-info (str "Missing " res) {:resource res})))
         v (edn/read-string (slurp url))]
     (when-not (m/validate Registry v)
       (throw (ex-info "strands.edn is invalid" {:explain (me/humanize (m/explain Registry v))})))
     v)))

(def strands "The strands, in registry order." (:strands (registry)))

(def ids "The strand ids, in registry order." (mapv :id strands))

(def by-id "Strand id -> strand." (into {} (map (juxt :id identity)) strands))

(def Field
  "One strand id."
  (into [:enum {:error/message (str "a strand id of " resource)}] ids))

(def Fields
  "A non-empty set of strand ids."
  [:set {:min 1} Field])

(defn title "The strand's title, or the id's name." [id]
  (or (:title (by-id id)) (name id)))
