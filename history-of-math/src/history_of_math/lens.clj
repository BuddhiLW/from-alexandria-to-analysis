(ns history-of-math.lens
  "Validated modern readings of each historical notebook; content is EDN,
   presentation belongs here. No mathematical computation lives in the lens."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [history-of-math.links :as links]
            [malli.core :as m]
            [malli.error :as me]
            [nextjournal.clerk :as clerk]
            [history-of-math.strands :as strands]))

(def Field
  "A lens card's field: a strand id of strands.edn, the one vocabulary of
   fields (history-of-math.strands). :label names the card more narrowly
   (Astronomy, Computing) when the strand is broader than the card."
  strands/Field)
(def Lens
  [:map {:closed true}
   [:now [:vector [:map {:closed true} [:claim :string] [:modern :string] [:tex :string]]]]
   [:fields [:vector [:map {:closed true} [:field Field] [:label {:optional true} [:string {:min 1}]]
                      [:how :string] [:example :string]]]]
   [:tangents [:vector [:map {:closed true} [:series :keyword] [:hook :string]
                        [:link {:optional true} :string]]]]
   [:texts [:vector [:map {:closed true} [:author :string] [:work :string]
                     [:year [:or :int :string]] [:url [:re #"^https://[^ ]+$"]]
                     [:kind [:enum :wikisource :commons-scan :archive :translation]]
                     [:lang :string]]]]
   [:links [:vector [:map {:closed true} [:to [:re #"^[a-z0-9][a-z0-9-]*$"]]
                     [:relation [:enum :anticipates :generalizes :proves :refutes :uses]]
                     [:note :string]]]]])

(defn series [] (edn/read-string (slurp (io/resource "history_of_math/series.edn"))))
(defn ids [] (->> (io/file "resources/history_of_math/lens") .listFiles
                  (filter #(str/ends-with? (.getName %) ".edn"))
                  (map #(str/replace (.getName %) #"\.edn$" "")) sort vec))
(defn read-lens [id]
  (when-let [resource (io/resource (str "history_of_math/lens/" id ".edn"))]
    (let [v (edn/read-string (slurp resource))]
      (when-not (m/validate Lens v)
        (throw (ex-info (str "Invalid lens " id) {:id id :errors (me/humanize (m/explain Lens v))})))
      v)))

(defn validate-refs [id lens notebook-ids]
  (let [known-series (set (keys (series)))]
    (concat (for [{:keys [to]} (:links lens) :when (not (contains? notebook-ids to))]
              {:id id :unknown-notebook to})
            (for [{:keys [series]} (:tangents lens) :when (not (contains? known-series series))]
              {:id id :unknown-series series}))))

(defn- card [title body]
  [:div {:style {:border "1px solid #71819566" :border-radius "12px" :padding "1em"
                 :background "#182b4133" :min-width "220px" :flex "1 1 260px"}}
   [:strong title] [:p body]])

(defn section-hiccup
  "Pure hiccup for the notebook's closing section; mode :notebook or :hub."
  ([id lens] (section-hiccup id lens :notebook))
  ([id {:keys [now fields tangents texts links]} mode]
   (let [registry (series)]
     [:section {:class "history-lens" :style {:margin "3em 0" :padding "1.5em"
                                               :border-top "2px solid #597b9a"}}
      [:h2 "Through today's lens"]
      (into [:div {:class "lens-now"}]
            (for [{:keys [claim modern tex]} now]
              [:div [:h3 claim] [:p modern] [:div {:class "lens-tex"} (str "$$" tex "$$")]]))
      [:h3 "Where it lives now"]
      (into [:div {:style {:display "flex" :flex-wrap "wrap" :gap "0.7em"}}]
            (for [{:keys [field label how example]} fields] (card (or label (strands/title field)) [:span how " " [:em example]])))
      [:h3 "Follow a tangent"]
      (into [:div {:style {:display "flex" :flex-wrap "wrap" :gap "0.7em"}}]
            (for [{:keys [series hook]} tangents
                  :let [{:keys [title status url]} (get registry series)]]
              (card (if (= status :live)
                      [:a {:href url :data-ignore-anchor-click "true"} title]
                      (str title " · planned")) hook)))
      [:h3 "More texts by the author"]
      (into [:ul] (for [{:keys [author work year url kind lang]} texts]
                    [:li [:a {:href url :target "_blank" :rel "noopener noreferrer"}
                          (str author ", " work " (" year ")")]
                     (str " · " (name kind) " · " lang)]))
      [:h3 "Leads to / comes from"]
      (into [:ul] (for [{:keys [to relation note]} links]
                    [:li [:a (links/anchor (links/notebook-href mode to)) to]
                     " · " (links/relation-label relation) " — " note]))])))

(defn section [id]
  (if-let [data (read-lens id)]
    (let [hiccup (section-hiccup id data)
          now (into [:div {:class "lens-now"}]
                    (for [{:keys [claim modern tex]} (:now data)]
                      [:div [:h3 claim] [:p modern] (clerk/tex tex)]))]
      (clerk/html (assoc hiccup 3 now)))
    (throw (ex-info (str "Missing lens: " id) {:id id}))))
