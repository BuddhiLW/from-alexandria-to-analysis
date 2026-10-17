(ns history-of-math.topics-view
  "The graph of the topics, rendered in the browser: the topics on a ring,
   a chord between two topics for the works they share. Selecting a topic
   (a node, the picker, or ?series=<slug> in the URL) lights it and its
   neighbours and lists its works, built first; selecting a chord or a work
   shows which topics it bridges. Positions, counts and hrefs come from the
   JVM (history-of-math.topics/graph); the view only chooses what is lit."
  (:require [reagent.core :as r]))

(def accent "#e7a85c")
(def node-colour "#7fb3d5")
(def ink "#e9f1f5")
(def muted "#8fa0b0")

(defn- url-slug [slugs]
  (let [q (.get (js/URLSearchParams. (.. js/window -location -search)) "series")]
    (when (some #{q} slugs) q)))

(defn- remember!
  "Put the selected topic in the URL (?series=<slug>), so the page can be
   shared or reloaded with it."
  [slug]
  (try
    (let [u (js/URL. (.. js/window -location -href))]
      (if slug (.set (.-searchParams u) "series" slug) (.delete (.-searchParams u) "series"))
      (.replaceState js/history nil "" (str u)))
    (catch :default _ nil)))

(defn- lit
  "The topics and edges lit by selection sel ({:kind :topic|:edge|:work :id})."
  [sel edges work-topics]
  (case (:kind sel)
    :topic (let [s (:id sel)
                 es (filter #(or (= s (:from %)) (= s (:to %))) edges)]
             {:topics (into #{s} (mapcat (juxt :from :to)) es) :edges (set es)})
    :edge (let [e (:id sel)] {:topics #{(:from e) (:to e)} :edges #{e}})
    :work (let [ts (set (get work-topics (:id sel)))]
            {:topics ts :edges (set (filter #(and (ts (:from %)) (ts (:to %))
                                                   (some #{(:id sel)} (:works %)))
                                            edges))})
    nil))

(defn- chord [{:keys [cx cy]} a b]
  (let [mx (/ (+ (:x a) (:x b)) 2) my (/ (+ (:y a) (:y b)) 2)
        qx (+ mx (* 0.35 (- cx mx))) qy (+ my (* 0.35 (- cy my)))]
    (str "M" (:x a) "," (:y a) " Q" qx "," qy " " (:x b) "," (:y b))))

(defn- graph-svg [{:keys [topics edges ring]} sel select! lit-set]
  (let [{:keys [width height]} ring
        by (into {} (map (juxt :slug identity)) topics)
        on? (fn [slug] (or (nil? lit-set) (contains? (:topics lit-set) slug)))
        edge-on? (fn [e] (or (nil? lit-set) (contains? (:edges lit-set) e)))]
    [:svg {:class "topics-graph" :view-box (str "0 0 " width " " height) :role "img"
           :aria-label "The topics and the works they share"
           :style {:display "block" :width "100%" :max-width (str width "px") :height "auto"
                   :margin "0 auto" :background "#101e30" :border-radius "10px"
                   :font-family "ui-serif, Georgia, serif"}}
     (for [{:keys [from to weight] :as e} edges
           :let [a (by from) b (by to) chosen? (= e (:id sel))]]
       ^{:key (str from "~" to)}
       [:g {:class "topics-edge" :data-from from :data-to to :data-weight weight}
        [:path {:d (chord ring a b) :fill "none" :stroke accent
                :stroke-width (min 9 (+ 0.8 (* 1.1 weight))) :stroke-linecap "round"
                :stroke-opacity (cond chosen? 0.95 (edge-on? e) (if lit-set 0.75 0.42) :else 0.06)}]
        ;; a wide invisible path, so a thin chord is easy to hit
        [:path {:class "topics-edge-hit" :d (chord ring a b) :fill "none" :stroke "transparent"
                :stroke-width 14 :style {:cursor "pointer"}
                :on-click #(select! {:kind :edge :id e})}
         [:title (str (:title a) " and " (:title b) ": " weight " shared " (if (= 1 weight) "work" "works"))]]])
     (for [{:keys [slug title x y radius lines anchor lx ly built planned scope]} topics
           :let [chosen? (and (= :topic (:kind sel)) (= slug (:id sel)))]]
       ^{:key slug}
       [:g {:class "topics-node" :data-series slug :data-built built :data-planned planned
            :data-lit (str (on? slug)) :tab-index 0 :role "button"
            :aria-label (str title ", " built " built, " planned " planned")
            :style {:cursor "pointer"} :opacity (if (on? slug) 1 0.3)
            :on-click #(select! (if chosen? nil {:kind :topic :id slug}))
            :on-key-down #(when (#{"Enter" " "} (.-key %))
                            (.preventDefault %)
                            (select! (if chosen? nil {:kind :topic :id slug})))}
        [:title scope]
        [:circle {:cx x :cy y :r radius :fill (if (pos? built) node-colour "none")
                  :stroke (if chosen? accent node-colour) :stroke-width (if chosen? 4 2)}]
        [:text {:x lx :y ly :fill ink :font-size 17 :text-anchor anchor
                :font-weight (if chosen? "bold" "normal")}
         (for [[i line] (map-indexed vector lines)]
           ^{:key i} [:tspan {:x lx :dy (if (zero? i) 0 18)} line])
         [:tspan {:x lx :dy 17 :fill muted :font-size 13} (str built " built, " planned " planned")]]])]))

(defn- topic-button [by slug select! & [suffix]]
  [:button {:class "topics-pick" :data-series slug
            :on-click #(select! {:kind :topic :id slug})}
   (:title (by slug)) suffix])

(defn- work-row [{:keys [key label built? href]} slug work-topics select!]
  (let [others (remove #{slug} (get work-topics key))]
    [:li {:class (if built? "topics-work" "topics-work topics-planned") :data-work key
          :data-built (str built?) :style (when-not built? {:opacity 0.55})}
     (if built?
       [:a {:href href :data-ignore-anchor-click "true" :class "topics-notebook"} label]
       [:span label [:em " (planned)"]])
     (when (seq others)
       [:button {:class "topics-bridges" :data-work key
                 :on-click #(select! {:kind :work :id key})}
        (str "also in " (count others) " other " (if (= 1 (count others)) "topic" "topics"))])]))

(defn- panel [{:keys [topics edges work-topics]} sel select!]
  (let [by (into {} (map (juxt :slug identity)) topics)
        work-of (into {} (for [t topics w (:works t)] [(:key w) w]))]
    (case (:kind sel)
      :topic
      (let [{:keys [slug title scope href works built planned]} (by (:id sel))
            nbs (sort-by (comp - second)
                         (for [{:keys [from to weight]} edges
                               :when (or (= slug from) (= slug to))]
                           [(if (= slug from) to from) weight]))]
        [:section {:class "topics-panel" :data-kind "topic" :data-series slug}
         [:h3 [:a {:href href :data-ignore-anchor-click "true" :class "topics-open-series"} title]]
         [:p scope]
         [:p {:class "topics-facts"}
          (str built " built, " planned " planned. ")
          [:a {:href href :data-ignore-anchor-click "true"} "Open its timeline"]]
         (when (seq nbs)
           (into [:p {:class "topics-neighbours"} "Shares works with "]
                 (interpose " "
                            (for [[o w] nbs]
                              ^{:key o} [topic-button by o select! (str " (" w ")")]))))
         [:ol {:class "topics-works"}
          (for [w (concat (filter :built? works) (remove :built? works))]
            ^{:key (:key w)} [work-row w slug work-topics select!])]])

      :edge
      (let [{:keys [from to weight] ks :works} (:id sel)
            ws (map #(or (some (fn [w] (when (= % (:key w)) w)) (:works (by from))) (work-of %)) ks)]
        [:section {:class "topics-panel" :data-kind "edge" :data-from from :data-to to}
         [:h3 (:title (by from)) " and " (:title (by to))]
         [:p (str weight " shared " (if (= 1 weight) "work" "works") ". ")
          [topic-button by from select!] " " [topic-button by to select!]]
         [:ol {:class "topics-works"}
          (for [w (concat (filter :built? ws) (remove :built? ws))]
            ^{:key (:key w)} [work-row w from work-topics select!])]])

      :work
      (let [k (:id sel)
            {:keys [label built? notebook]} (work-of k)
            ts (get work-topics k)]
        [:section {:class "topics-panel" :data-kind "work" :data-work k}
         [:h3 label (when-not built? [:em " (planned)"])]
         [:p (str "On " (count ts) " " (if (= 1 (count ts)) "topic" "topics") ":")]
         [:ul {:class "topics-work-topics"}
          (for [slug ts
                :let [w (some #(when (= k (:key %)) %) (:works (by slug)))]]
            ^{:key slug}
            [:li [topic-button by slug select!]
             (when (and built? notebook (:href w))
               [:span " " [:a {:href (:href w) :data-ignore-anchor-click "true" :class "topics-notebook"}
                           "read it in this topic"]])])]])

      [:section {:class "topics-panel" :data-kind "none"}
       [:p "Pick a topic on the ring or in the list to see its works and the topics it shares them with. "
        "Pick a chord to see the works two topics share."]])))

(defn render
  "The topic picker, the graph and the panel of the selection."
  [{:keys [topics edges work-topics] :as g}]
  (r/with-let [sel (r/atom (when-let [s (url-slug (map :slug topics))] {:kind :topic :id s}))]
    (let [select! (fn [s]
                    (reset! sel s)
                    (when (or (nil? s) (= :topic (:kind s))) (remember! (:id s))))
          lit-set (lit @sel edges work-topics)]
      [:div {:class "topics-hub" :data-selected (if @sel (str (name (:kind @sel))) "none")}
       [:label {:class "topics-picker" :style {:display "block" :margin "0.6em 0"}}
        "Topic: "
        [:select {:value (if (= :topic (:kind @sel)) (:id @sel) "")
                  :on-change #(let [v (.. % -target -value)]
                                (select! (when (seq v) {:kind :topic :id v})))}
         [:option {:value ""} "All topics"]
         (for [{:keys [slug title built planned]} topics]
           ^{:key slug} [:option {:value slug} (str title " (" built " built, " planned " planned)")])]]
       [graph-svg g @sel select! lit-set]
       [panel g @sel select!]])))
