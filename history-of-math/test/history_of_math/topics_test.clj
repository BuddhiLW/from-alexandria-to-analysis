(ns history-of-math.topics-test
  "The page that connects every topic (<out>/series/): the graph is drawn
   from series.edn and the notebooks, an edge per pair of topics that share
   a work, and the plain list names every topic with working links."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [history-of-math.apex :as apex]
            [history-of-math.build :as build]
            [history-of-math.links :as links]
            [history-of-math.series :as series]
            [history-of-math.topics :as topics]
            [clojure.set :as set]))

(defn- nodes [h] (filter vector? (tree-seq coll? seq h)))
(defn- attrs [n] (when (map? (second n)) (second n)))
(defn- with-class [h c] (filter #(= c (:class (attrs %))) (nodes h)))

(def the-twelve
  ["alexandria-to-analysis" "chords-to-fourier" "dice-to-measure" "equations-to-structures"
   "whole-numbers" "bridges-to-colours" "shapes-that-stretch" "curved-space"
   "infinite-counted" "proof-and-machine" "law-of-nature" "figures-and-groups"])

(defn- g []
  (let [{:keys [reg notebooks joined]} (topics/site)]
    {:reg reg :nbs notebooks :joined joined :g (topics/graph reg notebooks joined)}))

(deftest every-topic-is-a-node-in-registry-order
  (let [{:keys [reg nbs joined g]} (g)
        ts (:topics g)]
    (is (= (map :slug (series/notebook-series reg)) (map :slug ts)))
    (is (= (set the-twelve) (set (map :slug ts))) "the twelve topics, no more, no less")
    (is (= 12 (count ts)))
    (testing "a node is sized by the notebooks built on it"
      (doseq [[a b] (partition 2 1 (sort-by :built ts))]
        (is (<= (:radius a) (:radius b)) (str (:slug a) " vs " (:slug b))))
      (is (< 1 (count (distinct (map :built ts)))) "non-vacuity: topics differ in size"))
    (doseq [{:keys [slug built planned href] ws :works :as t} ts
            :let [s ((series/by-slug reg) slug)]]
      (testing slug
        (is (= (str "../series/" slug "/") href) "the topic's timeline, from <out>/series/")
        (is (= (count ws) (+ built planned)))
        (is (= built (count (filter :built? ws))))
        (is (<= built (count (filter #(and (:built? %) (:notebook %)) (apex/index-rows s nbs joined)))))
        (is (= (count ws) (count (distinct (map :key ws)))) "a work once per topic")
        (is (every? #(= (str "../" (:notebook %) "/?series=" slug) (:href %)) (filter :built? ws)))
        (is (every? #(nil? (:href %)) (remove :built? ws)))
        (is (< 0 (:radius t)))))))

(deftest an-edge-is-the-works-two-topics-share
  (let [{:keys [g]} (g)
        {:keys [topics edges work-topics]} g
        keys-of (into {} (for [t topics] [(:slug t) (set (map :key (:works t)))]))]
    (is (<= 10 (count edges)) "non-vacuity: the topics do share works")
    (doseq [{:keys [from to weight built] ws :works} edges]
      (testing (str from "~" to)
        (is (not= from to))
        (is (= weight (count ws)))
        (is (<= built weight))
        (is (= (set ws) (set/intersection (keys-of from) (keys-of to))))))
    (testing "every pair that shares a work has its edge"
      (let [pairs (set (map (juxt :from :to) edges))]
        (doseq [[i a] (map-indexed vector topics) b (drop (inc i) topics)
                :when (seq (set/intersection (keys-of (:slug a)) (keys-of (:slug b))))]
          (is (contains? pairs [(:slug a) (:slug b)])))))
    (testing "work -> topics agrees with the topics' works"
      (doseq [[k slugs] work-topics s slugs]
        (is (contains? (keys-of s) k))))
    (testing "Gauss and Euler bridge several topics"
      (is (<= 3 (count (work-topics "gauss" []))) (pr-str (work-topics "gauss")))
      (is (<= 3 (count (work-topics "euler" []))) (pr-str (work-topics "euler"))))))

(deftest the-plain-list-names-every-topic-with-working-links
  (let [{:keys [g]} (g)
        lst (topics/topic-list g)
        sections (with-class lst "topics-topic")
        hrefs (set (keep #(:href (attrs %)) (nodes lst)))]
    (is (= (map :slug (:topics g)) (map #(:data-series (attrs %)) sections)))
    (doseq [{:keys [slug title] ws :works} (:topics g)]
      (testing slug
        (is (contains? hrefs (str "../series/" slug "/")))
        (is (str/includes? (pr-str lst) title))
        (doseq [w (filter :built? ws)] (is (contains? hrefs (:href w))))))
    (testing "built works come first in each topic, planned ones are greyed text"
      (doseq [sec sections
              :let [lis (filter #(= :li (first %)) (nodes sec))
                    built-flags (map #(:data-built (attrs %)) lis)
                    planned (with-class sec "topics-work topics-planned")]]
        (is (= built-flags (sort-by #(if (= "true" %) 0 1) built-flags)) "no built work after a planned one")
        (doseq [li planned]
          (is (not= :a (first (nth li 2))) "a planned work's label is not a link")
          (is (every? #(str/starts-with? % "#topic-")
                      (keep #(when (= :a (first %)) (:href (attrs %))) (nodes li)))
              "its only links jump to the other topics in the list"))))
    (testing "the noscript HTML is the same list"
      (let [html (topics/noscript-html)]
        (doseq [slug the-twelve]
          (is (str/includes? html (str "href=\"../series/" slug "/\""))))
        (is (str/includes? html "topics-list"))
        (is (not (str/includes? html "<script")))))))

(deftest the-topics-page-is-built-into-series-with-its-noscript-list
  (let [calls (atom [])
        seen (atom nil)
        template (str build/notebooks-dir "/series/series_page.clj")]
    (build/build-series! template "out" (fn [opts]
                                          (swap! calls conj opts)
                                          (when (str/ends-with? (first (:paths opts)) "topics.clj")
                                            (reset! seen @build/!noscript))))
    (is (some #{{:paths [(str build/notebooks-dir "/series/topics.clj")] :out-path "out/series"}} @calls))
    (is (str/includes? @seen "topics-list"))
    (is (nil? @build/!noscript) "reset after the page")
    (is (str/includes? (build/with-noscript "<body><div id=\"clerk\">x</div>" "LIST")
                       "<noscript><div class=\"hom-noscript\">LIST</div></noscript><div id=\"clerk\">"))))

(deftest the-page-is-linked-from-the-apex-and-every-topic
  (let [{:keys [reg nbs joined]} (g)]
    (is (= "series/" (links/topics-href 0)))
    (is (= "../../series/" (links/topics-href 2)))
    (is (some #(= "series/" (:href (attrs %))) (with-class (apex/cards reg nbs joined) "topics-hub-link")))
    (is (str/includes? (slurp "src/history_of_math/topic.clj") "links/topics-href 2"))
    (let [t (slurp "notebooks/series/topics.clj")]
      (is (not (str/includes? t "\u2014")))
      (is (str/includes? t "topics/topic-list")))))
