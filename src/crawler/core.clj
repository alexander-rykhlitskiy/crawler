(ns crawler.core
  (:gen-class)
  (:require [org.httpkit.client :as httpkit]
            [net.cgrand.enlive-html :as html]))

(defn create-node
  [value]
  (atom {:value value :children []}))

(defn add-note
  [record place]
  (let [node (create-node record)]
    (swap! place assoc :children (conj (@place :children) node))
    node))

(defn display-tree
  ([log-root]
    (println)
    (display-tree (@log-root :children) 0))
  ([nodes level]
    (let [indent (apply str (repeat (* level 4) " "))]
      (doseq [node nodes]
        (println indent (@node :value))
        (display-tree (@node :children) (+ level 1))))))

(defn new-log
  [log url & rest]
  (add-note (apply str url "    " rest) log))

(defn fetch-links
  [parsed-body]
  (let [a-tags (html/select parsed-body [:a])
        links (map #(get-in % [:attrs :href]) a-tags)]
    links))

(defn select-links
  [links]
  (->> links
    (filter identity)
    (filter #(.startsWith % "http"))))

(defn process-page
  [log url body max-depth current-depth]
  (try
    (let [parsed-body (html/html-resource (java.io.StringReader. body))
          links (fetch-links parsed-body)
          links (select-links links)
          log (new-log log url "ok " (count links) " links")]
      [log links])
    (catch java.lang.ClassCastException e [log []])))

(defn handle-redirect
  [log url redirect-url]
  (let [redirect-url (first (select-links [redirect-url]))]
    (if redirect-url
      [(new-log log url "redirect" " " redirect-url) url])))

(defn get-page
  [url]
  (let [response @(httpkit/get url {:follow-redirects false :throw-exceptions false})
        status (response :status)]
    (if
      (= status 200)
      [:ok (response :body)]
      (if
        (= 404 status)
        [:not-found nil]
        (if
          (contains? #{301 302 307} status)
          [:redirect (get-in response [:headers :location])]
          [:unknown-error status])))))

(defn parse-urls-from-file
  [path]
  {:pre [(not (nil? path))]}
  (with-open [r (clojure.java.io/reader path)]
    (doall (line-seq r))))

(defn crawl
  [log url max-depth current-depth]
  (if (>= max-depth current-depth)
    (let [[code content] (get-page url)]
      (case code
        :ok
        (let [[log links] (process-page log url content max-depth current-depth)]
          (doall (pmap (fn [link]
                          (crawl log link max-depth (inc current-depth)))
                          links)))
        :not-found
        (new-log log url "404")
        :redirect
        (let [redirect-url content
              [log url] (handle-redirect log url redirect-url)]
          (if url
            (crawl log redirect-url max-depth (inc current-depth))))
        :unknown-error
        (new-log log url "unknown error")))))

(defn process-urls
  [log urls depth]
  (doall (pmap (fn [url]
    (crawl log url depth 1)) urls))
  (display-tree log)
  (shutdown-agents))

(defn -main
  [file depth]
  (process-urls (create-node :root) (parse-urls-from-file file) (Integer/parseInt depth)))
