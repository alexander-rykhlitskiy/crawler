(ns crawler.core
  (:gen-class)
  (:require [org.httpkit.client :as httpkit]
            [net.cgrand.enlive-html :as html]))

(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

; (def ^:dynamic *base-url* "http://tut.by/")

(defn fetch-url [url]
  (html/html-resource (java.net.URL. url)))

(defn get-links [url]
  (let [page (fetch-url url)]
    (remove #(= nil %)
      (map :href
        (map #(get % :attrs)
          (html/select page [:a]))))))

(defn crawl [url depth]
  (println (str "crawl called: " "depth=" depth " url=" url))
  (doseq [url-path (get-links url)]
    #(if (> depth 0)
      (crawl url (dec depth)))))

(defn -main
  [& args]
  (let [url (first args)]
    (let [depth (read-string (second args))]
      (crawl url depth)))
)
