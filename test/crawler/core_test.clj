(ns crawler.core-test
  (:require [clojure.test :refer :all]
            [crawler.core :refer :all]
            [net.cgrand.enlive-html :as html]
            [org.httpkit.client :as httpkit]))

(deftest test-get-page
  (testing "Processes 404 code"
    (with-redefs-fn {#'httpkit/get (fn [& _] (atom {:status 404}))}
      #(is (=
        [:not-found nil]))))
        (get-page "url")

  (testing "Processes 1000 code as unknown error"
    (with-redefs-fn {#'httpkit/get (fn [& _] (atom {:status 1000}))}
      #(is (=
        [:unknown-error 1000])))))
        (get-page "url")

(deftest test-select-links
  (testing "Returns absolute links"
    (is (=
      (select-links ["http://select.by/index", "relative.html", "/another.html", "https://secure.com/blabla"])
      ["http://select.by/index", "https://secure.com/blabla"]))))

(deftest test-fetch-links
  (testing "Fetches all links from page"
    (let [html-string "<a href=\"http://select.by/link\">Link</a>
            <div>div content</div><a href=\"/blabla/link\">Link</a>"
          html-fragment (html/html-resource (java.io.StringReader. html-string))]
      (is (=
        (fetch-links html-fragment)
        ["http://select.by/link", "/blabla/link"])))))
