(defproject turtle "0.1.0-SNAPSHOT"
            :description "FIXME: write description"
            :url "http://example.com/FIXME"
            :license {:name "Eclipse Public License"
                      :url "http://www.eclipse.org/legal/epl-v10.html"}
            :dependencies [[org.clojure/clojure "1.8.0"]
                           [org.clojure/clojurescript "1.9.293"]
                           [org.clojure/core.async "0.2.395"]
                           [figwheel-sidecar "0.5.8"]
                           [rm-hull/monet "0.3.0"]
                           [backtick "0.3.0"]
                           #_[com.taoensso/tufte "1.1.1"]]
            :plugins [[lein-cljsbuild "1.1.1"]]
            :source-paths ["src" "script"]
            :cljsbuild {:builds [{:id           "min"
                                  :source-paths ["src"]
                                  :compiler     {:main          'turtle.core
                                                 :asset-path    "js/out"
                                                 :optimizations :advanced
                                                 :output-to     "resources/public/js/out/main.min.js"
                                                 :output-dir    "resources/public/js/out"}}]})
