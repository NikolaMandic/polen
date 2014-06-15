(defproject rapp "0.1.0-SNAPSHOT"
  :description "TODO"
  :url "TODO"
  :license {:name "TODO: Choose a license"
            :url "http://choosealicense.com/"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [instaparse "1.3.1"]
                 [com.stuartsierra/clojure.walk2 "0.1.0-SNAPSHOT"]
                 [clojurewerkz/titanium "1.0.0-beta1"]
                 ]
  :profiles {:dev {:dependencies [[org.clojure/tools.namespace "0.2.4"]]
                   :source-paths ["dev"]}})
