(defproject k-means "0.1.0-SNAPSHOT"
  :description "Programming assignment for QUT INB375"
  :url ""
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]]
  :main ^:skip-aot k-means.core
  :target-path "target/%s"
  :plugins [[codox "0.8.13"]]
  :codox {:output-dir "hscells.github.io/k-means/doc"}
  :profiles {:uberjar {:aot :all}})
