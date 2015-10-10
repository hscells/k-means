(defproject k-means "0.7"
  :description "Programming assignment for QUT INB375 - https://github.com/hscells/k-means"
  :url ""
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"] [com.taoensso/timbre "4.1.1"]]
  :main ^:skip-aot k-means.core
  :target-path "target/%s"
  :plugins [[codox "0.8.13"]
            [lein-clique "0.1.2"]
            [cc.artifice/lein-gossip "0.2.1"]]
  :codox {:output-dir "hscells.github.io/k-means/doc"}
  :profiles { :uberjar {:aot :all}
              :debug {:timbre {:current-level :trace}}
              :dev {:timbre {:current-level :debug}}})
