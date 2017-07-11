(defproject geom-viz-gorilla "0.1.0-SNAPSHOT"
  :description "using geom-viz with gorilla-repl"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [gorilla-renderable "2.0.0"]
                 [thi.ng/geom "0.0.908"]]
  :main ^:skip-aot geom-viz-gorilla.core
  :plugins [[lein-gorilla "0.4.0"]]
  :profiles {:uberjar {:aot :all}})
