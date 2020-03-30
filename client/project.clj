(defproject client "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [com.novemberain/monger "3.1.0"]
                 [org.clojure/data.json "1.0.0"]
                 [compojure "1.6.1"]
                 [ring "1.8.0"]
                 [org.clojure/java.jdbc "0.7.11"]
                 [stencil "0.5.0"]
                 [com.h2database/h2 "1.4.199"]
                 [ring/ring-defaults "0.3.2"]]
  :ring {:handler client.core/-main}
  :main client.core
  :repl-options {:init-ns client.core})
