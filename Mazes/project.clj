(defproject maze_task_2 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [com.novemberain/monger "3.1.0"]
                 [org.clojure/data.json "1.0.0"]
                 [compojure "1.6.1"]
                 [ring "1.8.0"]
                 [org.clojure/java.jdbc "0.7.11"]
                 [stencil "0.5.0"]
                 [com.h2database/h2 "1.4.199"]
                 [ring/ring-defaults "0.3.2"]]
  :repl-options {:init-ns maze-task-2.core})
