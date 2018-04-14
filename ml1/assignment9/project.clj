(defproject ml1-assignment9 "0.1.0-SNAPSHOT"
  :description "ML1 assignment 9."
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/data.csv "0.1.4"]
                 [uncomplicate/neanderthal "0.17.1"]]

  :profiles {:dev {:source-paths ["dev" "src"]
                   :dependencies [[proto-repl "0.3.1"]
                                  [proto-repl-charts "0.3.1"]
                                  [proto-repl-sayid "0.1.3"]]
                   :repl-options {:init-ns user}}})
