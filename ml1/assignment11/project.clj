(defproject ml1-assignment11 "0.1.0-SNAPSHOT"
  :description "ML1 assignment 11."
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/data.csv "0.1.4"]]

  :profiles {:dev {:source-paths ["src"]
                   :dependencies [[proto-repl "0.3.1"]
                                  [proto-repl-charts "0.3.1"]
                                  [proto-repl-sayid "0.1.3"]]
                   :repl-options {:init-ns assignment11}}})
