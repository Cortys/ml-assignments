(defproject ml2-assignment1 "0.1.0-SNAPSHOT"
  :description "ML2 assignment 1."
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/math.combinatorics "0.1.4"]
                 [org.clojure/data.csv "0.1.4"]
                 [lambda-ml "0.1.1"]]

  :profiles {:dev {:dependencies [[proto-repl "0.3.1"]
                                  [proto-repl-charts "0.3.1"]
                                  [proto-repl-sayid "0.1.3"]]
                   :repl-options {:init-ns task1}}})
