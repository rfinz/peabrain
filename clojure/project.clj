(defproject peabrain "0.1.1-SNAPSHOT"
  :description "maybe peabrain can be a real boy?"
  :url "https://github.com/rfinz/peabrain"
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [uncomplicate/neanderthal "0.22.1"]
                 [potemkin "0.4.5"]]
  :repl-options {:init-ns peabrain.core}
  :exclusions [[org.jcuda/jcuda-natives :classifier "apple-x86_64"]
               [org.jcuda/jcublas-natives :classifier "apple-x86_64"]])
