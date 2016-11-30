(defproject thinktopic/think.image "0.4.2-SNAPSHOT"
  :description "Image manipulation library"
  :url "http://github.com/thinktopic/think.image"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [thinktopic/datatype "0.1.0"]
                 [net.mikera/imagez "0.12.0"]]

  :source-paths ["src/clj" "src/cljc"]

  :java-source-paths ["java"]
  :profiles { :uberjar { :aot :all }
             :dev {:source-paths ["test/shared"
                                  "test/clj"] }
             :test {:source-paths ["test/shared"
                                   "test/clj"] }})
