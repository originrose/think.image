(defproject thinktopic/think.image "0.4.11-SNAPSHOT"
  :description "Image manipulation library"
  :url "http://github.com/thinktopic/think.image"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha17"]
                 [thinktopic/think.datatype "0.3.11"]
                 [net.mikera/imagez "0.12.0"]]

  :source-paths ["src/clj" "src/cljc"]

  :plugins [[lein-codox "0.10.2"]]

  :java-source-paths ["java"]

  :profiles { :uberjar { :aot :all }
             :dev {:source-paths ["test/shared"
                                  "test/clj"] }
             :test {:source-paths ["test/shared"
                                   "test/clj"] }}

  :think/meta {:type :library :tags [:image :clj :cljc]})
