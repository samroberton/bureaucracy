(defproject com.samroberton/bureaucracy "0.2.0-SNAPSHOT"
  :description  "First-class state management for UIs via composeable statecharts."

  :url          "https://github.com/samroberton/bureaucracy"

  :license      {:name "Eclipse Public License"
                 :url "https://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/tools.logging "0.3.1"]
                 [prismatic/schema "1.0.1"]]

  :profiles     {:provided {:dependencies [[org.clojure/clojure "1.7.0"]
                                           [cljsjs/react "0.14.0-1"]
                                           [cljsjs/react-dom "0.14.0-1"]]}})
