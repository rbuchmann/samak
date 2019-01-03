(defproject samak "0.1.0-SNAPSHOT"
  :license {:name "The MIT License"
            :url  "https://opensource.org/licenses/MIT"}
  :source-paths ["src" "ui_src"]
  :description "A hello world application for electron"
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/clojurescript "1.10.439"]
                 [figwheel "0.5.16"]
                 [reagent "0.6.1"]
                 [garden "1.3.2"]
                 [ring/ring-core "1.6.1"]
                 [org.clojure/core.async "0.3.443"]
                 [org.clojure/core.logic "0.8.11"]
                 [datascript "0.16.1"]
                 [com.stuartsierra/dependency "0.2.0"]
                 [net.cgrand/xforms "0.9.2"]
                 [cljs-http "0.1.43"]
                 [clj-http "3.7.0"]
                 [automat "0.2.2"]
                 [cljsjs/klayjs "0.3.2-0"]
                 [cljsjs/elkjs "0.3.0-0"]
                 ;; [org.eclipse.elk/parent "0.3.0" :extension "pom"]
                 [camel-snake-kebab "0.4.0"]
                 [keybind "2.1.0"]
                 [expound "0.7.2"]
                 [org.clojure/test.check "0.9.0"]]
  :plugins [[lein-cljsbuild "1.1.7"]
            [lein-figwheel "0.5.16"]
            [lein-cooper "1.2.2"]
            [lein-garden "0.3.0"]]

  :main samak.main

  :clean-targets ^{:protect false} ["resources/main.js"
                                    "resources/public/js/ui-core.js"
                                    "resources/public/js/ui-core.js.map"
                                    "resources/public/js/ui-out"]
  :garden
  {:builds [{:source-paths ["ui_src"]
             :stylesheet   ui.styles/style
             :compiler     {:output-to "resources/public/css/main.css"}}]}
  :cljsbuild
  {:builds
   [{:source-paths ["src" "cli_src"]
     :id           "cli-dev"
     :compiler     {:output-to      "cli/samak-cli.js"
                    :output-dir     "cli"
                    ;; :source-map     true
                    :asset-path     "cli"
                    :optimizations  :none
                    :cache-analysis true
                    :target         :nodejs
                    :infer-externs  true
                    :main           "cli.node-core"}}
    {:source-paths ["src" "ui_src"]
     :id           "frontend-dev"
     :compiler     {:output-to      "resources/public/js/ui-core.js"
                    :output-dir     "resources/public/js/ui-out"
                    :source-map     true
                    :asset-path     "js/ui-out"
                    :optimizations  :none
                    :cache-analysis true
                    :main           "ui.core"}}
    {:source-paths ["src" "dev_src"]
     :id           "frontend-release"
     :compiler     {:output-to      "resources/public/js/ui-core.js"
                    :output-dir     "resources/public/js/ui-release-out"
                    :source-map     "resources/public/js/ui-core.js.map"
                    :optimizations  :advanced
                    :cache-analysis true
                    :infer-externs  true
                    :main           "dev.core"}}
    {:source-paths ["electron_src"]
     :id           "electron-dev"
     :compiler     {:output-to      "resources/main.js"
                    :output-dir     "resources/public/js/electron-dev"
                    :optimizations  :simple
                    :pretty-print   true
                    :cache-analysis true}}
    {:source-paths ["electron_src"]
     :id           "electron-release"
     :compiler     {:output-to      "resources/main.js"
                    :output-dir     "resources/public/js/electron-release"
                    :optimizations  :advanced
                    :pretty-print   true
                    :cache-analysis true
                    :infer-externs  true}}]}
  :figwheel {:http-server-root "public"
             :css-dirs         ["resources/public/css"]
             :reload-clj-files {:clj  true
                                :cljc true}
             :ring-handler     tools.figwheel-middleware/app
             :server-port      3449})
