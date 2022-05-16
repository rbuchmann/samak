(ns foo.handler
  (:require [compojure.core :refer [defroutes GET]]
            [compojure.route :as route]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [ring.util.response :as response]
            [derekchiang.ring-proxy :refer [wrap-proxy]]))

(defroutes app-routes
  (route/resources "/" {:root "resources/public"})
  (route/not-found "Not Found"))

(def app (-> app-routes
             (wrap-defaults (assoc-in site-defaults [:security :anti-forgery] false))
             (wrap-proxy "/api" "http://localhost:9411/api")))
