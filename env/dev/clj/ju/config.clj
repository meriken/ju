(ns ju.config
  (:require [selmer.parser :as parser]
            [taoensso.timbre :as timbre]
            [ju.dev-middleware :refer [wrap-dev]]))

(def defaults
  {:init
   (fn []
     (parser/cache-off!)
     (timbre/info "\n-=[ju started successfully using the development profile]=-"))
   :middleware wrap-dev})
