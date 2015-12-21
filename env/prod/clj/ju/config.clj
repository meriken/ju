(ns ju.config
  (:require [taoensso.timbre :as timbre]))

(def defaults
  {:init
   (fn []
     (timbre/info "\n-=[ju started successfully]=-"))
   :middleware identity})
