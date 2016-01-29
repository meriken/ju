(ns ju.config
  (:require [taoensso.timbre :as timbre]))

(def defaults
  {:init
   (fn []
     (timbre/info "Ju started successfully."))
   :middleware identity})
