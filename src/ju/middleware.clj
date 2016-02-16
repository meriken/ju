(ns ju.middleware
  (:require [ju.layout :refer [*app-context* error-page]]
            [taoensso.timbre :as timbre]
            [environ.core :refer [env]]
            [ring.middleware.flash :refer [wrap-flash]]
            [ring.util.codec :refer [percent-decode]]
            [immutant.web.middleware :refer [wrap-session]]
            ;[ring.middleware.webjars :refer [wrap-webjars]]
            [ring.middleware.defaults :refer [site-defaults wrap-defaults]]
            [ring.middleware.anti-forgery :refer [wrap-anti-forgery]]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.keyword-params :refer [wrap-keyword-params]]
            [ring.middleware.format :refer [wrap-restful-format]]
            [buddy.auth.middleware :refer [wrap-authentication]]
            [buddy.auth.backends.session :refer [session-backend]]
            [buddy.auth.accessrules :refer [restrict]]
            [buddy.auth :refer [authenticated?]]
            [ju.layout :refer [*identity*]]
            [ju.config :refer [defaults]])
  (:import [javax.servlet ServletContext]
           (java.io ByteArrayInputStream)))

(defn wrap-context [handler]
  (fn [request]
    (binding [*app-context*
              (if-let [context (:servlet-context request)]
                ;; If we're not inside a servlet environment
                ;; (for example when using mock requests), then
                ;; .getContextPath might not exist
                (try (.getContextPath ^ServletContext context)
                     (catch IllegalArgumentException _ context))
                ;; if the context is not specified in the request
                ;; we check if one has been specified in the environment
                ;; instead
                (:app-context env))]
      (handler request))))

(defn wrap-internal-error [handler]
  (fn [req]
    (try
      (handler req)
      (catch Throwable t
        (timbre/error t)
        (error-page {:status 500
                     :title "Something very bad has happened!"
                     :message "We've dispatched a team of highly trained gnomes to take care of the problem."})))))

(defn wrap-csrf [handler]
  (wrap-anti-forgery
    handler
    {:error-response
     (error-page
       {:status 403
        :title "Invalid anti-forgery token"})}))

(defn wrap-formats [handler]
  (wrap-restful-format handler {:formats [:json-kw ]}))

(defn on-error [request response]
  (error-page
    {:status 403
     :title (str "Access to " (:uri request) " is not authorized")}))

(defn wrap-restricted [handler]
  (restrict handler {:handler authenticated?
                     :on-error on-error}))

(defn wrap-identity [handler]
  (fn [request]
    (binding [*identity* (get-in request [:session :identity])]
      (handler request))))

(defn wrap-auth [handler]
  (-> handler
      wrap-identity
      (wrap-authentication (session-backend))))

(defn- percent-encode-everything
  [s]
  (cond
    (zero? (count s))
    ""

    (re-find #"^(%[0-9A-Fa-f]{2})+$" s)
    s

    (re-find #"^%[0-9A-Fa-f]{2}" s)
    (let [match (re-find #"^(%[0-9A-Fa-f]{2})(.*)$" s)]
      (percent-encode-everything
        (str
          (nth match 1)
          (percent-encode-everything (nth match 2)))))

    (re-find #"^\+" s)
    (percent-encode-everything
      (str
        "%20"
        (apply str (drop 1 s))))

    :else
    (percent-encode-everything
      (str
        (format "%%%02X" (int (first (take 1 s))))
        (apply str (drop 1 s))))))

(defn percent-decode-for-2ch-post
  [s]
  ;(timbre/debug "percent-decode-for-2ch-post:" s)
  (percent-decode (percent-encode-everything s) "windows-31j"))

(defn wrap-2ch-post
  [handler]
  (fn [request]
    (if (= (:uri request) "/test/bbs.cgi")
      (handler (assoc request
                 :form-params
                 (->> (clojure.string/split (slurp (:body request)) #"&")
                             (map #(re-find #"^([^=]+)=(.*)$" %))
                             (map #(do {(keyword (nth % 1)) (percent-decode-for-2ch-post (nth % 2))}))
                             (apply merge))))
      (handler request))))

(defn wrap-base [handler]
  (-> ((:middleware defaults) handler)
      wrap-auth
      wrap-formats
      ;wrap-webjars
      wrap-flash
      (wrap-session {:cookie-attrs {:http-only true}})
      (wrap-defaults
        (-> site-defaults
            (assoc-in [:security :anti-forgery] false)
            (dissoc :session)))
      wrap-2ch-post
      wrap-context
      wrap-internal-error))
