(ns ju.core
  (:require [reagent.core :as reagent :refer [atom]]
            [reagent.session :as session]
            [secretary.core :as secretary :include-macros true]
            [goog.events :as events]
            [goog.history.EventType :as EventType]
            [markdown.core :refer [md->html]]
            [ajax.core :refer [GET POST]]
            )
  (:use     [jayq.core :only [$ attr on off html add-class]])
  (:use-macros [jayq.macros :only [ready]])
  (:import [goog.history Html5History]
            [goog Uri]
            [goog.history EventType]))

(enable-console-print!)

(defn nav-link [uri title page collapsed?]
  [:li {:class (when (= page (session/get :page)) "active")}
   [:a {:href uri
        :on-click #(reset! collapsed? true)}
    title]])
(def collapsed? (atom true))
(defn navbar []
  (let []
    (fn []
      [:nav.navbar.navbar-default.navbar-fixed-top
       [:div.container
        [:div.navbar-header
         [:button#navbar-toggle-button.navbar-toggle
          {:class         (when-not @collapsed? "collapsed")
           :data-toggle   "collapse"
           :aria-expanded @collapsed?
           :aria-controls "navbar"
           :on-click      #(swap! collapsed? not)}
          [:span.sr-only "Toggle Navigation"]
          [:span.icon-bar]
          [:span.icon-bar]
          [:span.icon-bar]]
         [:a.navbar-brand {:href "/"} "需"]]
        [:div.navbar-collapse.collapse
         (when-not @collapsed? {:class "in"})
         [:ul.nav.navbar-nav
          [nav-link "/" "目次" :home collapsed?]
          [nav-link "/threads" "スレッド一覧" :threads collapsed?]
          [nav-link "/new-posts" "新着レスまとめ読み" :new-posts collapsed?]
          [nav-link "/create-new-thread" "新規スレッド作成" :create-new-thread collapsed?]
          [nav-link "/status" "状態" :status collapsed?]
          [nav-link "/help" "使い方" :help collapsed?]]]]])))

(defn home-page []
  [:div.container
   [:h3 "需"]
   [:p
    "「需」は新月ネットワークに参加しているP2P型の匿名掲示板です。"
    "新月ネットワーク規約を守った上で、自由に利用してください。"]

   [:div.row
    [:div#main-menu-column.col-sm-6
     [:div#main-menu.list-group
      [:a {:href "/threads" :class "list-group-item"} "スレッド一覧" [:span.glyphicon.glyphicon-chevron-right.pull-right]]
      [:a {:href "/new-posts" :class "list-group-item"} "新着レスまとめ読み" [:span.glyphicon.glyphicon-chevron-right.pull-right]]
      ; [:a {:href "/create-new-thread" :class "list-group-item"} "新規スレッド作成" [:span.glyphicon.glyphicon-chevron-right.pull-right]]
      ; [:a {:href "/status" :class "list-group-item"} "状態" [:span.glyphicon.glyphicon-chevron-right.pull-right]]
      [:a {:href "/help" :class "list-group-item"} "使い方" [:span.glyphicon.glyphicon-chevron-right.pull-right]]]]

    [:div#tag-menu-column.col-sm-6
     [:div#tag-menu.panel.panel-default
      [:div.panel-heading "タグ一覧"]
      [:div.panel-body
       (map (fn [group]
              [:div.btn-group.btn-group-sm {:role "group"}
               (map #(do [:a.btn.btn-success %]) group)])
            [["質問" "雑談" "ニュース" "実況"]
             ["生活", "料理", "日課"]
             ["画像", "動画", "二次元", "三次元", "18禁"]
             ["趣味", "音楽", "テレビ"]
             ["漫画", "アニメ", "ゲーム", "2ch"]
             ["PC", "ソフトウェア", "ハードウェア"]
             ["開発", "プログラミング", "IT", "P2P"]
             ["新月", "運用", "スレ一覧", "テスト"]
             ["きれいな新月", "裏"]])]]]]

   (comment when-let [docs (session/get :docs)]
     [:div.row
      [:div.col-md-12
       [:div {:dangerouslySetInnerHTML
              {:__html (md->html docs)}}]]])
   ]
  )

(defn threads-page []
  [:div.container
   [:h3 "スレッド一覧"]
   [:div#content
    (session/get :thread-list)
    ;[:span.glyphicon.glyphicon-refresh.spinning.loading-page]
    ]])

(defn new-posts-page []
  [:div.container
   [:h3 "新着まとめ読み"]
   [:div#content
    [:span.glyphicon.glyphicon-refresh.spinning.loading-page]]])

(defn create-new-thread-page []
  [:div.container
   [:h3 "新規スレッド作成"]
   [:div#content
    [:span.glyphicon.glyphicon-refresh.spinning.loading-page]]])

(defn help-page []
  [:div.container
   [:h3 "使い方"]
   [:div#content
    [:span.glyphicon.glyphicon-refresh.spinning.loading-page]]])

(def pages
  {:home #'home-page
   :threads #'threads-page
   :new-posts #'new-posts-page
   :create-new-thread #'create-new-thread-page
   :help #'help-page})

(def history (atom nil))

(defn handle-click-on-link [e]
  (let [href (.-href (.-target e))
        href (if href href (.-href (.-parentElement (.-target e))))
        path (.getPath (.parse Uri href))
        title (.-title (.-target e))]
    ;(js/alert path)
    ;(.log js/console path)
    (when (and href path (not (= path "/status")))
      (. e preventDefault)
      (. e stopPropagation)
      (. @history (setToken path title))
      (reset! collapsed? true))))

(defn update-page []
  ;(js/alert "update-links")
  (-> "a:not(#navbar-toggle-button):not(.updated)" (keyword) ($)
      (off "click")
      (on "click" #(handle-click-on-link %))
      (add-class "updated")))

; https://groups.google.com/forum/#!topic/clojurescript/5WWfXAf4EDI
; http://stackoverflow.com/questions/27602592/reagent-component-did-mount
(defn page []
  (fn []
    [(with-meta #(do [(pages (session/get :page))])
                {:component-did-mount (fn [this] (update-page))
                 :component-did-update (fn [this] (update-page))}) ]))


;; -------------------------
;; Routes
(defn unhexify
  [s]
  (try
    (js/decodeURIComponent (apply str (map (fn [[x y]] (str "%" x y)) (partition 2 s))))
    (catch js/Error e
      s)))

(defn thread-list-handler
  [response n-list]
   (if (pos? (count n-list))
     (GET
       (str js/context "/api/threads")
       (if (first n-list)
         {:handler #(thread-list-handler % (rest n-list))
          :response-format :json
          :keywords? true
          :params {:n (first n-list)}}
         {:handler #(thread-list-handler % (rest n-list))
          :response-format :json
          :keywords? true})))
  (session/put!
    :thread-list
    [:deb.list-group
     (map
       #(do
         [:a.list-group-item {:href "/"}
          (unhexify (clojure.string/replace (:file-name %) #"^thread_" ""))
          [:span.glyphicon.glyphicon-chevron-right.pull-right]])
       response)]))

;(defn error-handler [{:keys [status status-text]}]
;  (.log js/console (str "something bad happened: " status " " status-text)))

(defn fetch-thread-list! []
  (session/put! :thread-list [:span.glyphicon.glyphicon-refresh.spinning.loading-page])
  (let [n-list '(20 100  nil)]
    (GET (str js/context "/api/threads") {:handler #(thread-list-handler % (rest n-list)) :response-format :json :keywords? true :params {:n (first n-list)}})))

(secretary/defroute "/" [] (session/put! :page :home))
(secretary/defroute "/threads" [] (fetch-thread-list!) (session/put! :page :threads))
(secretary/defroute "/new-posts" [] (session/put! :page :new-posts))
(secretary/defroute "/create-new-thread" [] (session/put! :page :create-new-thread))
(secretary/defroute "/help" [] (session/put! :page :help))





; -------------------------
;; Initialize app
(defn fetch-docs! []
  (GET (str js/context "/docs") {:handler #(session/put! :docs %)}))

(defn mount-components []
  (reagent/render [#'navbar] (.getElementById js/document "navbar"))
  (reagent/render [#'page] (.getElementById js/document "app")))

(defn init! []
  (reset! history (doto (Html5History.)
                    (events/listen
                      EventType/NAVIGATE
                      (fn [event]
                        (secretary/dispatch! (.-token event))))
                    (.setUseFragment false)
                    (.setPathPrefix "")
                    (.setEnabled true)))
  (fetch-docs!)
  (mount-components))
