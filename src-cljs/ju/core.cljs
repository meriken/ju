(ns ju.core
  (:require [reagent.core :as reagent :refer [atom]]
            [reagent.session :as session]
            [secretary.core :as secretary :include-macros true]
            [goog.events :as events]
            [goog.history.EventType :as EventType]
            [markdown.core :refer [md->html]]
            [ajax.core :refer [GET POST]]
            [cljs-time.core]
            [cljs-time.format]
            [cljs-time.coerce]
            [cljs-time.local]
            [goog.crypt.base64])
  (:use     [jayq.core :only [$ parent attr on off html add-class remove-class has-class]])
  (:use-macros [jayq.macros :only [ready]])
  (:import [goog.history Html5History]
            [goog Uri]
            [goog.history EventType]
            [goog.crypt Md5]))



(def page-size 20)
(def jump-command (atom nil))
(def history (atom nil))
(def navbar-collapsed? (atom true))



(defn handle-click-on-link [e]
  (let [$target ($ (.-target e))
        href (.-href (.-target e))
        href (if href href (.-href (.-parentElement (.-target e))))
        path (.getPath (.parse Uri href))
        title (.-title (.-target e))]
    ;(js/alert path)
    ;(.log js/console path)
    (.blur ($ (.-target e)))
    (when (and href path (not (= path "/status")))
      (. e preventDefault)
      (. e stopPropagation)
      (. @history (setToken path title))
      (reset! navbar-collapsed? true)
      (remove-class (parent ($ (keyword "button[data-toggle=dropdown]"))) "open")
      (reset! jump-command
              (cond
                (has-class $target "jump-to-bottom") :bottom
                (has-class (parent $target) "jump-to-bottom") :bottom
                :else :top))
      ;(process-jump-command)
      )))

(defn process-jump-command
  []
  (.log js/console (str "process-jump-command: " @jump-command))
  (cond
    (= @jump-command :top)
    (js/setTimeout #(.scrollTop ($ (keyword "html,body")) 0) 0)
    (= @jump-command :bottom)
    (js/setTimeout #(.scrollTop ($ (keyword "html,body")) (.height ($ js/document))) 0))
  ;(reset! jump-command nil)
  )

(defn uuid
  "returns a type 4 random UUID: xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx"
  []
  (let [r (repeatedly 30 (fn [] (.toString (rand-int 16) 16)))]
    (apply str (concat (take 8 r) ["-"]
                       (take 4 (drop 8 r)) ["-4"]
                       (take 3 (drop 12 r)) ["-"]
                       [(.toString  (bit-or 0x8 (bit-and 0x3 (rand-int 15))) 16)]
                       (take 3 (drop 15 r)) ["-"]
                       (take 12 (drop 18 r))))))

(defn nav-link [uri title page]
  [:li {:class (when (= page (session/get :page)) "active")}
   [:a {:href uri
        :on-click #(reset! navbar-collapsed? true)}
    title]])

(defn navbar []
  (let []
    (fn []
      [:nav.navbar.navbar-default.navbar-fixed-top
       [:div.container
        [:div.navbar-header
         [:button#navbar-toggle-button.navbar-toggle
          {:class         (when-not @navbar-collapsed? "collapsed")
           :data-toggle   "collapse"
           :aria-expanded @navbar-collapsed?
           :aria-controls "navbar"
           :on-click      #(swap! navbar-collapsed? not)}
          [:span.glyphicon.glyphicon-menu-hamburger]
          ;[:span.glyphicon.glyphicon-triangle-bottom]
          ]
         [:a.navbar-brand {:href "/"} "需"]]
        [:div.navbar-collapse.collapse
         (when-not @navbar-collapsed? {:class "in"})
         [:ul.nav.navbar-nav
          [nav-link "/" "目次" :home]
          [nav-link "/threads" "スレッド一覧" :threads]
          [nav-link "/new-posts" "新着レスまとめ読み" :new-posts]
          [nav-link "/create-new-thread" "新規スレッド作成" :create-new-thread]
          [nav-link "/status" "状態" :status]
          [nav-link "/help" "使い方" :help]]]]])))

(defn navbar-bottom
  []
  (fn []
    [:nav.navbar.navbar-default.navbar-fixed-bottom
     [:div.container {:style {:text-align :center}}
      [:div.btn-group.btn-group-sm
       [:a.btn.btn-default.navbar-btn.updated
        {:on-click (fn [e]
                     (reset! jump-command :top)
                     (.back (.-history js/window)))}
        [:span.glyphicon.glyphicon-arrow-left]]
       [:a.btn.btn-default.navbar-btn.updated
        {:on-click (fn [e]
                     (reset! jump-command :top)
                     (.forward (.-history js/window)))}
        [:span.glyphicon.glyphicon-arrow-right]]
       [:a.btn.btn-default.navbar-btn.updated
        [:span.glyphicon.glyphicon-refresh]]
       [:a.btn.btn-default.navbar-btn.updated
        {:on-click (fn [e] (.animate ($ (keyword "html,body")) (clj->js {:scrollTop 0}) "slow"))}
        [:span.glyphicon.glyphicon-chevron-up]]
       [:a.btn.btn-default.navbar-btn.updated
        {:on-click (fn [e] (.animate ($ (keyword "html,body")) (clj->js {:scrollTop (.height ($ js/document))}) "slow"))}
        [:span.glyphicon.glyphicon-chevron-down]]
       [:a.btn.btn-default.navbar-btn.updated
        [:span.glyphicon.glyphicon-pencil] "書込"]]]]))

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
      [:a {:href "/create-new-thread" :class "list-group-item"} "新規スレッド作成" [:span.glyphicon.glyphicon-chevron-right.pull-right]]
      [:a {:href "/status" :class "list-group-item"} "状態" [:span.glyphicon.glyphicon-chevron-right.pull-right]]
      [:a {:href "/help" :class "list-group-item"} "使い方" [:span.glyphicon.glyphicon-chevron-right.pull-right]]]]

    [:div#tag-menu-column.col-sm-6
     [:div#tag-menu.panel.panel-default
      [:div.panel-heading "タグ一覧"]
      [:div.panel-body
       (map (fn [group]
              [:div.btn-group.btn-group-sm {:role "group" :key (uuid)}
               (map #(do [:a.btn.btn-success {:key (uuid)} %]) group)])
            [["質問" "雑談" "ニュース" "実況"]
             ["生活", "料理", "日課"]
             ["画像", "動画", "二次元", "三次元", "18禁"]
             ["趣味", "音楽", "テレビ"]
             ["漫画", "アニメ", "ゲーム", "2ch"]
             ["PC", "ソフトウェア", "ハードウェア"]
             ["開発", "プログラミング", "IT", "P2P"]
             ["新月", "運用", "スレ一覧", "テスト"]
             ["きれいな新月", "裏"]])]]]]

   ])

(defn threads-page []
  [:div.container
   [:h3 "スレッド一覧"]
   [:div#content
    (session/get :thread-list)
    ]])


(defn thread-page []
    [:div.container
     [:h3 (session/get :thread-title)]
     [:div#content
      [:div.btn-group.btn-group-sm.btn-group-justified.page-jump-buttons
       [:a.btn.btn-default.first-page [:span.glyphicon.glyphicon-backward] " 最新"]
       [:a.btn.btn-default.prev-page [:span.glyphicon.glyphicon-triangle-left] "前頁"]
       [:div.btn-group.btn-group-sm {:role "group"}
        [:button.btn.btn-default.dropdown-toggle
         {:data-toggle "dropdown" :aria-haspopup "true" :aria-expanded "false"}
         (if (zero? (session/get :page-num)) "最新" (session/get :page-num)) "頁" [:span.glyphicon.glyphicon-triangle-bottom {:style {:font-size "9px"}}]]
        [:ul.dropdown-menu
         (doall (for [n (range 0 (session/get :num-pages))]
           [:li {:key (uuid)} [:a
                 {:href (str "/thread/"
                             (js/decodeURIComponent (session/get :thread-title))
                             (if (zero? n) "" (str "/p" n)))
                  :on-click handle-click-on-link}
                 (if (zero? n) "最新" n) "頁"]]))]]
       [:a.btn.btn-default.next-page "次頁" [:span.glyphicon.glyphicon-triangle-right]]
       [:a.btn.btn-default.last-page "最初 " [:span.glyphicon.glyphicon-forward]]]

      [:div.btn-group.btn-group-sm.btn-group-justified.page-jump-buttons
       [:a.btn.btn-default.prev-posts.jump-to-bottom "以前のレスを読む"]]
      (session/get :posts)
      [:div.btn-group.btn-group-sm.btn-group-justified.page-jump-buttons
       [:a.btn.btn-default.next-posts "続きのレスを読む"]]

      [:div.btn-group.btn-group-sm.btn-group-justified.page-jump-buttons
       [:a.btn.btn-default.first-page.jump-to-bottom [:span.glyphicon.glyphicon-backward] " 最新"]
       [:a.btn.btn-default.prev-page.jump-to-bottom [:span.glyphicon.glyphicon-triangle-left] "前頁"]
       [:div.btn-group.btn-group-sm.dropup {:role "group"}
        [:button.btn.btn-default.dropdown-toggle
         {:data-toggle "dropdown" :aria-haspopup "true" :aria-expanded "false"}
         (if (zero? (session/get :page-num)) "最新" (session/get :page-num)) "頁" [:span.glyphicon.glyphicon-triangle-top {:style {:font-size "9px"}}]]
        [:ul.dropdown-menu
         (doall (for [n (range 0 (session/get :num-pages))]
           [:li {:key (uuid)} [:a.jump-to-bottom
                 {:href (str "/thread/"
                             (js/decodeURIComponent (session/get :thread-title))
                             (if (zero? n) "" (str "/p" n)))
                  :on-click handle-click-on-link}
                 (if (zero? n) "最新" n) "頁"]]))]]
       [:a.btn.btn-default.next-page.jump-to-bottom "次頁" [:span.glyphicon.glyphicon-triangle-right]]
       [:a.btn.btn-default.last-page.jump-to-bottom "最初 " [:span.glyphicon.glyphicon-forward]]]]])

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
   :thread #'thread-page
   :new-posts #'new-posts-page
   :create-new-thread #'create-new-thread-page
   :help #'help-page})

(defn update-page []
  (let [page-num (session/get :page-num)
        num-pages (session/get :num-pages)
        href-base (str "/thread/" (js/decodeURIComponent (session/get :thread-title)))]
    (-> ($ :.btn.first-page)
        (attr "href" href-base))
    (-> ($ :.btn.prev-page)
        (attr "href" (str href-base (if (> page-num 1) (str "/p" (dec page-num)) ""))))
    (-> ($ :.btn.next-posts)
        (attr "href" (str href-base (if (> page-num 1) (str "/p" (dec page-num)) ""))))
    (-> ($ :.btn.next-page)
        (attr "href" (str href-base "/p" (if (< page-num (dec num-pages)) (inc page-num) (dec num-pages)))))
    (-> ($ :.btn.prev-posts)
        (attr "href" (str href-base "/p" (if (< page-num (dec num-pages)) (inc page-num) (dec num-pages)))))
    (-> ($ :.btn.last-page)
        (attr "href" (str href-base "/p" (dec num-pages))))
    (if (<= page-num 0)
      (do
        (-> ($ :.btn.first-page) (add-class "disabled"))
        (-> ($ :.btn.prev-page) (add-class "disabled")))
      (do
        (-> ($ :.btn.first-page) (remove-class "disabled"))
        (-> ($ :.btn.prev-page) (remove-class "disabled"))))
    (if (>= page-num (dec num-pages))
      (do
        (-> ($ :.btn.last-page) (add-class "disabled"))
        (-> ($ :.btn.next-page) (add-class "disabled")))
      (do
        (-> ($ :.btn.last-page) (remove-class "disabled"))
        (-> ($ :.btn.next-page) (remove-class "disabled"))))
    )

  (-> "a:not(#navbar-toggle-button):not(.updated)" (keyword) ($)
      (off "click")
      (on "click" #(handle-click-on-link %))
      (add-class "updated"))

  (process-jump-command)

  (js/setTimeout
    (fn []
      (.log js/console "update-page"))
    0))

; https://groups.google.com/forum/#!topic/clojurescript/5WWfXAf4EDI
; http://stackoverflow.com/questions/27602592/reagent-component-did-mount
(defn page []
  (fn []
    [(with-meta #(do [(pages (session/get :page))])
                {:component-did-mount (fn [this] (update-page))
                 :component-did-update (fn [this] (update-page))}) ]))



;; -------------------------
;; Routes
(defn file-name-to-path
  [file-name]
  ; TODO: Allow for other application types.
  (try
    (str
      "/thread/"
      (apply str (map (fn [[x y]] (str "%" x y)) (partition 2 (clojure.string/replace file-name #"^thread_" "")))))
    (catch js/Error e
      "/")))

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
    [:div.list-group
     (map
       #(do
         [:a.list-group-item.updated
          {:href (file-name-to-path (:file-name %))
           :on-click (fn [e] (handle-click-on-link e))
           :key (uuid)}
          (unhexify (clojure.string/replace (:file-name %) #"^thread_" ""))
          [:span {:style {:background-color "#ddd"  :border-radius "4px" :margin-left "4px" :font-size 12  :padding "0 6px" :font-weight :normal}} (:num-records %)]
          [:span.glyphicon.glyphicon-chevron-right.pull-right]])
       response)])
  ;(reset! jump-command nil)
  )

(defn fetch-thread-list! []
  (session/put! :thread-list [:span.glyphicon.glyphicon-refresh.spinning.loading-component])
  (let [n-list '(20 100  nil)]
    (GET (str "/api/threads") {:handler #(thread-list-handler % (rest n-list)) :response-format :json :keywords? true :params {:n (first n-list)}})))

(defn posts-handler
  [response]
  (let [num-posts (:num-posts response)
        num-pages (+ (quot num-posts page-size) (if (pos? (rem num-posts page-size)) 1 0))]
    (.log js/console "posts-handler:" num-posts num-pages)
    (session/put! :num-posts num-posts)
    (session/put! :num-pages num-pages)
    (session/put!
      :posts
      [:div#posts
       (map
         (fn [post]
           (let
             [name (and (:name post)
                        (-> (:name post)
                            (clojure.string/replace #"&gt;" ">")
                            (clojure.string/replace #"&lt;" "<")))
              mail (and (:mail post)
                        (-> (:mail post)
                            (clojure.string/replace #"&gt;" ">")
                            (clojure.string/replace #"&lt;" "<")))
              body (drop-last
                     (apply concat
                            (map
                              #(list
                                (-> %
                                    (clojure.string/replace #"&gt;" ">")
                                    (clojure.string/replace #"&lt;" "<"))
                                [:br {:key (uuid)}])
                              (clojure.string/split (:body post) #"<br>"))))
              md5 (Md5.)
              _ (.update md5 (:pubkey post) (count (:pubkey post)))]

             [:div.panel.panel-default.post {:key (uuid)}
              [:div.panel-heading
               [:a.btn.btn-xs.btn-default.id [:span.glyphicon.glyphicon-tag] (take 8 (:record-id post))] " "
               (if (:name post) [:span.name [:span.glyphicon.glyphicon-user] name]) " "
               (if (:mail post) [:span.mail [:span.glyphicon.glyphicon-envelope] mail]) " "
               (if (:pubkey post) [:span.signature [:span.glyphicon.glyphicon-pencil] (take 11 (goog.crypt.base64/encodeByteArray (.digest md5)))]) " "
               [:span.timestamp
                [:span.glyphicon.glyphicon-time]
                (cljs-time.format/unparse
                  (cljs-time.format/formatter "yyyy-MM-dd HH:mm")
                  (cljs-time.core/to-default-time-zone (cljs-time.coerce/from-long (* (:stamp post) 1000))))]]
              [:div.panel-body body]]))
         (:posts response))])))

(defn fetch-posts!
  [thread-title page-num]
  (.log js/console "fetch-posts!:" thread-title page-num)
  (session/put! :posts [:span.glyphicon.glyphicon-refresh.spinning.loading-component])
  (GET (str "/api/thread" )
       {:handler posts-handler
        :response-format :json
        :keywords? true
        :params {:thread-title thread-title :page-num page-num :page-size page-size}}))

(secretary/defroute "/" [] (session/put! :page :home) (reset! jump-command :top) (process-jump-command))
(secretary/defroute "/threads" [] (fetch-thread-list!) (session/put! :page :threads) (reset! jump-command :top))
(secretary/defroute "/new-posts" [] (session/put! :page :new-posts))
(secretary/defroute "/create-new-thread" [] (session/put! :page :create-new-thread))
(secretary/defroute "/help" [] (session/put! :page :help))

(secretary/defroute
  "/thread/:thread-title"
  [thread-title]
  (fetch-posts! thread-title 0)
  (session/put! :thread-title thread-title)
  (session/put! :page-num 0)
  (session/put! :page :thread))

(secretary/defroute
  "/thread/:thread-title/:qualifier"
  [thread-title qualifier]
  (let [page-num (js/parseInt (nth (re-find #"^p([0-9]+)$" qualifier) 1 0))]
    (fetch-posts! thread-title page-num)
    (session/put! :thread-title thread-title)
    (session/put! :page-num page-num)
    (session/put! :page :thread)))





; -------------------------
;; Initialize app
(defn fetch-docs! []
  (GET (str js/context "/docs") {:handler #(session/put! :docs %)}))

(defn mount-components []
  (reagent/render [#'navbar] (.getElementById js/document "navbar"))
  (reagent/render [#'page] (.getElementById js/document "app"))
  (reagent/render [#'navbar-bottom] (.getElementById js/document "navbar-bottom")))

(defn init! []
  (enable-console-print!)
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
