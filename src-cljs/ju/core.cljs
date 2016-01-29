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
            [goog.crypt.base64]
            [goog.string]
            goog.string.format
            [markdown.core]
            [ju.param　:as param]
            [ju.param :as param]
            [ju.param :as param])
  (:use     [jayq.core :only [$ parent attr on off html add-class remove-class has-class ajax]])
  (:use-macros [jayq.macros :only [ready]])
  (:import [goog.history Html5History]
            [goog Uri]
            [goog.history EventType]
            [goog.crypt Md5]))



(declare update-page)
(declare fetch-threads!)
(declare fetch-posts!)
(declare fetch-new-posts!)
(declare generate-html-for-post)
(declare set-title)



(def page-size 20)
(def jump-command (atom :top))
(def history (atom nil))
(def navbar-collapsed? (atom true))
(def navbar-enabled? (atom true))
(def navbar-bottom-enabled? (atom true))
(def post-form-enabled? (atom true))
(def posts-displayed? (atom false))
(def download-thread? (atom false))

(def new-post-notification (atom false))
(def server-status (atom nil))
(def server-node-name (atom nil))
(def server-url-base (atom nil))
(def service-name (atom nil))
(def enable-recaptcha (atom param/enable-recaptcha))
(def recaptcha-site-key (atom param/recaptcha-site-key))
(def recaptcha-secret-key (atom param/recaptcha-secret-key))



(defn remove-tooltips
  []
  (.tooltip ($ (keyword "[data-toggle=\"tooltip\"]")) "hide")
  (.remove ($ :.tooltip)))

(defn open-internal-page
  ([path title new-jump-command]
   (.setToken @history (if (re-find #"\?" path) path (str path "?_=1")) title)
   (reset! navbar-collapsed? true)
   (remove-class (parent ($ (keyword "button[data-toggle=dropdown]"))) "open")
   (remove-tooltips)
   (reset! jump-command new-jump-command)))

(defn handle-click-on-link [e]
  (.log js/console "handle-click-on-link" e)
  (let [$target ($ (.-target e))
        href (.-href (.-target e))
        href (if href href (.-href (.-parentElement (.-target e))))
        path (.getPath (.parse Uri href))
        query-string (.getQuery (.parse Uri href))
        title (.-title (.-target e))]
    ;(js/alert path)
    (.log js/console path)
    (.blur ($ (.-target e)))
    (when (and href path)
      (. e preventDefault)
      (. e stopPropagation)
      (open-internal-page
        (str path (if (pos? (count query-string)) "?" "") query-string)
        title
        (cond
          (has-class $target "jump-to-first-new-post") :first-new-post
          (has-class (parent $target) "jump-to-first-new-post") :first-new-post

          (has-class $target "jump-to-bottom") :bottom
          (has-class (parent $target) "jump-to-bottom") :bottom

          :else :top)))))

(defn process-jump-command
  []
  ;(.log js/console (str "process-jump-command: " @jump-command))
  (cond
    (= @jump-command :bottom)
    (js/setTimeout #(.scrollTop ($ (keyword "html,body")) (.height ($ js/document))) 0)

    (and (= @jump-command :first-new-post)
         (pos? (.-length ($ :.post.new)))
         (pos? (.-length ($ (keyword ".post:not(.new)")))))
    (js/setTimeout
      #(.scrollTop
        ($ js/document)
        (- (.-top (.offset ($ :.post.new)))
           (.outerHeight ($ :.navbar-header))
           10))
      0)

    (or (= @jump-command :top) (= @jump-command :first-new-post))
    (js/setTimeout #(.scrollTop ($ (keyword "html,body")) 0) 0)))

(defn my-uuid
  "returns a type 4 random uuid: xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx"
  []
  (let [r (repeatedly 30 (fn [] (.toString (rand-int 16) 16)))]
    (apply str (concat (take 8 r) ["-"]
                       (take 4 (drop 8 r)) ["-4"]
                       (take 3 (drop 12 r)) ["-"]
                       [(.toString  (bit-or 0x8 (bit-and 0x3 (rand-int 15))) 16)]
                       (take 3 (drop 15 r)) ["-"]
                       (take 12 (drop 18 r))))))



(defn nav-link [uri title page]
  [:li {:class (str
                 (when (= page (session/get :page)) "active")
                 (when (and (= page :new-posts) @new-post-notification) " new-posts"))}
   [:a {:href uri
        :on-click #(do (handle-click-on-link %) (reset! navbar-collapsed? true))}
    title]])

(defn navbar []
  (let []
    (fn []
      [:nav.navbar.navbar-default.navbar-fixed-top
       {:style {:display (if @navbar-enabled? "display" "none")}}
       [:div.container
        [:div.navbar-header
         [:button#navbar-toggle-button.navbar-toggle
          {:class (str
                    (when-not @navbar-collapsed? "collapsed")
                    (when @new-post-notification " new-posts"))
           :data-toggle   "collapse"
           :aria-expanded @navbar-collapsed?
           :aria-controls "navbar"
           :on-click      #(swap! navbar-collapsed? not)}
          [:span.glyphicon.glyphicon-menu-hamburger]
          ;[:span.glyphicon.glyphicon-triangle-bottom]
          ]
         [:a.navbar-brand {:on-click handle-click-on-link :href "/"} @service-name]]
        [:div.navbar-collapse.collapse
         (when-not @navbar-collapsed? {:class "in"})
         [:ul.nav.navbar-nav
          [nav-link "/" "目次" :home]
          [nav-link "/recent-threads" "スレッド一覧" :recent-threads]
          [nav-link "/new-posts" "新着レスまとめ読み" :new-posts]
          ;[nav-link "/create-new-thread" "新規スレッド作成" :create-new-thread]
          [nav-link "/status" "状態" :status]
          [nav-link "/help" "使い方" :help]]]]])))

(defn navbar-bottom
  []
  (fn []
    [:nav.navbar.navbar-default.navbar-fixed-bottom
     {:style {:display (if @navbar-bottom-enabled? "display" "none")}}
     [:div.container {:style {:text-align :center}}
      [:div.btn-group.btn-group-sm
       [:a.btn.btn-default.navbar-btn
        {:on-click (fn [e]
                     (reset! jump-command :top)
                     (.back (.-history js/window)))}
        [:span.glyphicon.glyphicon-arrow-left]]
       [:a.btn.btn-default.navbar-btn
        {:on-click (fn [e]
                     (reset! jump-command :top)
                     (.forward (.-history js/window)))}
        [:span.glyphicon.glyphicon-arrow-right]]
       [:a.btn.btn-default.navbar-btn
        {:on-click (fn [e]
                     (case (session/get :page)
                           :threads
                           (fetch-threads! :threads)
                           :recent-threads
                           (fetch-threads! :recent-threads)
                           :thread
                           (do
                             (reset! jump-command :first-new-post)
                             (fetch-posts! (session/get :thread-title) (session/get :page-num) (session/get :record-short-id)))
                           :new-posts
                           (do
                             (reset! jump-command :top)
                             (fetch-new-posts!))
                           nil
                     ))}
        [:span.glyphicon.glyphicon-refresh]]
       [:a.btn.btn-default.navbar-btn
        {:on-click (fn [e] (.animate ($ (keyword "html,body")) (clj->js {:scrollTop 0}) "slow"))}
        [:span.glyphicon.glyphicon-triangle-top]]
       [:a.btn.btn-default.navbar-btn
        {:on-click (fn [e] (.animate ($ (keyword "html,body")) (clj->js {:scrollTop (.height ($ js/document))}) "slow"))}
        [:span.glyphicon.glyphicon-triangle-bottom]]
       [:a.btn.btn-default.navbar-btn
        {:class (if (and (= (session/get :page) :thread) (not @post-form-enabled?)) "" "disabled")
         :on-click handle-click-on-link
         :href (str (session/get :href-base) "?posts=0&post-form=1")}
        [:span.glyphicon.glyphicon-pencil] "書き込み"]]]]))

(defn home-page []
  [(keyword (str "div.container"
                 (if (not @navbar-enabled?) ".without-navbar")
                 (if (not @navbar-bottom-enabled?) ".without-navbar-bottom")))
   [:h3 @service-name]
   [:p
    "「" @service-name "」は新月ネットワークに参加しているP2P型の匿名掲示板です。"
    [:a {:href "/terms" :on-click handle-click-on-link} "新月ネットワーク利用規約"] "を守った上で、自由に利用してください。"]

   [:div.row
    [:div#main-menu-column.col-sm-6
     [:div#main-menu.list-group
      [:a {:on-click handle-click-on-link :href "/recent-threads" :class "list-group-item"} "最近更新されたスレッド" [:span.glyphicon.glyphicon-chevron-right.pull-right]]
      [:a {:on-click handle-click-on-link :href "/threads" :class "list-group-item"} "全てのスレッド" [:span.glyphicon.glyphicon-chevron-right.pull-right]]
      [:a
       {:on-click handle-click-on-link
        :href     "/new-posts"
        :class    (str "list-group-item"
                       (if @new-post-notification " list-group-item-danger"))}
       "新着レスまとめ読み"
       [:span.glyphicon.glyphicon-chevron-right.pull-right]]
      [:a {:on-click handle-click-on-link :href "/create-new-thread" :class "list-group-item"} "新規スレッド作成" [:span.glyphicon.glyphicon-chevron-right.pull-right]]
      [:a {:on-click handle-click-on-link :href "/status" :class "list-group-item"} "状態" [:span.glyphicon.glyphicon-chevron-right.pull-right]]
      [:a {:on-click handle-click-on-link :href "/help" :class "list-group-item"} "使い方" [:span.glyphicon.glyphicon-chevron-right.pull-right]]]]

    [:div#tag-menu-column.col-sm-6
     [:div#tag-menu.panel.panel-default
      [:div.panel-heading "タグ一覧"]
      [:div.panel-body
       (map (fn [group]
              [:div.btn-group.btn-group-sm {:role "group" :key (my-uuid)}
               (map #(do [:a.btn.btn-success {:key (my-uuid)} %]) group)])
            [["質問" "雑談" "ニュース" "実況"]
             ["生活", "料理", "日課"]
             ["画像", "動画", "二次元", "三次元", "18禁"]
             ["趣味", "音楽", "テレビ"]
             ["漫画", "アニメ", "ゲーム", "2ch"]
             ["PC", "ソフトウェア", "ハードウェア"]
             ["開発", "プログラミング", "IT", "P2P"]
             ["新月", "運用", "スレ一覧", "テスト"]
             ["きれいな新月", "裏"]])]]]]])

(defn recent-threads-page []
  [(keyword (str "div.container"
                 (if (not @navbar-enabled?) ".without-navbar")
                 (if (not @navbar-bottom-enabled?) ".without-navbar-bottom")))
   [:h3 "最近更新されたスレッド"]
   [:div#content
    [:div.btn-group.btn-group-sm.btn-group-justified.refresh-threads-button
    [:a.btn.btn-default
     {:on-click #(do (reset! jump-command :top) (fetch-threads! :recent-threads))}
     [:span.glyphicon.glyphicon-refresh] "スレッド一覧を更新する"]]
    (session/get :recent-threads)
    [:div.btn-group.btn-group-sm.btn-group-justified.refresh-threads-button
     [:a.btn.btn-default
     {:on-click handle-click-on-link
      :href "/threads"}
     "全てのスレッドを表示する"]]
    ]])

(defn threads-page []
  [(keyword (str "div.container"
                 (if (not @navbar-enabled?) ".without-navbar")
                 (if (not @navbar-bottom-enabled?) ".without-navbar-bottom")))
   [:h3 "全てのスレッド"]
   [:div#content
    [:div.btn-group.btn-group-sm.btn-group-justified.refresh-threads-button
    [:a.btn.btn-default
     {:on-click #(do (reset! jump-command :top) (fetch-threads! :threads))}
     [:span.glyphicon.glyphicon-refresh] "スレッド一覧を更新する"]]
    (session/get :threads)]])

(defn top-page-jump-buttons
  []
  (fn []
      [:span
       ;[:div.btn-group.btn-group-sm.btn-group-justified.page-jump-buttons
       ; [:a.btn.btn-default
       ;  {:on-click handle-click-on-link
       ;   :href (str (session/get :href-base) "?download-thread=1")}
       ;  "このスレッドをダウンロード"]]

       [:div.btn-group.btn-group-sm.btn-group-justified.page-jump-buttons
       [:a.btn.btn-default.first-page
        {:on-click handle-click-on-link
         :href (session/get :href-base)
         :class (if (<= (session/get :page-num) 0) "disabled" "")}
        [:span.glyphicon.glyphicon-backward] " 最新"]
       [:a.btn.btn-default.prev-page
        {:on-click handle-click-on-link
         :href (str (session/get :href-base) (if (> (session/get :page-num) 1) (str "/p" (dec (session/get :page-num))) ""))
         :class (if (<= (session/get :page-num) 0) "disabled" "")}
        [:span.glyphicon.glyphicon-triangle-left] "前頁"]
       [:div.btn-group.btn-group-sm {:role "group"}
        [:button.btn.btn-default.dropdown-toggle
         {:data-toggle "dropdown" :aria-haspopup "true" :aria-expanded "false"
          :class (if (<= (dec (session/get :num-pages)) 1) "disabled" "")}
         (if (zero? (session/get :page-num)) "最新" (session/get :page-num)) "頁" [:span.glyphicon.glyphicon-triangle-bottom {:style {:font-size "9px"}}]]
        [:ul.dropdown-menu
         (doall (for [n (range 0 (session/get :num-pages))]
                  [:li {:key (my-uuid)} [:a
                                      {:href (str (session/get :href-base)
                                                  (if (zero? n) "" (str "/p" n)))
                                       :on-click handle-click-on-link}
                                      (if (zero? n) "最新" n) "頁"]]))]]
       [:a.btn.btn-default.next-page
        {:on-click handle-click-on-link
         :href (str (session/get :href-base) "/p"
                    (if (< (session/get :page-num) (dec (session/get :num-pages)))
                      (inc (session/get :page-num))
                      (dec (session/get :num-pages))))
         :class (if (>= (session/get :page-num) (dec (session/get :num-pages))) "disabled" "")}
        "次頁" [:span.glyphicon.glyphicon-triangle-right]]
       [:a.btn.btn-default.last-page
        {:on-click handle-click-on-link
         :href (str (session/get :href-base) "/p" (dec (session/get :num-pages)))
         :class (if (>= (session/get :page-num) (dec (session/get :num-pages))) "disabled" "")}
        "最初 " [:span.glyphicon.glyphicon-forward]]]
       [:div.btn-group.btn-group-sm.btn-group-justified.page-jump-buttons
        [:a.btn.btn-default.prev-posts.jump-to-bottom
         {:on-click handle-click-on-link
          :href (str (session/get :href-base) "/p"
                     (if (< (session/get :page-num) (dec (session/get :num-pages)))
                       (inc (session/get :page-num))
                       (dec (session/get :num-pages))))
          :class (if (>= (session/get :page-num) (dec (session/get :num-pages))) "disabled" "")}
         "前のレスを読む"]]]))

(defn bottom-page-jump-buttons
  []
    [:span
     [:div.btn-group.btn-group-sm.btn-group-justified.page-jump-buttons
      [:a.btn.btn-default.next-posts
       {:on-click handle-click-on-link
        :href (str (session/get :href-base) (if (> (session/get :page-num) 1) (str "/p" (dec (session/get :page-num))) ""))
        :class (if (<= (session/get :page-num) 0) "disabled" "")}
       "続きのレスを読む"]]
    [:div.btn-group.btn-group-sm.btn-group-justified.page-jump-buttons
     [:a.btn.btn-default.first-page.jump-to-bottom
      {:on-click handle-click-on-link
       :href (session/get :href-base)
       :class (if (<= (session/get :page-num) 0) "disabled" "")}
      [:span.glyphicon.glyphicon-backward] " 最新"]
     [:a.btn.btn-default.prev-page.jump-to-bottom
      {:on-click handle-click-on-link
       :href (str (session/get :href-base) (if (> (session/get :page-num) 1) (str "/p" (dec (session/get :page-num))) ""))
       :class (if (<= (session/get :page-num) 0) "disabled" "")}
      [:span.glyphicon.glyphicon-triangle-left] "前頁"]
     [:div.btn-group.btn-group-sm.dropup {:role "group"}
      [:button.btn.btn-default.dropdown-toggle
       {:data-toggle "dropdown" :aria-haspopup "true" :aria-expanded "false"
        :class (if (<= (dec (session/get :num-pages)) 1) "disabled" "")}
       (if (zero? (session/get :page-num)) "最新" (session/get :page-num)) "頁" [:span.glyphicon.glyphicon-triangle-top {:style {:font-size "9px"}}]]
      [:ul.dropdown-menu
       (doall (for [n (range 0 (session/get :num-pages))]
                [:li {:key (my-uuid)} [:a.jump-to-bottom
                                    {:href (str (session/get :href-base)
                                                (if (zero? n) "" (str "/p" n)))
                                     :on-click handle-click-on-link}
                                    (if (zero? n) "最新" n) "頁"]]))]]
     [:a.btn.btn-default.next-page.jump-to-bottom
      {:on-click handle-click-on-link
       :href (str (session/get :href-base) "/p" (if (< (session/get :page-num) (dec (session/get :num-pages))) (inc (session/get :page-num)) (dec (session/get :num-pages))))
       :class (if (>= (session/get :page-num) (dec (session/get :num-pages))) "disabled" "")}
      "次頁" [:span.glyphicon.glyphicon-triangle-right]]
     [:a.btn.btn-default.last-page.jump-to-bottom
      {:on-click handle-click-on-link
       :href (str (session/get :href-base) "/p" (dec (session/get :num-pages)))
       :class (if (>= (session/get :page-num) (dec (session/get :num-pages))) "disabled" "")}
      "最初 " [:span.glyphicon.glyphicon-forward]]]])

; This is rather tricky.
(defn recaptcha []
  (fn []
    [(with-meta #(if @enable-recaptcha [:div#g-recaptcha.g-recaptcha {:data-sitekey @recaptcha-site-key}])
                {:component-did-mount (fn [this]
                                        (if @enable-recaptcha
                                          (try
                                            (.render js/grecaptcha
                                                     "g-recaptcha"
                                                     (clj->js {:sitekey @recaptcha-site-key}))
                                            (.removeClass ($ :#g-recaptcha) "g-recaptcha")
                                            (catch js/Error _))))})]))

(defn submit-post
  [e]
  (.preventDefault e)
  (let [result (atom nil)
                _ (ajax "/api/post"
                        {:method      "POST"
                         :success     (fn [response] (reset! result (clojure.walk/keywordize-keys (js->clj response))))
                         :error       (fn [] (reset! result :error))
                         :async       false
                         :contentType false
                         :processData false
                         :data        (js/FormData. (.getElementById js/document "post-form"))})]
    (if (= @result :error)
      (.show js/BootstrapDialog (clj->js
                                  {:type (.-TYPE_DANGER js/BootstrapDialog)
                                   :title "エラー"
                                   :message "書き込みに失敗しました。"
                                   :buttons (clj->js [ (clj->js { :label "閉じる" :action #(.close %) })])}))
      (open-internal-page
        (str "/thread/" (js/decodeURIComponent (session/get :thread-title)))
        (session/get :thread-title)
        :first-new-post))))

(defn post-form
  []
  (fn []
    [:form#post-form
     {:style {:display (if @post-form-enabled? "display" "none")}}
     [:input {:type "hidden" :name "thread-title" :value (session/get :thread-title)}]
     [:div
      [:ul#post-form-tabs.nav.nav-tabs {:style {:border-bottom "none"}}
       [:li#post-form-edit-tab.active {:on-click #(js/switchTabsForPostForm "#post-form-edit-tab") :role "presentation"} [:a "編集"]]
       [:li#post-form-emoji-tab {:on-click #(js/switchTabsForPostForm "#post-form-emoji-tab") :role "presentation"} [:a "絵文字入力"]]
       [:li#post-form-preview-tab {:on-click #(js/switchTabsForPostForm "#post-form-preview-tab") :role "presentation"} [:a "プレビュー"]]]
      [:div.input-wrapper
       [:div.wrapped-input.input-group
        [:span.no-border.input-group-addon {:style {:border-radius "4px 0 0 0"}} [:span.glyphicon.glyphicon-user ]]
        [:input#name.no-border.form-control {:style {:border-radius "4px 0 0 0"} :name "name" :placeholder "名前"}]]
       [:div.wrapped-input.input-group
        [:span.no-border.input-group-addon [:span.glyphicon.glyphicon-envelope ]]
        [:input#mail.no-border.form-control {:name "mail" :placeholder "メール"}]]
     [:div.wrapped-input.input-group
      [:span.no-border.input-group-addon [:span.glyphicon.glyphicon-pencil ]]
      [:input#password.no-border.form-control {:type "password" :name "password" :placeholder "署名"}]]
       [:div.wrapped-input.input-group
        [:span.no-border.input-group-addon [:span.glyphicon.glyphicon-paperclip ]]
        [:span.no-border.btn.btn-block.btn-default.btn-file
         [:img {:alt "" :src "" :style {:max-height "80px" :max-width "80px"}} [:span#attachment-file-name "添付ファイル"]
          [:input#attachment {:type "file" :multiple "" :name "attachment"}]]]]
       [:textarea#body.no-border.wrapped-input.form-control {:rows "5" :name "body" :placeholder "本文"}]
       [:div#post-preview {:style {:display "none"}} "プレビュー"]
       [:button.btn.btn-block.btn-primary.wrapped-input.no-border {:on-click submit-post :style {:border-radius "0 0 4px 4px"}} "書き込む"]]]
     [recaptcha]
     [:a.btn.btn-default {:href "/terms" :on-click handle-click-on-link :style {:margin-bottom "10px"}} "新月ネットワーク利用規約"]]))

(defn highlight-code-block
  []
  (.each ($ (keyword "pre code:not(.highlighted)"))
         (fn [i block]
           (.highlightBlock js/hljs block)
           (.addClass ($ block) "highlighted"))))

;TODO: Do something with attachment.
(defn ^:export render-preview
  []
  (html ($ :#post-preview)
        (reagent.core/render-component-to-string
          (generate-html-for-post
            {:record-id (apply str (repeat 32 "?"))
             :record-short-id (apply str (repeat 8 "?"))
             :name  (goog.string/htmlEscape (.val ($ :input#name)))
             :mail  (goog.string/htmlEscape (.val ($ :input#mail)))
             :stamp (int (/ (cljs-time.coerce/to-long (cljs-time.core/now)) 1000))
             :body  (clojure.string/replace
                      (goog.string/htmlEscape (.val ($ :textarea#body)))
                      #"\n"
                      "<br>")}
            :preview
            (session/get :thread-title)
            '())))
  (js/setTimeout #(highlight-code-block) 0))

(defn thread-page []
    [(keyword (str "div.container"
                   (if (not @navbar-enabled?) ".without-navbar")
                   (if (not @navbar-bottom-enabled?) ".without-navbar-bottom")))
     [:a.jump-to-first-new-post
      {:on-click handle-click-on-link
       :href (session/get :href-base)}
      [:h3 (session/get :thread-title)]]
     [:div#content
      (if (and @posts-displayed? (session/get :page-num))
        [#'top-page-jump-buttons])
      (if @posts-displayed?
        (session/get :posts))
      (if (and @posts-displayed? (session/get :page-num))
        [#'bottom-page-jump-buttons])]
     [#'post-form]])

(defn new-posts-page []
  [(keyword (str "div.container"
                 (if (not @navbar-enabled?) ".without-navbar")
                 (if (not @navbar-bottom-enabled?) ".without-navbar-bottom")))
   [:h3 "新着レスまとめ読み"]
   [:div#content
    (session/get :posts)]])

(defn create-new-thread-page []
  [(keyword (str "div.container"
                 (if (not @navbar-enabled?) ".without-navbar")
                 (if (not @navbar-bottom-enabled?) ".without-navbar-bottom")))
   [:h3 "新規スレッド作成"]
   [:div#content]])

(defn help-page []
  [(keyword (str "div.container"
                 (if (not @navbar-enabled?) ".without-navbar")
                 (if (not @navbar-bottom-enabled?) ".without-navbar-bottom")))
   [:h3 "使い方"]
   [:div#content
    "このページは現在作成中です。"]])

(defn terms-page []
  [(keyword (str "div.container"
                 (if (not @navbar-enabled?) ".without-navbar")
                 (if (not @navbar-bottom-enabled?) ".without-navbar-bottom")))
   [:h3 "新月ネットワーク利用規約"]
   [:div#content
    "新月 - P2P anonymous BBS" [:br]
    "http://shingetsu.info/" [:br] [:br]
    "次の利用規約に同意した方のみ新月ネットワークに参加できます。" [:br] [:br]
    "(投稿者の責任または免責)" [:br]
    "1. 投稿者は投稿した記事に使用、改変または再配布の条件を記述しなければならない。"  [:br]
    [:span {:dangerouslySetInnerHTML {:__html "&nbsp;&nbsp;&nbsp;&nbsp;"}}] "条件は新月の仕組みに矛盾するものであってはならない。" [:br]
    "2. 第1項の条件の記述がない場合には、利用者は投稿者が使用、" [:br]
    [:span {:dangerouslySetInnerHTML {:__html "&nbsp;&nbsp;&nbsp;&nbsp;"}}] "改変または再配布を制限しないことに同意したものとみなすことができる。" [:br]
    "3. 投稿者は第1項の条件または第2項の同意が正しいことに責任を持つ。" [:br]
    "4. 投稿者は法律に定めのない限り、個別の記事で宣言しない限り、" [:br]
    [:span {:dangerouslySetInnerHTML {:__html "&nbsp;&nbsp;&nbsp;&nbsp;"}}] "かつ第3項に反しない限り、記事の内容が正しいこと、役に立つこと、" [:br]
    [:span {:dangerouslySetInnerHTML {:__html "&nbsp;&nbsp;&nbsp;&nbsp;"}}] "または不愉快でないことなどについて保証しない。" [:br] [:br]
    "(ノード管理者の責任または免責)" [:br]
    "5. ノード管理者は記事または掲示板を自由に編集または削除できる。" [:br]
    "6. ノード管理者は法律に定めのない限り、" [:br]
    [:span {:dangerouslySetInnerHTML {:__html "&nbsp;&nbsp;&nbsp;&nbsp;"}}] "ノードを管理・運営することで知った情報についての守秘義務を負わない。" [:br]
    "7. ノード管理者は法律に定めのない限り、記事の内容が正しいこと、役に立つこと、" [:br]
    [:span {:dangerouslySetInnerHTML {:__html "&nbsp;&nbsp;&nbsp;&nbsp;"}}] "または不愉快でないことなどについて保証しない。" [:br]
    "8. ノード管理者は自分の管理するノードに対して、" [:br]
    [:span {:dangerouslySetInnerHTML {:__html "&nbsp;&nbsp;&nbsp;&nbsp;"}}] "特定のユーザ、特定のノード、全ての利用者または全てのノードが" [:br]
    [:span {:dangerouslySetInnerHTML {:__html "&nbsp;&nbsp;&nbsp;&nbsp;"}}] "一時的または永続的に接続できることを保証しない。"]])

(defn status-page []
  [(keyword (str "div.container"
                 (if (not @navbar-enabled?) ".without-navbar")
                 (if (not @navbar-bottom-enabled?) ".without-navbar-bottom")))
   [:h3 "状態"]
   [:div#content
    "ファイルの数: " (:num-files @server-status) [:br]
    "レコードの数: " (+ (:num-records @server-status) (:num-deleted-records @server-status)) [:br]
    "有効なレコードの数: " (:num-records @server-status) [:br]
    "削除されたレコードの数: " (:num-deleted-records @server-status) [:br]
    "キャッシュサイズ: " (int (/ (:cache-size @server-status) 1000 1000)) "MB" [:br]
    "自分自身のノード名: " @server-node-name [:br]
    [:br]
    "隣接ノード" [:br]
    (map #(do [:span {:key (my-uuid)} % [:br]]) (:active-nodes @server-status))
    "計" (str (count (:active-nodes @server-status))) "台" [:br]
    [:br]
    "探索ノード" [:br]
    (map #(do [:span {:key (my-uuid)} % [:br]]) (:search-nodes @server-status))
    "計" (str (count (:search-nodes @server-status))) "台"
    ]])

(def pages
  {:home #'home-page
   :threads #'threads-page
   :recent-threads #'recent-threads-page
   :thread #'thread-page
   :new-posts #'new-posts-page
   :create-new-thread #'create-new-thread-page
   :status #'status-page
   :help #'help-page
   :terms #'terms-page})

; https://groups.google.com/forum/#!topic/clojurescript/5WWfXAf4EDI
; http://stackoverflow.com/questions/27602592/reagent-component-did-mount
(defn page []
  (fn []
    [(with-meta #(do [(pages (session/get :page))])
                {:component-did-mount (fn [this] (update-page))
                 :component-did-update (fn [this] (update-page))}) ]))



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
  [response dest]
  (session/put!
    dest
    [:div#threads.list-group
     (map
       #(let
         [thread-title (unhexify (clojure.string/replace (:file-name %) #"^thread_" ""))
          thread-last-accessed (.getItem js/localStorage (str "thread-last-accessed-" thread-title))
          new-posts? (and thread-last-accessed (> (:time-updated %) (js/parseInt thread-last-accessed)))]

         [:a.list-group-item.jump-to-first-new-post
          {:href (file-name-to-path (:file-name %))
           :on-click handle-click-on-link
           :key (my-uuid)
           :class (cond
                    new-posts? "list-group-item-danger"
                    thread-last-accessed "list-group-item-info"
                    :else "")}
          thread-title ; " (ID:" (str (:id %)) ")"
          [:span {:style {:border "solid 1px #999"
                          :background-color "#eee"
                          :color "#000"
                          :opacity 0.7
                          :border-radius "4px"
                          :margin-left "4px"
                          :font-size 11
                          :padding "0 6px"
                          :font-weight :normal}}
           (:num-records %)]
          [:span.glyphicon.glyphicon-chevron-right.pull-right]])
       response)]))

(defn update-threads
  [dest]
  (GET "/api/threads"
       {:handler #(thread-list-handler % dest)
        :response-format :json
        :keywords? true
        :params {:n (if (= dest :recent-threads) 100 nil)}}))

(defn fetch-threads! [dest]
  ;(session/put! dest [:span.glyphicon.glyphicon-refresh.spinning.loading-component])
  (session/put! dest nil)
  (session/put! :page dest)
  (update-threads dest))

(defn process-anchors
  [s thread-title]
  (let [match (re-find #"^(.*?)>>([0-9a-f]{8})(.*)$" s)]
    (if-not match
      s
      (concat [(nth match 1)
               [:a.btn.btn-default.btn-sm.anchor
                {;:href (str (session/get :href-base) "/" (nth match 2))
                 ;:on-click handle-click-on-link
                 :key (my-uuid)
                 :data-toggle "tooltip"
                 :data-container "body"
                 :data-thread-title thread-title
                 :data-record-short-id (nth match 2)}
                ">>" (nth match 2)]]
              (process-anchors (nth match 3) thread-title)))))

(defn process-bracket-links
  [s]
  (let [match (re-find #"^(.*?)\[\[([^/\]]+)(/[0-9a-f]{8})?\]\](.*)$" s)]
    (if-not match
      s
      (concat [(nth match 1)
               (if (and (nth match 3) (pos? (count (nth match 3))))
                 [:a.btn.btn-default.btn-sm.bracket-link.anchor
                  {:href     (str "/thread/" (js/decodeURIComponent (nth match 2)))
                   :on-click handle-click-on-link
                   :key      (my-uuid)
                   :data-toggle "tooltip"
                   :data-container "body"
                   :data-thread-title (nth match 2)
                   :data-record-short-id (clojure.string/replace (nth match 3) #"^/" "")}
                  "[[" (nth match 2) (nth match 3) "]]"]
                 [:a.btn.btn-default.btn-sm.bracket-link
                  {:href     (str "/thread/" (js/decodeURIComponent (nth match 2)))
                   :on-click handle-click-on-link
                   :key      (my-uuid)}
                  "[[" (nth match 2) "]]"])]
              (process-bracket-links (nth match 4))))))

(defn launch-image-viewer
  [src]
  (let [links (clj->js [src])
        options (clj->js {:useBootstrapModal false
                          :hidePageScrollbars false})]
    (.toggleClass ($ :#blueimp-gallery) "blueimp-gallery-controls" true)
    (.Gallery js/blueimp links options)))

(defn process-links
  [s]
  (let [match (re-find #"^(.*?)([htps]+://(www\.)?[-a-zA-Z0-9@:%._\+~#=]{2,256}\.[a-z]{2,6}\b([-a-zA-Z0-9@:%_\+.~#?&//=]*))(.*)$" s)]
    (if-not match
      s
      (concat [(nth match 1)
                (if false ; (re-find #"\.(jpe?g|gif|bmp|png)$" (nth match 2))
                    [:img {:src (nth match 2) :on-click #(launch-image-viewer (nth match 2)) :height 210}]
                    [:a {:href (nth match 2) :target "_blank" :key (my-uuid)} (nth match 2)])]
               (process-links (last match))))))

; Not particularly sophisticated, but it works.
(defn thread-title-to-file-name
  [thread-title]
  (str "thread_"
       (-> (js/encodeURIComponent thread-title)
           (clojure.string/replace #"[^%]{2}[-A-Za-z0-9_.~]" (fn [s] (str (apply str (drop-last s)) "%" (clojure.string/upper-case (.toString (.charCodeAt (last s) 0) 16)))))
           (clojure.string/replace #"[^%]{2}[-A-Za-z0-9_.~]" (fn [s] (str (apply str (drop-last s)) "%" (clojure.string/upper-case (.toString (.charCodeAt (last s) 0) 16)))))
           (clojure.string/replace #"[^%]{2}[-A-Za-z0-9_.~]" (fn [s] (str (apply str (drop-last s)) "%" (clojure.string/upper-case (.toString (.charCodeAt (last s) 0) 16)))))
           (clojure.string/replace #"^[^%]?[-A-Za-z0-9_.~]" (fn [s] (str (apply str (drop-last s)) "%" (clojure.string/upper-case (.toString (.charCodeAt (last s) 0) 16)))))
           (clojure.string/replace #"^[^%]?[-A-Za-z0-9_.~]" (fn [s] (str (apply str (drop-last s)) "%" (clojure.string/upper-case (.toString (.charCodeAt (last s) 0) 16)))))
           (clojure.string/replace  #"%" ""))))

(defn generate-html-for-post
  [post context thread-title anchors]
  (let
    [name (and (:name post)
               (goog.string/unescapeEntities (:name post)))
     mail (and (:mail post)
               (goog.string/unescapeEntities (:mail post)))
     body (if (and (:body post) (re-find #"^@markdown<br>" (:body post)))
            [:span.markdown {:dangerouslySetInnerHTML {:__html
                                              (markdown.core/md->html
                                                (-> (:body post)
                                                    (clojure.string/replace  #"^@markdown<br>" "")
                                                    (clojure.string/replace  #"<br>" "\n")
                                                    (goog.string/unescapeEntities)))}}]
            (->> (drop-last
                   (apply concat
                          (map
                            #(list
                              (goog.string/unescapeEntities %)
                              [:br {:key (my-uuid)}])
                            (clojure.string/split (:body post) #"<br>"))))
                 (map #(if (string? %) (process-links %) %))
                 (map #(if (string? %) (process-anchors % thread-title) %))
                 (map #(if (string? %) (process-bracket-links %) %))))
     md5 (Md5.)
     _ (.update md5 (:pubkey post) (count (:pubkey post)))
     href-base (str "/thread/" (js/encodeURIComponent thread-title))
     src (str href-base "/"
              (:record-id post)
              "." (:suffix post))
     complete-src (str @server-url-base src)
     body-exists? (pos? (count body))
     thumbnail-exists? (and (:suffix post) (re-find #"^(jpe?g|png|gif|bmp)$" (:suffix post)))
     ascii2d-form-id (my-uuid)
     tineye-form-id (my-uuid)
     heading [{:style {:vertical-align "middle"}}
              (if (not (= context :preview))
                [:div.btn-group.btn-group-sm.pull-right
                 [:a.btn.btn-default
                  {:href (str "/thread/" (js/encodeURIComponent thread-title) "/" (:record-short-id post) "?post-form=1")
                   :on-click handle-click-on-link}
                  [:span.glyphicon.glyphicon-pencil
                   {:style {:margin-left "2px"
                            :margin-right "2px"
                            :font-size "12px"}}]]
                 [:a.btn.btn-default.dropdown-toggle
                  {:data-toggle "dropdown" :aria-haspopup "true" :aria-expanded "false"}
                  [:span.glyphicon.glyphicon-cog
                   {:style {:font-size "12px"}}]
                  [:span.glyphicon.glyphicon-triangle-bottom
                   {:style {:font-size "8px"
                            :margin-left "2px"}}]]
                 [:ul.dropdown-menu
                  [:li [:a
                        {:href (str "/thread/" (js/encodeURIComponent thread-title) "/" (:record-short-id post) "?post-form=1")
                         :on-click handle-click-on-link}
                        "このレスに返信する"]]
                  [:li [:a
                        {:href (str "/thread/" (js/encodeURIComponent thread-title) "/" (:record-short-id post) "?post-form=1")
                         :on-click handle-click-on-link}
                        "このレスに返信する(引用付き)"]]
                  [:li.divider {:role "separator"}]
                  (map #(do
                         [:li {:key (my-uuid)}
                          [:a
                               {:href (str % (js/encodeURIComponent thread-title) "/" (:record-short-id post))
                                :target "_blank"}
                               "このレスを" (nth (re-find #"^https?://([^/:]+)(:[0-9]+)?/.*/$" %) 1 %) "で開く"]])
                       ["http://bbs.shingetsu.info/thread.cgi/"
                        "http://rep4649.ddo.jp:8000/thread.cgi/"
                        "http://opptape.iobb.net:8000/thread.cgi/"])
                  [:li.divider {:role "separator"}]
                  [:li [:a
                   {:href (str "/server/get/"
                               (thread-title-to-file-name thread-title)
                               "/" (:stamp post)
                               "/" (:record-id post))
                    :target "_blank"}
                   "このレスのレコードを表示する"]]
                  (if thumbnail-exists?
                    [:li.divider {:role "separator"}])
                  (if thumbnail-exists?
                    [:li [:a
                          {:src src
                           :on-click #(launch-image-viewer src)}
                          "添付画像を表示する"]])
                  (if (and thumbnail-exists? complete-src)
                    [:li [:a
                          {:href (str "https://images.google.co.jp/searchbyimage?image_url="
                                      (js/encodeURIComponent complete-src))
                           :target "_blank"}
                          "「Google」で画像検索"]])
                  (if (and thumbnail-exists? complete-src)
                    [:li
                     [:form
                      {:id ascii2d-form-id :action "https://www.ascii2d.net/imagesearch/search" :method "POST" :target "_blank"}
                      [:input {:type "hidden" :name "uri" :value complete-src}]]
                     [:a
                      {:on-click #(.submit (aget ($ (keyword (str "#" ascii2d-form-id))) 0)) :target "_blank"}
                      "「二次元画像詳細検索」で画像検索"]])
                  (if (and thumbnail-exists? complete-src)
                    [:li
                     [:form
                      {:id tineye-form-id :action "https://www.tineye.com/search" :method "POST" :target "_blank"}
                      [:input {:type "hidden" :name "url" :value complete-src}]]
                     [:a
                      {:on-click #(.submit (aget ($ (keyword (str "#" tineye-form-id))) 0)) :target "_blank"}
                      "「TinEye」で画像検索"]])
                  ]])
              [:div
               {:style {:vertical-align "middle"}}
               [:a.btn.btn-xs.btn-default.id
                {:href (str href-base "/" (:record-short-id post))
                 :on-click handle-click-on-link}
                [:span.glyphicon.glyphicon-tag] " " (:record-short-id post)] " "
               (if (and (:name post) (pos? (count (:name post)))) [:span.name [:span.glyphicon.glyphicon-user] name]) " "
               (if (and (:mail post) (pos? (count (:mail post)))) [:span.mail [:span.glyphicon.glyphicon-envelope] mail]) " "
               (if (:pubkey post) [:span.signature [:span.glyphicon.glyphicon-pencil] (take 11 (goog.crypt.base64/encodeByteArray (.digest md5)))]) " "

               [:span.timestamp
                [:span.glyphicon.glyphicon-time]
                (cljs-time.format/unparse
                  (cljs-time.format/formatter "yyyy-MM-dd HH:mm:ss")
                  (cljs-time.core/to-default-time-zone (cljs-time.coerce/from-long (* (:stamp post) 1000))))] " "
               (if (:suffix post)
                 [:a.btn.btn-xs.btn-default.attachment
                  {:href src}
                  [:span.glyphicon.glyphicon-paperclip] (str " " (clojure.string/replace (:record-id post) #".{16}$" "…") "." (:suffix post))])]

              ]
     reverse-anchors (remove nil? (map #(if (= (:destination %) (:record-short-id post)) (:source %) nil) anchors))
     body-with-image [body
                      (if (and body-exists? thumbnail-exists?) [:hr])
                      (if thumbnail-exists?
                        [:div {:style {:display "flex" :justify-content "center":align-items "center":height 210 :width "100%"}}
                               [:img {:style {:max-height 210 :max-width "100%" :width "auto" :height "auto"}
                                      :src src
                                      :on-click #(launch-image-viewer src)}]])
                      (if (pos? (count reverse-anchors))
                        [:div.reverse-anchors [:hr]
                         [:div
                          (map #(do [:a.btn.btn-default.btn-sm.anchor.reverse-anchor
                                     {:data-record-short-id %
                                      :data-thread-title thread-title
                                      :key (my-uuid)}
                                     "└" %])
                               reverse-anchors)]])]
     thread-last-accessed (.getItem js/localStorage (str "thread-last-accessed-" thread-title))
     new-post? (or (nil? thread-last-accessed) (> (:stamp post) (js/parseInt thread-last-accessed)))]

    (if (and (= context :thread)
             (nil? (session/get :record-short-id))
             (or (nil? (.getItem js/localStorage (str "thread-last-accessed-" thread-title)))
                 (> (:stamp post) (js/parseInt (.getItem js/localStorage (str "thread-last-accessed-" thread-title))))))
      (.setItem js/localStorage
                (str "thread-last-accessed-" thread-title)
                (str (:stamp post))))

    (case context
      :popup
      [:div.popup {:key (my-uuid)}
       (into [] (concat [:div.well.well-sm.popup-heading] heading))
       (into [] (concat [:div] body-with-image))]

      [:div.panel.post {:key (my-uuid) :class (if (and new-post? (= context :thread)) "panel-danger new" "panel-default")}
       (into [] (concat [:div.panel-heading] heading))
       (into [] (concat [(if (pos? (count reverse-anchors)) :div.panel-body.with-reverse-anchors :div.panel-body)] body-with-image))])))

(defn posts-handler
  [response]
  (let [num-posts (:num-posts response)
        num-pages (+ (quot num-posts page-size) (if (pos? (rem num-posts page-size)) 1 0))]
    ;(.log js/console "posts-handler:" num-posts num-pages (clj->js (:anchors response)))
    (session/put! :num-posts num-posts)
    (session/put! :num-pages num-pages)
    (session/put!
      :posts
      [(with-meta (fn [] [:div#posts
                          (doall
                            (map
                              #(generate-html-for-post % :thread (session/get :thread-title) (:anchors response))
                              (:posts response)))])
                  {:component-did-mount
                   #(do
                     (process-jump-command))})])))

(defn new-posts-handler
  [response]
  (let []
    ;(.log js/console "new-posts-handler:" num-posts num-pages (clj->js (:anchors response)))
    (session/put!
      :posts
      [(with-meta (fn []
                    (if (zero? (count (:threads response)))
                      [:div.alert.alert-info "既読スレッドの新着レスはありません。"]
                      [:div#new-posts
                       (map
                         (fn [thread]
                           [:div.thread
                            [:a
                             {:href (str "/thread/" (js/encodeURIComponent (:thread-title thread)))
                              :on-click handle-click-on-link}
                             [:div.alert.alert-info.thread-title (:thread-title thread)]]
                            (map
                              (fn [post]
                                (generate-html-for-post post :new-posts (:thread-title thread) (:anchors thread)))
                              (:posts thread))])
                         (:threads response))]))
                  {:component-did-mount
                   (fn []
                     (reset! new-post-notification false)
                     (dorun
                       (map
                         #(.setItem js/localStorage
                                    (str "thread-last-accessed-" (:thread-title %))
                                    (str (long (/ (.getTime (js/Date.)) 1000))))
                         (:threads response))))})])))

(defn posts-error-handler
  [response]
  (let []
    (session/put! :num-posts 0)
    (session/put! :num-pages 0)
    (session/put!
      :posts
      [:div#posts
       [:div.alert.alert-danger {:role "alert"}
        [:span.glyphicon.glyphicon-exclamation-sign]
        "レスの読み込みに失敗しました。" [:br]
        (str response)]])))

(defn fetch-posts!
  [thread-title page-num record-short-id]
  (.log js/console "fetch-posts!:" thread-title page-num record-short-id)
  (if @download-thread?
    (session/put! :posts [:span.glyphicon.glyphicon-refresh.spinning.loading-component])
    (session/put! :posts nil))
  (POST (str "/api/thread" )
        {:handler posts-handler
         :error-handler posts-error-handler
         :format :json
         :response-format :json
         :keywords? true
         :params {:thread-title thread-title :page-num page-num :page-size page-size :record-short-id record-short-id :download @download-thread?}}))

(defn fetch-new-posts!
  []
  ;(.log js/console "fetch-new-posts!:" thread-title page-num record-short-id)
  ;(session/put! :posts [:span.glyphicon.glyphicon-refresh.spinning.loading-component])
  (let [threads (remove nil?
                        (map #(if (re-find #"^thread-last-accessed-" (.key js/localStorage %))
                               {:thread-title (clojure.string/replace (.key js/localStorage %) #"^thread-last-accessed-" "")
                                :time-last-accessed (js/parseInt (.getItem js/localStorage (.key js/localStorage %)))})
                             (range (.-length js/localStorage))))]
    (session/put! :posts nil)
    (POST (str "/api/new-posts" )
          {:handler new-posts-handler
           :error-handler posts-error-handler
           :format :json
           :response-format :json
           :keywords? true
           :params {:threads threads}})))

(defn check-new-post-notification!
  []
  ;(.log js/console "check-new-post-notification!!:" )
  (let [threads (remove nil?
                        (map #(if (re-find #"^thread-last-accessed-" (.key js/localStorage %))
                               {:thread-title (clojure.string/replace (.key js/localStorage %) #"^thread-last-accessed-" "")
                                :time-last-accessed (js/parseInt (.getItem js/localStorage (.key js/localStorage %)))})
                             (range (.-length js/localStorage))))]
    (POST (str "/api/new-post-notification" )
          {:handler #(reset! new-post-notification (:result %))
           :format :json
           :response-format :json
           :keywords? true
           :params {:threads threads}})))

(defn fetch-server-status!
  ([]
   (fetch-server-status! false))
  ([sync]
    ;(.log js/console "fetch-server-status!" )
   (ajax "/api/status"
         {:method   "GET"
          :success  (fn [response]
                      (let [status (:status (clojure.walk/keywordize-keys (js->clj response)))]
                        (reset! server-node-name (:server-node-name status))
                        (reset! server-url-base (:server-url-base status))
                        (reset! service-name (:service-name status))
                        (reset! enable-recaptcha (:enable-recaptcha status))
                        (reset! recaptcha-site-key (:recaptcha-site-key status))
                        (reset! recaptcha-secret-key (:recaptcha-secret-key status))

                        (set-title)
                        (if (= (session/get :page) :status)
                          (reset! server-status status))))
          :async    (not sync)
          :dataType "json"})))

(defn process-query-string
  []
  (try
    (let [href (-> js/window .-location .-href)
          new-href (clojure.string/replace href #"(\?[^\?]+)\?[^\?]+$" "$1")
          new-href (clojure.string/replace new-href #"\?_=1$" "")
          new-href (clojure.string/replace new-href #"\?_=1&" "?")]
      (.replaceState (.-history js/window) "" (.-title js/document) new-href))
    (let [href (-> js/window .-location .-href)
          query (apply merge (map
                               #(let [match (re-find #"^(.*?)=(.*)" %)]
                                 {(keyword (nth match 1)) (nth match 2)})
                               (clojure.string/split (second (re-find #"^.*?\?(.*)$" href)) #"&")))]

      (reset! download-thread?
              (and
                (:download-thread query)
                (not  (= (:download-thread query) "0"))))
      (reset! navbar-enabled?
              (or
                (nil? (:navbar query))
                (not  (= (:navbar query) "0"))))
      (reset! navbar-bottom-enabled?
              (or
                (nil? (:navbar-bottom query))
                (not  (= (:navbar-bottom query) "0"))))
      (reset! post-form-enabled?
              (and
                (:post-form query)
                (not  (= (:post-form query) "0"))))
      (reset! posts-displayed?
              (or
                (nil? (:posts query))
                (not  (= (:posts query) "0"))))
      query)
    (catch js/Error e (.log js/console e) {})))

(defn set-title
  [& [params]]
  (set! (.-title js/document)
        (if @service-name
          (str (case (session/get :page)
                 :home "目次"
                 :threads "全てのスレッド"
                 :recent-threads "最近更新されたスレッド"
                 :thread (session/get :thread-title)
                 :new-posts "新着レスまとめ読み"
                 :create-new-thread "新規スレッド作成"
                 :status "状態"
                 :help "使い方"
                 :terms "新月ネットワーク利用規約"
                 "")
               " - " @service-name))))

(secretary/defroute "/" [] (process-query-string) (reset! jump-command :top) (session/put! :page :home) (set-title))
(secretary/defroute "/new-posts" [] (process-query-string) (fetch-new-posts!) (session/put! :page :new-posts) (set-title))
(secretary/defroute "/create-new-thread" [] (process-query-string) (session/put! :page :create-new-thread) (set-title))
(secretary/defroute "/help" [] (process-query-string) (session/put! :page :help) (set-title))
(secretary/defroute "/terms" [] (process-query-string) (session/put! :page :terms) (set-title))
(secretary/defroute "/status" [] (process-query-string) (session/put! :page :status) (fetch-server-status! true) (set-title))

(secretary/defroute
  "/threads" []
  (process-query-string)
  (reset! jump-command nil)
  (fetch-threads! :threads)
  (set-title))

(secretary/defroute
  "/recent-threads" []
  (process-query-string)
  (reset! jump-command nil)
  (fetch-threads! :recent-threads)
  (set-title))

(secretary/defroute
  "/thread/:thread-title"
  [thread-title]
  (process-query-string)
  (reset! jump-command :first-new-post)
  (if @posts-displayed?
    (fetch-posts! thread-title 0 nil))
  (session/put! :thread-title thread-title)
  (session/put! :page-num 0)
  (session/put! :record-short-id nil)
  (session/put! :href-base (str "/thread/" (js/decodeURIComponent thread-title)))
  (session/put! :page :thread)
  (set-title))

(secretary/defroute
  "/thread/:thread-title/:qualifier"
  [thread-title qualifier]
  (process-query-string)
  (let [page-num (nth (re-find #"^p([0-9]+)$" qualifier) 1 nil)
        page-num (and page-num (js/parseInt page-num))
        record-short-id (nth (re-find #"^([0-9a-f]{8})$" qualifier) 1 nil)]
    (reset! jump-command :top)
    (if @posts-displayed?
      (fetch-posts! thread-title page-num record-short-id))
    (session/put! :thread-title thread-title)
    (session/put! :page-num page-num)
    (session/put! :record-short-id record-short-id)
    (session/put! :href-base (str "/thread/" (js/decodeURIComponent thread-title)))
    (session/put! :page :thread)
    (set-title)))

(defn mount-components []
  (session/put! :navbar-enabled? true)
  (session/put! :navbar-bottom-enabled? true)
  (reagent/render [#'navbar] (.getElementById js/document "navbar"))
  (reagent/render [#'page] (.getElementById js/document "app"))
  (reagent/render [#'navbar-bottom] (.getElementById js/document "navbar-bottom")))

(defn keep-popups-within-view
  []
  (let [tooltip-top (atom nil)
        tooltip-bottom (atom nil)]
    (.each ($ :.tooltip)
           (fn [index element]
             (let
               [$element ($ element)
                top (+ (.-top (.offset $element)) 5)
                bottom (+ top (.height $element))]
               (reset! tooltip-top
                       (cond
                         (nil? @tooltip-top) top
                         (< top @tooltip-top) top
                         :else @tooltip-top))
               (reset! tooltip-bottom bottom))))
    ;(.log js/console @tooltip-top (.scrollTop ($ js/document)))
    (cond
      (and @tooltip-top
           (or (< (- @tooltip-bottom @tooltip-top) (.height ($ js/window)))
               (has-class (.last ($ :.tooltip)) "top"))
           (< @tooltip-top (.scrollTop ($ js/document))))
      (.animate ($ (keyword "html, body")) (clj->js {:scrollTop @tooltip-top}) "fast")

      (and @tooltip-bottom
           (or (< (- @tooltip-bottom @tooltip-top) (.height ($ js/window)))
               (has-class (.last ($ :.tooltip)) "bottom"))
           (> @tooltip-bottom (+ (.scrollTop ($ js/document)) (.height ($ js/window)))))
      (.animate ($ (keyword "html, body")) (clj->js {:scrollTop (- @tooltip-bottom (.height ($ js/window)))}) "fast"))))

(defn update-page []
  (js/setTimeout
    ; For some strange reasons, ($) does not work properly without setTimeout.
    (fn []
      ;(.log js/console "update-page:" (.-length ($ :.anchor)))
      (.each ($ (keyword ".popup"))
             (fn [index element]
               (-> ($ element)
                   (.unbind "touchstart touchend mousedown mouseup click")
                   (.on "touchstart touchend moused mouseup" #(.stopPropagation %)))))
      (.each ($ (keyword ".popup a.btn:not(.attachment)"))
             (fn [index element]
               (-> ($ element)
                   (.unbind "touchstart touchend mousedown mouseup click")
                   (.on "touchstart touchend moused mouseup" #(.stopPropagation %))
                   (.on "click"
                        #(do
                          (.preventDefault %)
                          (.stopPropagation %)
                          (handle-click-on-link %))))))
      (.each ($ :.anchor)
             (fn [index element]
               (-> ($ element)
                   (.unbind "touchstart touchend mousedown mouseup click")
                   (.on "touchstart touchend moused mouseup" #(.stopPropagation %))
                   (.click
                     #(do (.tooltip ($ element) "toggle")
                          (.preventDefault %)
                          (.stopPropagation %)))
                   (.hoverIntent
                     #(do (reset! jump-command nil) (.tooltip ($ element) "show"))
                     #())
                   (.tooltip
                     (clj->js {:trigger   "manual"
                               :placement (if (has-class ($ element) "reverse-anchor") "auto bottom" "auto top")
                               :opacity   1
                               :html      true
                               :title     (fn []
                                            (let [result (atom nil)
                                                  _ (ajax "/api/thread"
                                                          {:method   "POST"
                                                           :success  (fn [response] (reset! result (clojure.walk/keywordize-keys (js->clj response))))
                                                           :error    (fn [] (reset! result "ERROR"))
                                                           :async    false
                                                           :dataType "json"
                                                           :data     {:thread-title    (attr ($ element) "data-thread-title")
                                                                      :page-num        nil
                                                                      :page-size       nil
                                                                      :record-short-id (attr ($ element) "data-record-short-id")}})
                                                  post (first (:posts @result))]
                                              (reset! jump-command nil)
                                              (update-page)
                                              (reagent.core/render-component-to-string
                                                (generate-html-for-post post :popup (attr ($ element) "data-thread-title") (:anchors @result)))))})))))
      (keep-popups-within-view)
      (highlight-code-block)
      (process-jump-command)
      (when (and (= (session/get :page) :thread)
                 @post-form-enabled?)
        (-> ($ :textarea#body) (.focus)))
      (when (and
              (= (session/get :page) :thread)
              @post-form-enabled?
              (session/get :record-short-id))
        (-> ($ :textarea#body) (.val (str ">>" (session/get :record-short-id) "\n")))
        (.setSelectionRange (aget ($ :textarea#body) 0) 22 22)) ;TODO: This routine does not work with Firefox.
      ) 0))

(defn init! []
  (enable-console-print!)
  (reset! history (doto (Html5History.)
                    (events/listen
                      EventType/NAVIGATE
                      (fn [event]
                        ;(.log js/console "token:" (.-token event))
                        ;(.log js/console "href:" (-> js/window .-location .-href))
                        (secretary/dispatch! (.-token event))))
                    (.setUseFragment false)
                    (.setPathPrefix "")
                    (.setEnabled true)))
  (mount-components)
  (session/put! :recent-threads nil)
  (session/put! :threads nil)
  ;(.initHighlightingOnLoad js/hljs)

  (-> ($ js/document)
      (.on "mouseup touchend"
             (fn [e]
               (.stopPropagation e)
               (remove-tooltips))))

  (check-new-post-notification!)
  (fetch-server-status! true)
  (js/setInterval check-new-post-notification! 30000)
  (js/setInterval fetch-server-status! 30000)
  (js/setInterval #(when (= (session/get :page) :recent-threads) (reset! jump-command nil) (update-threads :recent-threads)) 60000)
  (js/setInterval #(when (= (session/get :page) :threads) (reset! jump-command nil) (update-threads :threads)) 180000))
