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
  (:use     [jayq.core :only [$ parent attr on off html add-class remove-class has-class ajax]])
  (:use-macros [jayq.macros :only [ready]])
  (:import [goog.history Html5History]
            [goog Uri]
            [goog.history EventType]
            [goog.crypt Md5]))



(declare update-page)
(declare fetch-threads!)
(declare fetch-posts!)



(def page-size 20)
(def recaptcha-sitekey "6LfTLRQTAAAAAEQstJ4pYl1qzIvJfBJ2-WRRMmYR")
(def jump-command (atom :top))
(def history (atom nil))
(def navbar-collapsed? (atom true))
(def navbar-enabled? (atom true))
(def navbar-bottom-enabled? (atom true))
(def post-form-enabled? (atom true))
(def posts-displayed? (atom false))


(defn remove-tooltips
  []
  (.tooltip ($ (keyword "[data-toggle=\"tooltip\"]")) "hide")
  (.remove ($ :.tooltip)))

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
    (when (and href path (not (= path "/status")))
      (. e preventDefault)
      (. e stopPropagation)
      ;TODO
      (. @history (setToken (str path "?_=1" (if (pos? (count query-string)) "&" "") query-string) title))
      (reset! navbar-collapsed? true)
      (remove-class (parent ($ (keyword "button[data-toggle=dropdown]"))) "open")
      (remove-tooltips)
      (reset! jump-command
              (cond
                (has-class $target "jump-to-bottom") :bottom
                (has-class (parent $target) "jump-to-bottom") :bottom
                :else :top))
      ;(process-jump-command)
      )))

(defn process-jump-command
  []
  ;(.log js/console (str "process-jump-command: " @jump-command))
  (cond
    (= @jump-command :top)
    (js/setTimeout #(.scrollTop ($ (keyword "html,body")) 0) 0)
    (= @jump-command :bottom)
    (js/setTimeout #(.scrollTop ($ (keyword "html,body")) (.height ($ js/document))) 0))
  ;(reset! jump-command nil)
  )

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
  [:li {:class (when (= page (session/get :page)) "active")}
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
          {:class         (when-not @navbar-collapsed? "collapsed")
           :data-toggle   "collapse"
           :aria-expanded @navbar-collapsed?
           :aria-controls "navbar"
           :on-click      #(swap! navbar-collapsed? not)}
          [:span.glyphicon.glyphicon-menu-hamburger]
          ;[:span.glyphicon.glyphicon-triangle-bottom]
          ]
         [:a.navbar-brand {:on-click handle-click-on-link :href "/"} "需"]]
        [:div.navbar-collapse.collapse
         (when-not @navbar-collapsed? {:class "in"})
         [:ul.nav.navbar-nav
          [nav-link "/" "目次" :home]
          [nav-link "/recent-threads" "スレッド一覧" :recent-threads]
          [nav-link "/new-posts" "新着レスまとめ読み" :new-posts]
          [nav-link "/create-new-thread" "新規スレッド作成" :create-new-thread]
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
                     (reset! jump-command nil)
                     (.back (.-history js/window)))}
        [:span.glyphicon.glyphicon-arrow-left]]
       [:a.btn.btn-default.navbar-btn
        {:on-click (fn [e]
                     (reset! jump-command nil)
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
                             (reset! jump-command :top)
                             (fetch-posts! (session/get :thread-title) (session/get :page-num) (session/get :record-short-id)))
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
        {:class (if (= (session/get :page) :thread) "" "disabled")
         :on-click handle-click-on-link
         :href (str (session/get :href-base) "?posts=0&post-form=1")}
        [:span.glyphicon.glyphicon-pencil] "書込"]]]]))

(defn home-page []
  [(keyword (str "div.container"
                 (if (not @navbar-enabled?) ".without-navbar")
                 (if (not @navbar-bottom-enabled?) ".without-navbar-bottom")))
   [:h3 "需"]
   [:p
    "「需」は新月ネットワークに参加しているP2P型の匿名掲示板です。"
    [:a {:href "/terms" :on-click handle-click-on-link} "新月ネットワーク利用規約"] "を守った上で、自由に利用してください。"]

   [:div.row
    [:div#main-menu-column.col-sm-6
     [:div#main-menu.list-group
      [:a {:on-click handle-click-on-link :href "/recent-threads" :class "list-group-item"} "最近更新されたスレッド" [:span.glyphicon.glyphicon-chevron-right.pull-right]]
      [:a {:on-click handle-click-on-link :href "/threads" :class "list-group-item"} "すべてのスレッド" [:span.glyphicon.glyphicon-chevron-right.pull-right]]
      [:a {:on-click handle-click-on-link :href "/new-posts" :class "list-group-item"} "新着レスまとめ読み" [:span.glyphicon.glyphicon-chevron-right.pull-right]]
      [:a {:on-click handle-click-on-link :href "/create-new-thread" :class "list-group-item"} "新規スレッド作成" [:span.glyphicon.glyphicon-chevron-right.pull-right]]
      [:a {:href "/status" :class "list-group-item"} "状態" [:span.glyphicon.glyphicon-chevron-right.pull-right]]
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
    [:div.btn-group.btn-group-justified.refresh-threads-button
    [:a.btn.btn-default
     {:on-click #(do (reset! jump-command :top) (fetch-threads! :recent-threads))}
     [:span.glyphicon.glyphicon-refresh] "スレッド一覧を更新する"]]
    (session/get :recent-threads)
    [:div.btn-group.btn-group-justified.refresh-threads-button
    [:a.btn.btn-default
     {:on-click #(do (reset! jump-command :bottom) (fetch-threads! :recent-threads))}
     [:span.glyphicon.glyphicon-refresh] "スレッド一覧を更新する"]]]])

(defn threads-page []
  [(keyword (str "div.container"
                 (if (not @navbar-enabled?) ".without-navbar")
                 (if (not @navbar-bottom-enabled?) ".without-navbar-bottom")))
   [:h3 "全てのスレッド"]
   [:div#content
    [:div.btn-group.btn-group-justified.refresh-threads-button
    [:a.btn.btn-default
     {:on-click #(do (reset! jump-command :top) (fetch-threads! :threads))}
     [:span.glyphicon.glyphicon-refresh] "スレッド一覧を更新する"]]
    (session/get :threads)
    [:div.btn-group.btn-group-justified.refresh-threads-button
    [:a.btn.btn-default
     {:on-click #(do (reset! jump-command :top) (fetch-threads! :threads))}
     [:span.glyphicon.glyphicon-refresh] "スレッド一覧を更新する"]]]])

(defn top-page-jump-buttons
  []
  (fn []
      [:span
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

(defn recaptcha []
  (fn []
    [:div#g-recaptcha.g-recaptcha {:sitekey recaptcha-sitekey})))}])
;    [(with-meta #(do [:div#g-recaptcha])
;                {:component-did-mount (fn [this] (.render js/grecaptcha "g-recaptcha" (clj->js {:sitekey recaptcha-sitekey})))})]))

(defn submit-post
  [e]
  (.preventDefault e)
  (let [result (atom nil)
        _ (ajax "/api/post"
                {:method "POST"
                 :success (fn [response] (reset! result (clojure.walk/keywordize-keys (js->clj response))))
                 :error (fn [] (reset! result "ERROR"))
                 :async false
                 :contentType false
                 :processData false
                 :data (js/FormData. (.getElementById js/document "post-form"))})]
    ))

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
       [:button.btn.btn-block.btn-primary.wrapped-input.no-border {:on-click submit-post :style {:border-radius "0 0 4px 4px"}} "書き込み"]]]
     [recaptcha]
     [:a.btn.btn-default {:href "/terms" :on-click handle-click-on-link :style {:margin-bottom "10px"}} "新月ネットワーク利用規約"]]))

(defn thread-page []
    [(keyword (str "div.container"
                   (if (not @navbar-enabled?) ".without-navbar")
                   (if (not @navbar-bottom-enabled?) ".without-navbar-bottom")))
     [:a
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
   [:h3 "新着まとめ読み"]
   [:div#content
    [:span.glyphicon.glyphicon-refresh.spinning.loading-page]]])

(defn create-new-thread-page []
  [(keyword (str "div.container"
                 (if (not @navbar-enabled?) ".without-navbar")
                 (if (not @navbar-bottom-enabled?) ".without-navbar-bottom")))
   [:h3 "新規スレッド作成"]
   [:div#content
    [:span.glyphicon.glyphicon-refresh.spinning.loading-page]]])

(defn help-page []
  [(keyword (str "div.container"
                 (if (not @navbar-enabled?) ".without-navbar")
                 (if (not @navbar-bottom-enabled?) ".without-navbar-bottom")))
   [:h3 "使い方"]
   [:div#content
    [:span.glyphicon.glyphicon-refresh.spinning.loading-page]]])

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

(def pages
  {:home #'home-page
   :threads #'threads-page
   :recent-threads #'recent-threads-page
   :thread #'thread-page
   :new-posts #'new-posts-page
   :create-new-thread #'create-new-thread-page
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
       #(do
         [:a.list-group-item
          {:href (file-name-to-path (:file-name %))
           :on-click handle-click-on-link
           :key (my-uuid)}
          (unhexify (clojure.string/replace (:file-name %) #"^thread_" ""))
          [:span {:style {:border "solid 1px #ddd" :background-color "#eee"  :border-radius "4px" :margin-left "4px" :font-size 12  :padding "0 6px" :font-weight :normal}} (:num-records %)]
          [:span.glyphicon.glyphicon-chevron-right.pull-right]])
       response)]))

(defn fetch-threads! [dest]
  (session/put! dest [:span.glyphicon.glyphicon-refresh.spinning.loading-component])
  (GET "/api/threads"
       {:handler #(thread-list-handler % dest)
        :response-format :json
        :keywords? true
        :params {:n (if (= dest :recent-threads) 100 nil)}}))

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

(defn process-links
  [s]
  (let [match (re-find #"^(.*?)(https?://(www\.)?[-a-zA-Z0-9@:%._\+~#=]{2,256}\.[a-z]{2,6}\b([-a-zA-Z0-9@:%_\+.~#?&//=]*))(.*)$" s)]
    (if-not match
      s
      (concat [(nth match 1)
               [:a {:href (nth match 2) :target "_blank" :key (my-uuid)} (nth match 2)]]
               (process-links (last match))))))

(defn generate-html-for-post
  [post context thread-title anchors]
  (let
    [name (and (:name post)
               (-> (:name post)
                   (clojure.string/replace #"&gt;" ">")
                   (clojure.string/replace #"&lt;" "<")
                   (clojure.string/replace #"&amp;" "&")))
     mail (and (:mail post)
               (-> (:mail post)
                   (clojure.string/replace #"&gt;" ">")
                   (clojure.string/replace #"&lt;" "<")
                   (clojure.string/replace #"&amp;" "&")))
     body (drop-last
            (apply concat
                   (map
                     #(list
                       (-> %
                           (clojure.string/replace #"&gt;" ">")
                           (clojure.string/replace #"&lt;" "<")
                           (clojure.string/replace #"&amp;" "&"))
                       [:br {:key (my-uuid)}])
                     (clojure.string/split (:body post) #"<br>"))))
     body (map #(if (string? %) (process-links %) %) body)
     body (map #(if (string? %) (process-anchors % thread-title) %) body)
     body (map #(if (string? %) (process-bracket-links %) %) body)
     md5 (Md5.)
     _ (.update md5 (:pubkey post) (count (:pubkey post)))
     href-base (str "/thread/" (js/decodeURIComponent thread-title))
     src (str href-base "/"
              (:record-short-id post)
              "." (:suffix post))
     heading [[:a.btn.btn-xs.btn-default.id
               {:href (str href-base "/" (:record-short-id post))
                :on-click handle-click-on-link}
               [:span.glyphicon.glyphicon-tag] " " (take 8 (:record-short-id post))] " "
              (if (and (:name post) (pos? (count (:name post)))) [:span.name [:span.glyphicon.glyphicon-user] name]) " "
              (if (and (:mail post) (pos? (count (:mail post)))) [:span.mail [:span.glyphicon.glyphicon-envelope] mail]) " "
              (if (:pubkey post) [:span.signature [:span.glyphicon.glyphicon-pencil] (take 11 (goog.crypt.base64/encodeByteArray (.digest md5)))]) " "
              [:span.timestamp
               [:span.glyphicon.glyphicon-time]
               (cljs-time.format/unparse
                 (cljs-time.format/formatter "yyyy-MM-dd HH:mm (Z)")
                 (cljs-time.core/to-default-time-zone (cljs-time.coerce/from-long (* (:stamp post) 1000))))] " "
              (if (and (:suffix post) (re-find #"^(jpe?g|png|gif|bmp)$" (:suffix post)))
                [:a.btn.btn-xs.btn-default.attachment
                 {:href src}
                 [:span.glyphicon.glyphicon-paperclip] (str " " (:record-short-id post) "." (:suffix post))])]
     body-exists? (pos? (count body))
     thumbnail-exists? (and (:suffix post) (re-find #"^(jpe?g|png|gif|bmp)$" (:suffix post)))
     reverse-anchors (remove nil? (map #(if (= (:destination %) (:record-short-id post)) (:source %) nil) anchors))
     body-with-image [body
                      (if (and body-exists? thumbnail-exists?) [:hr])
                      (if thumbnail-exists?
                        [:img {:height 210
                               :src src
                               :on-click #(let [links (clj->js [src])
                                                options (clj->js {:useBootstrapModal false})]
                                           (.toggleClass ($ :#blueimp-gallery) "blueimp-gallery-controls" true)
                                           (.Gallery js/blueimp links options))}])
                      (if (pos? (count reverse-anchors))
                        [:div.reverse-anchors [:hr]
                         [:div
                          (map #(do [:a.btn.btn-default.btn-sm.anchor.reverse-anchor
                                     {:data-record-short-id %
                                      :data-thread-title thread-title
                                      :key (my-uuid)}
                                     "└" %])
                               reverse-anchors)]])]

     ]
    (case context
      :popup
      [:div.popup {:key (my-uuid)}
       (into [] (concat [:div.well.well-sm.popup-heading] heading))
       (into [] (concat [:div] body-with-image))]

      [:div.panel.panel-default.post {:key (my-uuid)}
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
      [:div#posts
       (doall
         (map
           #(generate-html-for-post % :thread (session/get :thread-title) (:anchors response))
           (:posts response)))])))

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
        "レスの読み込みに失敗しました。"]])))

(defn fetch-posts!
  [thread-title page-num record-short-id]
  ;(.log js/console "fetch-posts!:" thread-title page-num record-short-id)
  (session/put! :posts [:span.glyphicon.glyphicon-refresh.spinning.loading-component])
  (POST (str "/api/thread" )
       {:handler posts-handler
        :error-handler posts-error-handler
        :format :json
        :response-format :json
        :keywords? true
        :params {:thread-title thread-title :page-num page-num :page-size page-size :record-short-id record-short-id}}))

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

(secretary/defroute "/" [] (process-query-string) (reset! jump-command :top) (session/put! :page :home))
(secretary/defroute "/new-posts" [] (process-query-string) (session/put! :page :new-posts))
(secretary/defroute "/create-new-thread" [] (process-query-string) (session/put! :page :create-new-thread))
(secretary/defroute "/help" [] (process-query-string) (session/put! :page :help))
(secretary/defroute "/terms" [] (process-query-string) (session/put! :page :terms))

(secretary/defroute
  "/threads" []
  (process-query-string)
  (reset! jump-command nil)
  (if (nil? (session/get :threads))
    (fetch-threads! :threads))
  (session/put! :page :threads))

(secretary/defroute
  "/recent-threads" []
  (process-query-string)
  (reset! jump-command nil)
  (if (nil? (session/get :recent-threads))
    (fetch-threads! :recent-threads))
  (session/put! :page :recent-threads))

(secretary/defroute
  "/thread/:thread-title"
  [thread-title]
  (.log js/console (clj->js (process-query-string)))
  (if @posts-displayed?
    (fetch-posts! thread-title 0 nil))
  (session/put! :thread-title thread-title)
  (session/put! :page-num 0)
  (session/put! :record-short-id nil)
  (session/put! :href-base (str "/thread/" (js/decodeURIComponent thread-title)))
  (session/put! :page :thread))

(secretary/defroute
  "/thread/:thread-title/:qualifier"
  [thread-title qualifier]
  (process-query-string)
  (let [page-num (nth (re-find #"^p([0-9]+)$" qualifier) 1 nil)
        page-num (and page-num (js/parseInt page-num))
        record-short-id (nth (re-find #"^([0-9a-f]{8})$" qualifier) 1 nil)]
    (if @posts-displayed?
      (fetch-posts! thread-title page-num record-short-id))
    (session/put! :thread-title thread-title)
    (session/put! :page-num page-num)
    (session/put! :record-short-id record-short-id)
    (session/put! :href-base (str "/thread/" (js/decodeURIComponent thread-title)))
    (session/put! :page :thread)))

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
             (fn[index element]
               (-> ($ element)
                   (.unbind "touchstart touchend mousedown mouseup click")
                   (.on "touchstart touchend moused mouseup" #(.stopPropagation %)))))
      (.each ($ (keyword ".popup a.btn:not(.attachment)"))
             (fn[index element]
               (-> ($ element)
                   (.unbind "touchstart touchend mousedown mouseup click")
                   (.on "touchstart touchend moused mouseup" #(.stopPropagation %))
                   (.on "click"
                     #(do
                          (.preventDefault %)
                          (.stopPropagation %)
                          (handle-click-on-link %))))))
      (.each ($ :.anchor)
             (fn[index element]
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
                     (clj->js {:trigger	"manual"
                               :placement (if (has-class ($ element) "reverse-anchor") "auto bottom" "auto top")
                               :opacity 1
                               :html true
                               :title (fn []
                                        (let [result (atom nil)
                                              _ (ajax "/api/thread"
                                                      {:method "POST"
                                                       :success (fn [response] (reset! result (clojure.walk/keywordize-keys (js->clj response))))
                                                       :error (fn [] (reset! result "ERROR"))
                                                       :async false
                                                       :dataType "json"
                                                       :data {:thread-title (attr ($ element) "data-thread-title")
                                                              :page-num nil
                                                              :page-size nil
                                                              :record-short-id (attr ($ element) "data-record-short-id")}})
                                              post (first (:posts @result))]
                                          (reset! jump-command nil)
                                          (update-page)
                                          (reagent.core/render-component-to-string
                                            (generate-html-for-post post :popup (attr ($ element) "data-thread-title") (:anchors @result)))))})))))
      (keep-popups-within-view)
      (process-jump-command))
    0))

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

  (-> ($ js/document)
      (.on "mouseup touchend"
             (fn [e]
               (.stopPropagation e)
               (remove-tooltips)))))
