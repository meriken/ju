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



(def page-size 20)
(def jump-command (atom nil))
(def history (atom nil))
(def navbar-collapsed? (atom true))



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
        :on-click #(do (handle-click-on-link %) (reset! navbar-collapsed? true))}
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

(declare fetch-threads!)
(declare fetch-posts!)

(defn navbar-bottom
  []
  (fn []
    [:nav.navbar.navbar-default.navbar-fixed-bottom
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
                           (fetch-posts! (session/get :thread-title) (session/get :page-num) (session/get :record-short-id))
                           nil
                     ))}
        [:span.glyphicon.glyphicon-refresh]]
       [:a.btn.btn-default.navbar-btn
        {:on-click (fn [e] (.animate ($ (keyword "html,body")) (clj->js {:scrollTop 0}) "slow"))}
        [:span.glyphicon.glyphicon-chevron-up]]
       [:a.btn.btn-default.navbar-btn
        {:on-click (fn [e] (.animate ($ (keyword "html,body")) (clj->js {:scrollTop (.height ($ js/document))}) "slow"))}
        [:span.glyphicon.glyphicon-chevron-down]]
       [:a.btn.btn-default.navbar-btn
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
             ["きれいな新月", "裏"]])]]]]])

(declare fetch-threads!)

(defn recent-threads-page []
  [:div.container
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
  [:div.container
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
                  [:li {:key (uuid)} [:a
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
                [:li {:key (uuid)} [:a.jump-to-bottom
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

(defn thread-page []
    [:div.container
     [:a
      {:on-click handle-click-on-link
       :href (session/get :href-base)}
      [:h3 (session/get :thread-title)]]
     [:div#content
      (if (session/get :page-num)
        [top-page-jump-buttons])
      (session/get :posts)
      (if (session/get :page-num)
        [bottom-page-jump-buttons])]])

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
   :recent-threads #'recent-threads-page
   :thread #'thread-page
   :new-posts #'new-posts-page
   :create-new-thread #'create-new-thread-page
   :help #'help-page})

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
           :key (uuid)}
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
  [s]
  (let [match (re-find #"^(.*?)>>([0-9a-f]{8})(.*)$" s)]
    (if-not match
      s
      (concat [(nth match 1)
               [:a.btn.btn-default.btn-sm.anchor
                {;:href (str (session/get :href-base) "/" (nth match 2))
                 ;:on-click handle-click-on-link
                 :key (uuid)
                 :data-toggle "tooltip"
                 :data-container "body"
                 :data-record-short-id (nth match 2)}
                ">>" (nth match 2)]]
              (process-anchors (nth match 3))))
    ))

(defn process-bracket-links
  [s]
  (let [match (re-find #"^(.*?)\[\[([^/\]]+)(/[0-9a-f]{8})?\]\](.*)$" s)]
    (if-not match
      s
      (concat [(nth match 1)
               [:a.btn.btn-default.btn-sm.bracket-link
                {:href (str "/thread/" (js/decodeURIComponent (nth match 2)))
                 :on-click handle-click-on-link
                 :key (uuid)
                 ;:data-toggle "tooltip"
                 ;:data-container "body"
                 ;:data-record-short-id (nth match 3)
                 }
                "[[" (nth match 2) "]]"]]
              (process-anchors (nth match 3))))
    ))

(defn generate-html-for-post
  [post context]
  (let
    [name (and (:name post)
               (-> (:name post)
                   (clojure.string/replace #"&amp;" "&")
                   (clojure.string/replace #"&gt;" ">")
                   (clojure.string/replace #"&lt;" "<")))
     mail (and (:mail post)
               (-> (:mail post)
                   (clojure.string/replace #"&amp;" "&")
                   (clojure.string/replace #"&gt;" ">")
                   (clojure.string/replace #"&lt;" "<")))
     body (drop-last
            (apply concat
                   (map
                     #(list
                       (-> %
                           (clojure.string/replace #"&amp;" "&")
                           (clojure.string/replace #"&gt;" ">")
                           (clojure.string/replace #"&lt;" "<"))
                       [:br {:key (uuid)}])
                     (clojure.string/split (:body post) #"<br>"))))
     body (map #(if (string? %) (process-anchors %) %) body)
     body (map #(if (string? %) (process-bracket-links %) %) body)
     md5 (Md5.)
     _ (.update md5 (:pubkey post) (count (:pubkey post)))
     src (str "/thread/"
              (js/decodeURIComponent (session/get :thread-title))
              "/"
              (:record-short-id post)
              "." (:suffix post))
     heading [[:a.btn.btn-xs.btn-default.id
               {:href (str (session/get :href-base) "/" (:record-short-id post))
                :on-click handle-click-on-link}
               [:span.glyphicon.glyphicon-tag] " " (take 8 (:record-short-id post))] " "
              (if (and (:name post) (pos? (count (:name post)))) [:span.name [:span.glyphicon.glyphicon-user] name]) " "
              (if (and (:mail post) (pos? (count (:mail post)))) [:span.mail [:span.glyphicon.glyphicon-envelope] mail]) " "
              (if (:pubkey post) [:span.signature [:span.glyphicon.glyphicon-pencil] (take 11 (goog.crypt.base64/encodeByteArray (.digest md5)))]) " "
              [:span.timestamp
               [:span.glyphicon.glyphicon-time]
               (cljs-time.format/unparse
                 (cljs-time.format/formatter "yyyy-MM-dd HH:mm")
                 (cljs-time.core/to-default-time-zone (cljs-time.coerce/from-long (* (:stamp post) 1000))))] " "
              (if (and (:suffix post) (re-find #"^(jpe?g|png|gif|bmp)$" (:suffix post)))
                [:a.btn.btn-xs.btn-default.attachment
                 {:href src}
                 [:span.glyphicon.glyphicon-paperclip] (str " " (:record-short-id post) "." (:suffix post))])]
     body-with-image [body
                      (if (pos? (count body)) [:br])
                      (if (and (:suffix post) (re-find #"^(jpe?g|png|gif|bmp)$" (:suffix post)))
                        [:img {:height 210
                               :src src
                               :on-click #(let [links (clj->js [src])
                                                options (clj->js {:useBootstrapModal false})]
                                           (.toggleClass ($ :#blueimp-gallery) "blueimp-gallery-controls" true)
                                           (.Gallery js/blueimp links options))
                               }])]]
    (case context
      :popup
      [:div.popup {:key (uuid)}
       (into [] (concat [:div.well.well-sm.popup-heading] heading))
       (into [] (concat [:div] body-with-image))]

      [:div.panel.panel-default.post {:key (uuid)}
       (into [] (concat [:div.panel-heading] heading))
       (into [] (concat [:div.panel-body] body-with-image))])))

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
       (doall
         (map
           #(generate-html-for-post % :thread)
           (:posts response)))])))

(defn fetch-posts!
  [thread-title page-num record-short-id]
  (.log js/console "fetch-posts!:" thread-title page-num record-short-id)
  (session/put! :posts [:span.glyphicon.glyphicon-refresh.spinning.loading-component])
  (POST (str "/api/thread" )
       {:handler posts-handler
        :format :json
        :response-format :json
        :keywords? true
        :params {:thread-title thread-title :page-num page-num :page-size page-size :record-short-id record-short-id}}))

(secretary/defroute "/" [] (reset! jump-command :top) (session/put! :page :home))
(secretary/defroute "/threads" [] (reset! jump-command nil) (session/put! :page :threads))
(secretary/defroute "/recent-threads" [] (reset! jump-command nil) (session/put! :page :recent-threads))
(secretary/defroute "/new-posts" [] (session/put! :page :new-posts))
(secretary/defroute "/create-new-thread" [] (session/put! :page :create-new-thread))
(secretary/defroute "/help" [] (session/put! :page :help))

(secretary/defroute
  "/thread/:thread-title"
  [thread-title]
  (fetch-posts! thread-title 0 nil)
  (session/put! :thread-title thread-title)
  (session/put! :page-num 0)
  (session/put! :href-base (str "/thread/" (js/decodeURIComponent thread-title)))
  (session/put! :page :thread))

(secretary/defroute
  "/thread/:thread-title/:qualifier"
  [thread-title qualifier]
  (let [page-num (nth (re-find #"^p([0-9]+)$" qualifier) 1 nil)
        page-num (and page-num (js/parseInt page-num))
        record-short-id (nth (re-find #"^([0-9a-f]{8})$" qualifier) 1 nil)]
    (fetch-posts! thread-title page-num record-short-id)
    (session/put! :thread-title thread-title)
    (session/put! :page-num page-num)
    (session/put! :record-short-id record-short-id)
    (session/put! :href-base (str "/thread/" (js/decodeURIComponent thread-title)))
    (session/put! :page :thread)))

(defn mount-components []
  (reagent/render [#'navbar] (.getElementById js/document "navbar"))
  (reagent/render [#'page] (.getElementById js/document "app"))
  (reagent/render [#'navbar-bottom] (.getElementById js/document "navbar-bottom")))

(defn update-page []
  (js/setTimeout
    ; For some strange reasons, ($) does not work properly without setTimeout.
    (fn []
      (.log js/console "update-page:" (.-length ($ :.anchor)))
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
                               :placement "auto top"
                               :html true
                               :title (fn []
                                        (let [result (atom nil)
                                              _ (ajax "/api/thread"
                                                      {:method "POST"
                                                       :success (fn [response] (reset! result response))
                                                       :error (fn [] (reset! result "ERROR"))
                                                       :async false
                                                       :dataType "json"
                                                       :data {:thread-title (session/get :thread-title)
                                                              :page-num nil
                                                              :page-size nil
                                                              :record-short-id (attr ($ element) "data-record-short-id")}})
                                              post (first (:posts (clojure.walk/keywordize-keys (js->clj @result))))]
                                          (reset! jump-command nil)
                                          (js/setTimeout update-page 0)
                                          (reagent.core/render-component-to-string (generate-html-for-post post :popup))))})))))
      (process-jump-command))
    0))

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
  (mount-components)
  (fetch-threads! :recent-threads)
  (js/setTimeout #(fetch-threads! :threads) 3000)

  (-> ($ js/document)
      (.on "mouseup touchend"
             (fn [e]
               (.stopPropagation e)
               (remove-tooltips)))))