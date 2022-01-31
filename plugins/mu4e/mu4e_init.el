;; 下面是 gmail 设置。(不确定是否和 163 设定正好相反？)
;; 注意：默认使用 .netrc 获取信息。

(require 'smtpmail)
(require 'message)

(setq
 send-mail-function 'smtpmail-send-it
 smtpmail-stream-type 'starttls
 ;; smtpmail-default-smtp-server "smtp.gmail.com"
 ;; smtpmail-smtp-server "smtp.gmail.com"       ; 使用 SMTPSERVER 环境变量
 smtpmail-smtp-service 587                   ; 587 是 gmail 的 smtp 端口.
 ;; user-full-name "Billy.Zheng"           ;发件人姓名, $NAME 环境变量指定
 ;; user-mail-address "my_email@gmail.com"      ;发件人默认邮件地址, 可以通过 $EMAIL 来指定.
 ;; smtpmail-smtp-user "myuser"                 ; 会自动在 .netrc 中查找
 smtpmail-queue-mail nil   ; 不使用 queue 模式, mu 要求这样设定来支持离线模式。
 smtpmail-queue-dir   "~/Maildir/queue/cur"
 starttls-use-gnutls t
 message-confirm-send t                 ;当发送一个 message 时, 会有确认提示.
 smtpmail-debug-info t
 ;; auth-source-save-behavior nil     ;不提示保存密码到 ~/.authinfo
 ;; auth-sources '("~/.netrc.gpg" "~/.authinfo.gpg" "~/.netrc" "~/.authinfo")
 ;; mail-archive-file-name "~/.mailbak"       ;指定发送邮件时，密抄一份到某个文件，作为备份。
 message-kill-buffer-on-exit  t              ;当发送消息成功后, 将 kill 消息 buffer.
 )

;; (with-eval-after-load 'smtpmail-async
;;   (custom-set-variables
;;    '(send-mail-function 'async-smtpmail-send-it)
;;    '(message-send-mail-function 'async-smtpmail-send-it)     ; show completion list when ambiguous
;;    ))

(add-hook 'message-mode-hook
          (lambda ()
            (define-key message-mode-map [(control c) (control K)] 'message-kill-buffer)
            (define-key message-mode-map [(control c) (control c)] 'message-fill-paragraph)
            (define-key message-mode-map [(control x) (control s)] 'message-send-and-exit)
            (turn-on-flyspell)
            (set (make-local-variable 'ac-auto-show-menu) t)
            (set (make-local-variable 'ac-use-menu-map) t)
            ))

;; 初始化 mu 的步骤：
;; 1. pacman -S xapian-core，安装必须的依赖
;; 2. 可选的，编译 mu
;; 3. mu init 初始化
;; 4. mu index 创建索引
;; 5.
;; ============================== mu4e 设置 ==============================
(require 'mu4e)
(require 'mu4e-contrib)                 ; Need by mu4e-html2text-command

;; 以上所有分类，均不含标记为 Spam 的邮件, 需要单独进这个分类查找。
;; (add-github-watch '("maildir:/trash" "Spam email" ?s))

;; 所有邮件，主要用作自定义查找。
(add-to-list 'mu4e-bookmarks '("" "All mail" ?a))

;; the maildirs you use frequently; access them with 'j' ('jump')
(setq mu4e-maildir-shortcuts
      '( ("/gmail/INBOX"               . ?i)
         ("/gmail/[Gmail].Sent Mail"   . ?s)
         ("/gmail/[Gmail].Drafts"   . ?s)
         ("/gmail/[Gmail].Trash"       . ?t)
         ;; ("/gmail/[Gmail].All Mail"    . ?a)
         ))

(setq mu4e-headers-fields '(
                            (:human-date . 12)
                            (:flags . 6)
                            (:mailing-list . 10)
                            (:from-or-to . 22)
                            (:subject))
      mu4e-headers-from-or-to-prefix '("" . "➦ ")
      )

(setq
 ;; mu4e-compose-complete-ignore-address-regexp "no-?reply" ;;默认值
 ;; mu4e-headers-show-threads t            ;; 显示邮件之间的关系, 默认值
 ;; mu4e-headers-include-related t          ;; 显示所有相关的邮件, 默认值,  W 开关。
 ;; mu4e-headersq-skip-duplicates t        ;; 跳过重复的邮件, 默认值

 ;; mu4e-headers-results-limit  500  ;; 默认值, 太大影响性能

 ;; mu4e-headers-show-target  t  ;; 默认值

 mail-user-agent 'mu4e-user-agent    ;; 当按下 C-x m 时，使用 mu4e 代替 compose-mail
 mu4e-compose-dont-reply-to-self t  ;; 当使用 replaying to all 时，跳过自己
 ;; mu4e-use-fancy-chars t
 mu4e-headers-leave-behavior 'apply
 ;; mu4e-index-update-in-background t ;; 默认值，用后台刷新
 mu4e-compose-signature (concat
                         "Thanks,\n"
                         "Billy"
                         ;; "Geek, Nerd, Rubyist\n"
                         ;; "程序员中的牛逼运维， 运维中的牛逼程序员。"
                         )
 gnus-unbuttonized-mime-types nil
 ;; message-cite-reply-position 'below ;; 当回复消息时，我们写的消息，在引用的文字下面。
 mu4e-sent-folder   "/gmail/[Gmail].Sent Mail"
 mu4e-drafts-folder "/gmail/[Gmail].Drafts"
 mu4e-trash-folder  "/gmail/[Gmail].Trash"
 mu4e-attachment-dir "~/Desktop"   ;; attachment save to ~/Desktop
 mu4e-view-show-images t

 ;; 默认值为 true, 表示不使用 mu4e 手动获取 mail.
 ;; mu4e-get-mail-command "offlineimap"
 mu4e-update-interval 60             ;; 因为不需要 retrieve mail, 因此每分钟刷新一次，默认 5 分钟.

 mu4e-change-filenames-when-moving t

 ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
 mu4e-sent-messages-behavior 'delete
 mu4e-headers-auto-update nil           ;; 我自己手动刷新 headers
 ;; mu4e-confirm-quit            nil      ;; 退出是无需确认。
 mu4e-compose-reply-to-address "vil963@gmail.com"
 )

(add-hook 'mu4e-compose-mode-hook 'auto-fill-mode)

;; 检测 xwidget 是否工作的办法： (xwidget-webkit-browse-url "https://www.gnu.org/")
;; mu4e-view 将造成 space 快捷键不工作。
;; (require 'mu4e-views)
;; ;; 调用 mu4e-views-mu4e-select-view-msg-method 选择采用的 view 方法。
;; (define-key mu4e-headers-mode-map (kbd "v") #'mu4e-views-mu4e-select-view-msg-method)
;; (define-key mu4e-headers-mode-map (kbd "M-n") #'mu4e-views-cursor-msg-view-window-down)
;; (define-key mu4e-headers-mode-map (kbd "M-p") #'mu4e-views-cursor-msg-view-window-up)
;; ;; (setq mu4e-views-next-previous-message-behaviour 'always-switch-to-view)

(require 'mu4e-alert)

(mu4e-alert-set-default-style 'libnotify)
(add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
(add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)

;; (require 'NetworkManager)

;; (NetworkManager-add-listener
;;  (lambda (state)
;;    (setq smtpmail-queue-mail (not state))
;;    (when (eq major-mode 'mu4e-main-mode)
;;      (let ((pos (point)))
;;        (mu4e~main-view-real nil nil)
;;        (goto-char pos)))))

(provide 'mu4e_init)
;;; mu4e_init.el ends here
