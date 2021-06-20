(require 'smtpmail-async)

;; 下面是 gmail 设置。(不确定是否和 163 设定正好相反？)
;; 注意：默认使用 .netrc 获取信息。

(setq
 ;; smtpmail-stream-type 'ssl                  ; SMTP 连接类型
 smtpmail-smtp-server "smtp.gmail.com"       ;不确定是否可以使用环境变量.
 smtpmail-default-smtp-server "smtp.gmail.com"
 smtpmail-smtp-service 587                   ; 587 是 gmail 的 smtp 端口.
 user-full-name "Billy.Zheng"           ;发件人姓名, $NAME 环境变量指定
 user-mail-address "vil963@gmail.com"      ;发件人默认邮件地址, 可以通过 $EMAIL 来指定.
 smtpmail-smtp-user "vil963@gmail.com"
 starttls-use-gnutls t
 smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
 send-mail-function 'async-smtpmail-send-it      ;使用通过 SMTP 指定的服务器发送邮件.
 message-send-mail-function 'async-smtpmail-send-it
 message-confirm-send t                 ;当发送一个 message 时, 会有确认提示.
 smtpmail-debug-info t
 message-kill-buffer-on-exit t        ;; 发送完成后, buffer 将会自动关闭
 auth-source-save-behavior nil     ;不提示保存密码到 ~/.authinfo
 ;; auth-sources '("~/.netrc.gpg" "~/.authinfo.gpg" "~/.netrc" "~/.authinfo")
 ;; mail-archive-file-name "~/.mailbak"       ;指定发送邮件时，密抄一份到某个文件，作为备份。
 message-kill-buffer-on-exit  t              ;当发送消息成功后, 将 kill 消息 buffer.
 )

(add-hook 'message-mode-hook
          '(lambda ()
             (define-key message-mode-map [(control c) (control K)] 'message-kill-buffer)
             (define-key message-mode-map [(control c) (control c)] 'message-fill-paragraph)
             (define-key message-mode-map [(control x) (control s)] 'message-send-and-exit)
             (turn-on-flyspell)
             (set (make-local-variable 'ac-auto-show-menu) t)
             (set (make-local-variable 'ac-use-menu-map) t)
             ))

;; ============================== mu4e 设置 ==============================
(require 'mu4e)
(require 'mu4e-contrib)                 ; Need by mu4e-html2text-command

(require 'mu4e-alert)
(add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
;; (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)

;; 所有未读邮件, 不含邮件列表, 及其他特殊分类。
(setq mu4e-bookmarks '(("flag:unread" "Unread mail" ?u)))
;; 添加今天、最近七天未读邮件, 这两个分类，包含所有类型邮件。
(add-to-list 'mu4e-bookmarks '("date:today..now" "Today's messages" ?t))
(add-to-list 'mu4e-bookmarks '("date:7d..now" "Last 7 days" ?w))

;; 以上所有分类，均不含标记为 Spam 的邮件, 需要单独进这个分类查找。
;; (add-github-watch '("maildir:/trash" "Spam email" ?s))

;; 所有邮件，主要用作自定义查找。
(add-to-list 'mu4e-bookmarks '("" "All mail" ?a))

;; (setq mu4e-maildir-shortcuts '(("/maillist". ?m)
;;                                ("/github" . ?g)
;;                                ("/sent" . ?s)
;;                                ("/unknown" . ?u)
;;                                ("/trash" . ?t)
;;                                ("/all" . ?a)
;;                                ("/chinese" . ?c)
;;                                ))

(setq mu4e-maildir-shortcuts
      '( ("/INBOX"               . ?i)
         ("/[Gmail].Sent Mail"   . ?s)
         ("/[Gmail].Trash"       . ?t)
         ("/[Gmail].All Mail"    . ?a)
         ))

(setq
 ;; mu4e-compose-complete-ignore-address-regexp "no-?reply" ;;默认值
 ;; mu4e-headers-show-threads t            ;; 显示邮件之间的关系, 默认值
 ;; mu4e-headers-include-related t          ;; 显示所有相关的邮件, 默认值,  W 开关。
 ;; mu4e-headersq-skip-duplicates t        ;; 跳过重复的邮件, 默认值

 ;; mu4e-headers-results-limit  500  ;; 默认值, 太大影响性能

 ;; mu4e-headers-show-target  t  ;; 默认值

 mail-user-agent 'mu4e-user-agent    ;; 当按下 C-x m 时，使用 mu4e 代替 compose-mail
 mu4e-compose-dont-reply-to-self t  ;; 当使用 replaying to all 时，跳过自己
 mu4e-update-interval 300             ;; 5 minutes
 mu4e-compose-signature (concat
                         "Billy"
                         ;; "Geek, Nerd, Rubyist\n"
                         ;; "程序员中的牛逼运维， 运维中的牛逼程序员。"
                         )
 mu4e-attachment-dir "~/Download"   ;; attachment save to ~/Download

 mu4e-maildir "~/Maildir/gmail"
 ;; 下面的目录相对于上面的 mu4e-maildir
 mu4e-drafts-folder "/[Gmail].Drafts"
 mu4e-sent-folder   "/[Gmail].Sent Mail"
 mu4e-trash-folder  "/[Gmail].Trash"

 ;; 默认值, 不使用 mu4e 手动获取 mail, 我们使用 crontab.
 ;; mu4e-get-mail-command "true"

 ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
 mu4e-sent-messages-behavior 'delete
 mu4e-headers-auto-update nil           ;; 我自己手动刷新 headers
 ;; mu4e-confirm-quit            nil      ;; 退出是无需确认。
 )

(provide 'mu4e_init)
;;; mu4e_init.el ends here
