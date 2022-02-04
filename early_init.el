;; Emacs 默认只是在 shell, vterm, shell, term 下面设定这个环境变量为相应的值。
;; 这里全局增加这个环境变量，设定为 true, 可以用来检测当前是否在 Emacs 中运行。
(setenv "INSIDE_EMACS" "true")
;; (setenv "RUBYLIB" "") ;; 如果设置 RUBYLIB 为空，可能造成一些 ruby 工具无法使用
(setenv "RUBYOPT" "")
;; (setenv "http_proxy" "")
;; (setenv "https_proxy" "")
;; (setenv "ftp_proxy" "")

;; (setq inhibit-compacting-font-caches t)  ; Don’t compact font caches during GC.

;; gc-cons-threshold controlled by gcmh.el
;; 注意，不要随便设置很大的 gc-cons-threshold, 否则会突然很卡。
(require 'gcmh)
(gcmh-mode)

;; 根据 lsp-dart 的建议，这个应该设定大一点，性能会好很多。
(setq read-process-output-max (* 1024 1024)) ;; 1M

(setq package-enable-at-startup nil)

(require 'auto-compile_init)

(require 'server)
;; Emacs server 设定
(setq server-use-tcp t) ;使用 `tcp-ip连接' 代替本地 `socket链接'.
;; (setq server-host "127.0.0.1") ;指定本emacs server是否允许远程连接.
(setq server-port 9299)

(setq native-comp-async-report-warnings-errors nil)
(setq warning-minimum-level :error)

(setq custom-theme-directory (expand-file-name "plugins/themes" default-directory))

(setq inhibit-startup-screen t)    ;关闭启动帮助画面
(setq initial-scratch-message "
(setq default-directory \"/home/zw963/Dropbox/common/.emacs.d\")
(normal-top-level-add-subdirs-to-load-path)
")

;; 添加几个需要的前缀。
(define-prefix-command 'meta-c-map)
(global-set-key [(meta c)] 'meta-c-map)

;;  在命令行下, 貌似 C-2 默认等价于: C-@，这里使用同样的绑定。
(define-key key-translation-map [(control \2)] [(control \@)])
(global-set-key [(control c) (j)] 'imenu)

(global-set-key [(meta D)] (lambda () (interactive) (dired "./"))) ; 打开 dired buffer.
(global-set-key [(control s)] 'isearch-forward-regexp) ;更改Ctrl-s为正则搜索

(define-key indent-rigidly-map [(control b)] 'indent-rigidly-left)
(define-key indent-rigidly-map [(control f)] 'indent-rigidly-right)
(define-key indent-rigidly-map [(meta b)]  'indent-rigidly-left-to-tab-stop)
(define-key indent-rigidly-map [(meta f)] 'indent-rigidly-right-to-tab-stop)

(add-hook 'isearch-mode-hook
          (lambda ()
            (define-key isearch-mode-map [(control b)] 'isearch-delete-char)
            (define-key isearch-mode-map [(control f)] 'isearch-yank-char)
            (define-key isearch-mode-map [(meta f)] 'isearch-yank-word)
            (define-key isearch-mode-map [(control e)] 'isearch-yank-line)
            (define-key isearch-mode-map [(control y)] 'isearch-yank-kill)
            (define-key isearch-mode-map [(meta \')] 'expand-abbrev)
            (define-key isearch-mode-map [(super n)] 'isearch-repeat-forward)
            (define-key isearch-mode-map [(super p)] 'isearch-repeat-backward)
            (define-key isearch-mode-map [(meta \5)] 'isearch-query-replace-regexp)
            ))

(relative-load "functions.el")
;; (relative-load "keybindings.el")

(require 'mark-lines)

(defun mark-next-line ()
  "Mark next line continuously."
  (interactive)
  (if (use-region-p)
      (next-line nil)
    (if (eql (point-max) (line-end-position))
        (mark-lines-next-line nil)
      (mark-lines-previous-line nil)
      )))

(defun open-line-and-indent (n)
  (interactive "*p")
  (call-interactively 'open-line)
  (indent-according-to-mode)
  )

(defun hack-local-variables-confirm (all-vars unsafe-vars risky-vars dir-name)  t)

(global-set-key [(meta k)] 'mark-next-line)
(global-set-key [(control o)] 'open-line-and-indent)

(relative-load "autoloads.el")
(relative-load "base_init.el")
(relative-load "init.el")

;; 打开这两个变量, 查看那些包有错误.
(unless (string= (getenv "LOAD_INIT") "true")
  (setq debug-on-error t)              ;需要调试时，开启这个。
  (setq no-byte-compile t)
  ;; (setq debug-on-signal t)
  )
