;; (setq read-process-output-max (* 1024 1024)) ;; 1M
;; (setq inhibit-compacting-font-caches t)  ; Don’t compact font caches during GC.

;; gc-cons-threshold controlled by gcmh.el
;; 注意，不要随便设置很大的 gc-cons-threshold, 否则会突然很卡。
(require 'gcmh)
(gcmh-mode)

(require 'auto-compile_init)
(require 'cl)

(require 'server)
;; Emacs server 设定
(setq server-use-tcp t) ;使用 `tcp-ip连接' 代替本地 `socket链接'.
;; (setq server-host "127.0.0.1") ;指定本emacs server是否允许远程连接.
(setq server-port 9299)

(setq custom-theme-directory (expand-file-name "plugins/themes" default-directory))

(setq inhibit-startup-screen t)    ;关闭启动帮助画面
(setq initial-scratch-message "
(setq default-directory \"/home/zw963/Dropbox/common/.emacs.d\")
(normal-top-level-add-subdirs-to-load-path)
")

(relative-load "functions.el")
(relative-load "keybindings.el")

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

(global-set-key [(meta k)] 'mark-next-line)
(global-set-key [(control o)] 'open-line-and-indent)

(relative-load "autoloads.el")
(relative-load "base_init.el")
(relative-load "init.el")

;; 打开这两个变量, 查看那些包有错误.
(unless (string= (getenv "LOAD_INIT") "true")
  (setq debug-on-error t)              ;需要调试时，开启这个。
  (setq no-byte-compile t)
  ;;(setq debug-on-signal t)
  )

;; ============================== 编程相关 ==============================


;; (require 'treemacs_init)

;; (require 'rvm)
;; (require 'lsp-mode)
;; (require 'lsp-modeline)
;; (require 'lsp-headerline)
;; (require 'dap-mode)
;; (require 'dap-ui)
;; (require 'dap-mouse)
;; (require 'ruby_init)
;; (require 'company)
