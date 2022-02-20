;; Emacs 默认只是在 shell, vterm, shell, term 下面设定这个环境变量为相应的值。
;; 这里全局增加这个环境变量，设定为 true, 可以用来检测当前是否在 Emacs 中运行。
(setenv "INSIDE_EMACS" "true")
;; (setenv "RUBYLIB" "") ;; 如果设置 RUBYLIB 为空，可能造成一些 ruby 工具无法使用
(setenv "RUBYOPT" "")
;; (setenv "http_proxy" "")
;; (setenv "https_proxy" "")
;; (setenv "ftp_proxy" "")

(require 'gcmh_init)
(require 'auto-compile_init)
(require 'crux_init)
;; (require 'exec-path-from-shell_init)
(require 'themes_init)

(require 'server)
;; Emacs server 设定
(setq server-use-tcp t) ;使用 `tcp-ip连接' 代替本地 `socket链接'.
;; (setq server-host "127.0.0.1") ;指定本emacs server是否允许远程连接.
(setq server-port 9299)

(setq inhibit-startup-screen t ;关闭启动帮助画面
      initial-scratch-message "
(setq default-directory \"/home/zw963/Dropbox/common/.emacs.d\")
(normal-top-level-add-subdirs-to-load-path)
"
      )

;; 当加载 .dir-locals 时，永远不提示
(defun hack-local-variables-confirm (all-vars unsafe-vars risky-vars dir-name) t)

(relative-load "autoloads.el")
(relative-load "functions.el")
(relative-load "base_init.el")
(relative-load "init.el")

;; 打开这两个变量, 查看那些包有错误.
(unless (string= (getenv "LOAD_INIT") "true")
  (setq debug-on-error t)              ;需要调试时，开启这个。
  (setq no-byte-compile t)
  ;; (setq debug-on-signal t)
  )
