;; ------------------------------使用的变量------------------------------

(defcustom boring-buffer-regexp-list
  (list (rx string-start " ")
        ;; (rx string-start "*")
        (rx string-start "*helm")
        "\\`.*ompil.*\\'"
        "\\`\\*[Hh]elp.*\\'"
        (rx string-start "*Selection")
        (rx string-start "*Backtrace")
        (rx string-start "*Warnings")
        (rx string-start "*Ibuffer")
        (rx string-start "*Flutter Run")
        (rx string-start "*dart_analysis_server")
        (rx string-start "*LSP")
        (rx string-start "*lsp")
        (rx string-start "*Bookmark")
        (rx string-start "*Alerts")
        (rx string-start "*gopls")
        (rx "TAGS" line-end)
        (rx string-start "*scratch*" string-end)
        (rx string-start "*quickrun*" string-end)
        (rx string-start "*vterm")
        )
  "无用的 buffer 匹配列表, 被 ibuffer, ido, helm 使用."
  :type  '(repeat (choice regexp))
  :group 'base_init
  )

(defcustom boring-file-regexp-list
  (list
   (rx "/." line-end)
   (rx "/.." line-end)
   (rx ".elc" line-end)
   (rx "#" line-end)
   (rx "~" line-end)
   ;; (rx "locate.db" line-end)
   (rx line-start "/tmp/?")
   (rx line-start "/ssh:")
   (rx "TAGS" line-end)
   )
  "无用的 file 匹配列表, 被 ibuffer, ido, helm 使用."
  :type  '(repeat (choice regexp))
  :group 'base_init
  )

(defadvice save-buffer (before make-default-directory activate)
  "Create missing directory when find file."
  (unless (file-exists-p default-directory) (make-directory default-directory t)))

(require 'recentf)
(setq recentf-max-saved-items 100)            ;增大 recentf 历史记录为 50
;; (setq recentf-max-menu-items 10)
;; (setq recentf-exclude '("/tmp/" "/ssh:"))
(setq recentf-exclude '(boring-file-regexp-list))
(add-hook 'delete-frame-functions (lambda (frame) (recentf-save-list)))
(recentf-mode t) ;显示最近打开的文件列表
;; FIXME: 是否要删除 recentf?
;; (defadvice recentf-cleanup (around with-silent activate)
;;   (flet ((message (x &rest) ()))
;;     ad-do-it))
;; (add-hook 'after-save-hook 'recentf-cleanup)

(require 'savehist)                     ; 保存 minibuffer 历史
(setq history-length 500)
(setq savehist-additional-variables '(mark-ring
                                      global-mark-ring
                                      search-ring
                                      regexp-search-ring
                                      extended-command-history))
(savehist-mode t)
(add-hook 'delete-frame-functions (lambda (frame) (savehist-autosave)))
(require 'saveplace)                    ; 打开文件时, 恢复光标位置
(save-place-mode t)
(add-hook 'delete-frame-functions (lambda (frame) (save-place-kill-emacs-hook)))

;; (require 'thingatpt)
;; (require 'imenu)

;; (require 'desktop)
;; (setq desktop-save t
;;       desktop-load-locked-desktop t)

;; (setq desktop-path (list user-emacs-directory)
;;       desktop-auto-save-timeout 120)

;; (defun sanityinc/desktop-time-restore (orig &rest args)
;;                (let ((start-time (current-time)))
;;                  (prog1
;;                      (apply orig args)
;;                    (message "Desktop restored in %.2fms"
;;                             (sanityinc/time-subtract-millis (current-time)
;;                                                             start-time)))))

;; (defun sanityinc/desktop-time-buffer-create (orig ver filename &rest args)
;;                (let ((start-time (current-time)))
;;                  (prog1
;;                      (apply orig ver filename args)
;;                    (message "Desktop: %.2fms to restore %s"
;;                             (sanityinc/time-subtract-millis (current-time)
;;                                                             start-time)
;;                             (when filename
;;                               (abbreviate-file-name filename))))))

;; (defun sanityinc/time-subtract-millis (b a)
;;                (* 1000.0 (float-time (time-subtract b a))))

;; ;; (advice-add 'desktop-read :around 'sanityinc/desktop-time-restore)
;; ;; (advice-add 'desktop-create-buffer :around 'sanityinc/desktop-time-buffer-create)

;; (setq desktop-globals-to-save
;;       '((comint-input-ring        . 50)
;;         (compile-history          . 30)
;;         desktop-missing-file-warning
;;         (dired-regexp-history     . 20)
;;         (extended-command-history . 30)
;;         (face-name-history        . 20)
;;         (file-name-history        . 100)
;;         (grep-find-history        . 30)
;;         (grep-history             . 30)
;;         (magit-revision-history   . 50)
;;         (minibuffer-history       . 50)
;;         (org-clock-history        . 50)
;;         (org-refile-history       . 50)
;;         (org-tags-history         . 50)
;;         (query-replace-history    . 60)
;;         (read-expression-history  . 60)
;;         (regexp-history           . 60)
;;         (regexp-search-ring       . 20)
;;         register-alist
;;         (search-ring              . 20)
;;         (shell-command-history    . 50)
;;         tags-file-name
;;         tags-table-list))

;; ;; (desktop-save-mode t)

;; (use-package session
;;   ;; :hook (after-init . session-initialize)
;;   :config
;;   (setq session-save-file (locate-user-emacs-file ".session"))
;;   (setq session-name-disable-regexp "\\(?:\\`'/tmp\\|\\.git/[A-Z_]+\\'\\)")
;;   (setq session-save-file-coding-system 'utf-8))


;; ------------------------------ELPA相关设置------------------------------
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("elpy" . "http://jorgenschaefer.github.io/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.emacs-china.org/gnu/"))

;; 注意，flycheck 依赖这个包，对 emacs-lisp 做语法检查。
;; (package-initialize)

;; (unless (require 'el-get nil 'noerror)
;;   (with-current-buffer
;;       (url-retrieve-synchronously
;;        "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
;;     (goto-char (point-max))
;;     (eval-print-last-sexp)))

;; ------------------------------一些函数 ------------------------------
;; 某些模式会使用 F12 快捷键执行附加的操作。(全局绑定为无操作)
(global-set-key [f12] 'noop)

;; ------------------------------显示相关------------------------------

;; TODO: 奇怪，取消某个设置, 会导致 next-line 到最后, 有奇怪的提示音.

(setq
 inhibit-startup-echo-area-message t
 scroll-preserve-screen-position t              ;移动屏幕时，保持光标相对屏幕的位置不变。
 revert-without-query '(".*")                   ;设定那些 buffer 执行 revert 时, 不提示确认.
 read-quoted-char-radix 10                      ;字符代码以十进制显示
 echo-keystrokes 0.1                            ;快速在minibuffer显示按键提示，间隔0.1秒
 display-time-day-and-date t                    ;显示时间包含当前日期
 blink-matching-paren nil                       ;关闭括号匹配自动跳转
 ;; visible-bell t                              ;开启屏幕闪屏提示
 ring-bell-function 'ignore                     ; 即使没有 visible-bell, 也不会有语音提示。
 display-time-24hr-format t                     ;使用24小时形式显示时间.
 display-buffer-reuse-frames t                  ;切换到已有的frame
 dabbrev-case-fold-search nil                   ;自动完成大小写敏感.
 ;; initial-major-mode 'org-mode                ;初始的 scratch 使用 org-mode.
 truncate-lines t                               ;关闭自动折行.
 load-prefer-newer t                            ;总首选较新的那个。(即使 el 比elc 新)
 package-enable-at-startup nil                  ; 性能好一点？
 native-comp-async-report-warnings-errors nil   ; 抑制 native-comp 的警告
 warning-minimum-level :error                   ; 只显示错误级别的警告
 )

(require 'whitespace)
(setq whitespace-line-column nil)
(setq whitespace-style
      '(face
       spaces
       empty            ; empty lines at beginning/end of buffer
       tab-mark
       trailing       ; trailing blanks
       ))


;; (setq whitespace-global-modes '(not makefile-mode))
(global-whitespace-mode 1)

(setq lpr-command "new_lpr")

;; 开启几个默认关闭的操作
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p) ;用 y/n 方式代替 yes/no

;; ------------------------------备份选项------------------------------
(setq version-control t                 ;开启版本控制, 注意: 只有在退出emacs buffer之后, 才算一次备份(check in)
      auto-save-default t               ; 开启自动保存功能.
      auto-save-interval 50             ; 输入 50 个字符后，会自动保存。
      auto-save-timeout 10              ; 10 秒钟自动保存一次。
      vc-make-backup-files t            ; 即使开启vc(e.g. git), 也备份
      kept-new-versions 1000            ;备份最近修改的版本 1000 次
      kept-old-versions 2               ;备份最原始版本两次。
      delete-old-versions t             ;删除不满足以上条件的版本
      backup-by-copying t
      backup-directory-alist '(("." . "~/.backups")) ;设置备份文件保存的目录
      vc-suppress-confirm t             ;vc操作不需提示确认
      ;; vc-command-messages t             ;显示vc backend命令的提示消息, 这个不要开，否则 diff-hl 内容非常多。
      vc-follow-symlinks t              ; vc 操作符号链接指向的那个文件。
      )

;; ==============================杂项==============================

(setq delete-by-moving-to-trash t               ;删除文件使用回收站.
      ;; menu-updating-frame nil                   ;死窗口, 运行这个.
      ;; auto-save-list-file-prefix nil            ;关闭 auto session restore. FIXME: 这个注释掉了, 不知道啥意思.
      set-mark-command-repeat-pop t             ;开启C-u .重复弹出pop功能.
      mode-require-final-newline t              ;任意主模式, 文件尾部添加一个新行.(fundamental除外)
      kill-ring-max 100                         ;设置kill-ring可以保存的最大记录
      ;; 在覆盖系统剪贴板之前, 保存剪贴板内容到 Emacs 的 kill-ring.
      save-interprogram-paste-before-kill t
      sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
      inhibit-default-init t                    ;不进行全局(default)初始化
      tab-width 4                               ;设置默认TAB间隔为4个空格.
      tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120)
      colon-double-space t                      ;重排文本时, 在冒号后面新加入两个空格.
      ;; truncate-partial-width-windows t          ;不能在一行显示时，自动截断, this will cause lsp the first candidate is blank.
      ;; create-lockfiles nil                      ; 禁用锁定文件, .# 开头的文件, 例如：.#ruby.rb
      mouse-yank-at-point t                     ;而是使用鼠标中键在光标处 yank.
      ;; browse-url-browser-function 'browse-url-generic
      ;; browse-url-generic-program "firefox"
      ;; browse-url-generic-args nil
      vc-handled-backends '(Git)
      )

(setq-default
 ;; major-mode 'text-mode                  ;设置默认主模式为text-mode
 case-fold-search nil      ;搜索和匹配大小写敏感.
 indicate-empty-lines t    ;在左侧边缘显示行尾空行标志.
 imenu-auto-rescan t
 fill-column 82             ;设置默认 82 个字符为换行标记.
 left-fringe-width 10
 cursor-type 'bar
 cursor-in-non-selected-windows 'hollow ; 在没有激活的 window 上显示一个空心正方形
 )

(setq mouse-buffer-menu-mode-groups
      '(
        (".*\\.rb" . "Ruby")
        (".*\\.el". "Lisp")
        ))
(setq mouse-buffer-menu-mode-mult 1)

;; 设置同其他软件交换信息的编码系统,这个不建议手动设置，可能复制粘帖数据会引起乱码.
;; (set-selection-coding-system (if (eq system-type 'windows-nt) 'utf-16-le 'utf-8))

(set-language-environment 'utf-8) ;使用UTF-8字符集
(prefer-coding-system 'utf-8) ;总是优先使用 UTF-8 检测文件内容
(set-default-coding-systems 'utf-8)

;; (setq coding-system-for-read 'utf-8)
;; 强制保存为utf8编码系统, 如果不指定这个, 可以通过
;; set-buffer-file-coding-system 指定使用哪种编码系统保存当前文件
;; (setq coding-system-for-write 'utf-8)

;; ============================== 开启的模式 ==============================
(global-prettify-symbols-mode t)
(global-so-long-mode t)
(global-hl-line-mode t)                 ; 光标行高亮

(require 'uniquify)
(setq uniquify-separator "/"               ;; The separator in buffer names.
      uniquify-buffer-name-style 'forward) ;; names/in/this/style

;; 这个到底是否需要？ 不确定，先注释看看效果。
;; (turn-on-eldoc-mode)

(add-hook 'text-mode-hook 'goto-address-mode)

(delete-selection-mode t)                       ;选区替换模式.

(global-auto-revert-mode t)
;; (setq auto-revert-interval 0.5)
(setq auto-revert-verbose nil)

;; 这个同时看两个 buffer 很有用.
;; (follow-mode t)

(windmove-default-keybindings) ;; Shift+方向键选择窗口q
;; 将所有开启的窗口布局, 放入一个 stack, 可以通过 C-c <left> 或 C-c <right> 浏览 stack
(winner-mode t)

;; 开启试一下
(setq
 frame-resize-pixelwise t
 ;; mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil))
 ;; mouse-wheel-progressive-speed nil
 )

;; Disable margin for overline and underline
(setq
 overline-margin 0
 underline-minimum-offset 0
 )

;; (setq window-sides-vertical t)

(blink-cursor-mode -1)                  ;光标不闪
(transient-mark-mode t)
(column-number-mode t)                  ;显示列号码
(tooltip-mode -1)                       ;在minibuffer显示菜单帮助提示
;; (display-time-mode t)                   ;显示时间
(show-paren-mode t)                     ;一直显示括号
(auto-image-file-mode t)                     ;对于图片扩展名的文件显示图片

;; 这个鬼设置有啥用？ 忘记了。
;; (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)

;; (electric-pair-mode t)
;; ;; electric-pair-text-pairs 中的字符，仅仅在 comment/string 中生效。
;; (add-to-list 'electric-pair-text-pairs '(?` . ?'))
;; (add-to-list 'electric-pair-text-pairs '(?（ . ?）))
;; (remove-hook 'sh-mode-hook 'sh-electric-here-document-mode)

;; diff-mode
(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG$" . diff-mode))
;; conf-unix-mode
(add-to-list 'auto-mode-alist '("sudoers\\.tmp\\|\\.cnf\\|hosts\\|keymap\\|exports\\|\\.*rc$" . conf-unix-mode))
;; emacs-lisp-mode
(add-to-list 'auto-mode-alist '("abbrev_defs" . emacs-lisp-mode))
;; sh-mode
(add-to-list 'auto-mode-alist '("\\.env\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.env\\.local\\'" . sh-mode))

;;; ------------------------- test features ------------------------------

(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))

(repeat-mode 1)

;; (file-name-shadow-mode 1)
;; (minibuffer-depth-indicate-mode 1)                   ;显示minibuffer深度

(visual-line-mode 1)
(setq visual-line-fringe-indicators '(nil right-curly-arrow))
(setq what-cursor-show-names t)
(minibuffer-electric-default-mode t)
(minibuffer-depth-indicate-mode t)
;; 我为什么要打开这个？
;; (setq enable-recursive-minibuffers t)
(setq toggle-truncate-lines nil)
(setq comment-auto-fill-only-comments t)

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point-max)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(require 'abbrev)
(setq abbrev-file-name (expand-file-name "abbrev_defs" default-directory))

;; 有关几个是否使用 Xwindow 组件的设置

;; 默认值 t, 表示如果用鼠标操作时，使用当前操作系统的 GUI 对话框来创建文件。
(setq use-file-dialog nil)
;; (setq use-dialog-box nil)
;; (setq x-gtk-use-system-tooltips nil)

(provide 'base_init)

;;; base_init.el ends here
