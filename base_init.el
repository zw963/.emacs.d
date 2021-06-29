;; ------------------------------使用的变量------------------------------

;;; 这个文件是全局生效的改动, 而且不常修改.

;;; Code:
(setenv "RUBYOPT" "")
;; (setenv "RUBYLIB" "")
(setenv "http_proxy" "")
(setenv "https_proxy" "")
(setenv "ftp_proxy" "")

(defcustom boring-buffer-regexp-list
  (list (rx string-start " ")
        (rx string-start "*helm")
        (rx string-start "*Ido")
        (rx string-start "*Compile")
        (rx string-start "*compilation")
        (rx string-start "*Selection")
        (rx string-start "*Backtrace")
        (rx string-start "*Warnings")
        (rx string-start "*Help")
        (rx "TAGS" line-end)
        (rx string-start "*scratch*" string-end))
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
   (rx "locate.db" line-end)
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

;; 设置透明度
;; (add-to-list 'default-frame-alist '(alpha 95 80))
(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(95 . 80) '(100 . 100)))))

(require 'recentf)
(setq recentf-max-saved-items 100)            ;增大 recentf 历史记录为 50
;; (setq recentf-max-menu-items 10)
;; (setq recentf-exclude '("/tmp/" "/ssh:"))
(setq recentf-exclude '(boring-file-regexp-list))
;; 如果某个 window 没有激活，在那个 window 上不显示 cursor。
(setq-default cursor-in-non-selected-windows nil)
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

;; ------------------------------ELPA相关设置------------------------------
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;; 注意，flycheck 依赖这个包，对 emacs-lisp 做语法检查。
;; (package-initialize)

;; (unless (require 'el-get nil 'noerror)
;;   (with-current-buffer
;;       (url-retrieve-synchronously
;;        "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
;;     (goto-char (point-max))
;;     (eval-print-last-sexp)))

;; ------------------------------一些函数 ------------------------------
(defun noop (&optional noop)
  "Do nothing, NOOP."
  (interactive)
  )
;; 某些模式会使用 F12 快捷键执行附加的操作。(全局绑定为无操作)
(global-set-key [f12] 'noop)

(defun add-list-to-list(target list)
  "Add LIST to TARGET."
  (set target (append list (eval target))))

;; 自动安装 package, e.g: (install 'imenu-anywhere)
(defun install (package)
  "Install a PACKAGE."
  (interactive)
  (unless (package-installed-p package)
    (package-refresh-contents)
    (package-install package)))

;; ------------------------------显示相关------------------------------

;; TODO: 奇怪，取消某个设置, 会导致 next-line 到最后, 有奇怪的提示音.

(setq use-file-dialog nil                       ;关闭 GUI dialog.
      use-dialog-box nil
      inhibit-startup-echo-area-message t
      scroll-preserve-screen-position t         ;移动屏幕时，保持光标相对屏幕的位置不变。
      revert-without-query '(".*")              ;设定那些 buffer 执行 revert 时, 不提示确认.
      read-quoted-char-radix 10                 ;字符代码以十进制显示
      echo-keystrokes 0.1                       ;快速在minibuffer显示按键提示，间隔0.1秒
      display-time-day-and-date t               ;显示时间包含当前日期
      blink-matching-paren nil                  ;关闭括号匹配自动跳转
      visible-bell t                            ;开启屏幕闪屏提示
      display-time-24hr-format t                ;使用24小时形式显示时间.
      display-buffer-reuse-frames t             ;切换到已有的frame
      dabbrev-case-fold-search nil              ;自动完成大小写敏感.
      ;; initial-major-mode 'org-mode           ;初始的 scratch 使用 org-mode.
      truncate-lines t                          ;关闭自动折行.
      load-prefer-newer t                       ;总首选较新的那个。(即使 el 比elc 新)
      )

(require 'whitespace)
(setq
 whitespace-line-column nil
 show-trailing-whitespace t                ;显示行尾多余空格, 这个没有生效?
 whitespace-style '
 (face
  empty            ; empty lines at beginning/end of buffer
  ;; lines-tail       ; lines go beyond `fill-column'
  space-before-tab ; spaces before tab
  tabs             ; tabs (show by face)
  trailing         ; trailing blanks
  tab-mark         ; tabs (show by symbol)
  ;; lines space-after-tab
  ;; empty indentation
  ;; newline newline-mark
  ))

;; Title 上不显示当前文件名，因为当使用多窗口时，实在是效果不太好。
(setq frame-title-format
      '((:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name))
                 "%b")) " - Emacs " emacs-version))
;; , 中文，,
(dolist (param '(
                 (menu-bar-lines . 0)
                 (tool-bar-lines . 0)
                 (vertical-scroll-bars . nil)
                 (left-fringe . 1)
                 (right-fringe . 1)
                 ;; 当以 daemon 启动时， 光标使用系统光标黑色， 这里改为浅白色。
                 ;; (cursor-color . "AntiqueWhite3")
                 ;; (fullscreen . nil)

                 ;; 暂时注释掉这个字体。
                 ;; (font . "yaheiInconsolata-15")

                 ;; (font . "Rec Mono Linear:style=Regular-15")
                 ;; (font . "SHS Monaco Adjusted Medium-12")
                 ;; (font . "Input Mono Narrow-12")
                 ))
  (add-to-list 'default-frame-alist param)
  (add-to-list 'initial-frame-alist param)
  )

(setq lpr-command "new_lpr")

;; 开启几个默认关闭的操作
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p) ;用 y/n 方式代替 yes/no

(setq my-english-font "Fira Code")
(setq my-english-font-height (* 15 10))

(setq my-chinese-font "PingFang SC")
(setq my-chinese-font-size 15)

;; 有关设置字体的一些参考代码
(defun my-better-font()
  (interactive)
  ;; english font
  ;; 设置英文字体
  (set-face-attribute 'default nil :height my-english-font-height :weight 'regular :family my-english-font)
  (if (display-graphic-p)
      (progn
        ;; 设置中文字体
        (dolist (charset '(kana han symbol cjk-misc bopomofo))
          (set-fontset-font (frame-parameter nil 'font)
                            charset
                            (font-spec :family my-chinese-font :size my-chinese-font-size)))
        ;; instruct Emacs to use emoji fonts,
        (set-fontset-font t 'symbol (font-spec :family "Noto Color Emoji") nil 'prepend)
        )
    ))

(my-better-font)

;; ------------------------------ 启动最大化 ------------------------------
;; 可以通过运行 fc-list 来查看字体的名称.
(defun initialize-frame-delay (&optional frame)
  "Maximize FRAME when create a new FRAME."
  (when (display-graphic-p)
    ;; 这个设定应该是设定所有的 unicode 字体.
    ;; (set-fontset-font "fontset-default" "unicode" ("SHS Monaco Adjusted Medium-12"))

    ;; (dolist (charset '(kana han symbol cjk-misc bopomofo))
    ;;   (set-fontset-font (frame-parameter nil 'font)
    ;;                     charset
    ;;                     (font-spec :family "SHS Monaco Adjusted Medium-12")))

    )

  ;; (set-fontset-font "fontset-default" 'unicode'("yaheiInconsolata-15"))

  ;; (set-fontset-font "fontset-default" '(#x20a0 . #x2a3b)
  ;;                   (font-spec :family "等距更纱黑体 SC"
  ;;                              :size 24) nil 'prepend)

  (unless (or
           (eq this-command 'make-frame-command)
           (eq this-command 'ace-window)
           (eq this-command 'make-frame)
           ;; (eq this-command 'diredc)
           )
    (run-with-idle-timer 0 nil 'toggle-frame-maximized)
    )
  )

(add-hook 'window-setup-hook 'toggle-frame-maximized t)
;; NOTICE: 传送给aftar-make-frame-function的函数必须有且只能有一个参数用来表示新建立的frame.

(if (and (fboundp 'daemonp) (daemonp))
    (add-hook 'after-make-frame-functions 'initialize-frame-delay t)
  )

(global-set-key [(f5)] (lambda ()
                         (interactive)
                         (toggle-frame-maximized)
                         (set-frame-position (selected-frame) 0 0)
                         (set-frame-size (selected-frame) 120 63)))

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
      vc-command-messages t             ;显示vc backend命令的提示消息.
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
      truncate-partial-width-windows t          ;不能在一行显示时，自动截断。
      ;; create-lockfiles nil                      ; 禁用锁定文件, #开头的文件。
      mouse-yank-at-point t                     ;而是使用鼠标中键在光标处 yank.
      browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox"
      ;; browse-url-generic-args nil
      )

(setq-default
 ;; major-mode 'text-mode                  ;设置默认主模式为text-mode
 case-fold-search nil      ;搜索和匹配大小写敏感.
 indicate-empty-lines t    ;在左侧边缘显示行尾空行标志.
 imenu-auto-rescan t
 fill-column 82             ;设置默认 82 个字符为换行标记.
 left-fringe-width 10
 cursor-type 'bar
 )

(setq diff-switches "-Naur")                     ;Default to unified diffs
(setq vc-rcs-diff-switches "-u")

(defun update-diff-colors ()
  "Update the colors for diff faces."
  (set-face-attribute 'diff-added nil
                      :foreground "white" :background "DarkGreen")
  (set-face-attribute 'diff-removed nil
                      :foreground "white" :background "DarkRed")
  (set-face-attribute 'diff-changed nil
                      :foreground "white" :background "purple"))
(add-hook 'diff-mode-hook
          '(lambda ()
             (define-key diff-mode-map [(meta backspace)] 'backward-kill-word)
             (define-key diff-mode-map [(meta ?\d)] 'backward-kill-word)
             (define-key diff-mode-map [(super n)] 'diff-hunk-next)
             (define-key diff-mode-map [(super p)] 'diff-hunk-prev)
             (update-diff-colors)
             ))


;; ediff 最重要的两个命令，a, b 分别表示左右两个 buffer 的 diff.

;; 如果希望在一个单独的 frame 中读取 ediff 帮助, 注释下面这行代码.
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-coding-system-for-read 'utf-8-auto-unix)
;; (setq ediff-coding-system-for-write 'utf-8-auto-unix)
(setq ediff-auto-refine-limit 30000)

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
(global-auto-revert-mode t)
(global-hl-line-mode t)                 ; 光标行高亮

(require 'uniquify)
(setq uniquify-separator "/"               ;; The separator in buffer names.
      uniquify-buffer-name-style 'forward) ;; names/in/this/style

;; (add-hook 'prog-mode-hook '(lambda ()
;;                              (add-hook 'before-save-hook 'delete-trailing-whitespace)
;;                              ))
;; (add-hook 'org-mode-hook '(lambda ()
;;                             (remove-hook 'before-save-hook 'delete-trailing-whitespace)
;;                             ))

(setq whitespace-global-modes '(not makefile-mode))
(global-whitespace-mode 1)

;; 这个到底是否需要？ 不确定，先注释看看效果。
;; (turn-on-eldoc-mode)

(add-hook 'prog-mode-hook '(lambda ()
                             (font-lock-add-keywords
                              nil '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\|NOTICE\\|WARN\\):"
                                     1 font-lock-warning-face t)))
                             (subword-mode)           ; 不要全局开启 subword-mode，对 ido 有影响。
                             (goto-address-prog-mode)
                             (display-fill-column-indicator-mode) ;; 全局开启会造成 helm 也显示.
                             (setq-local indent-tabs-mode nil)      ;禁止 insert \t 字符.

                             ;; 注意最后一个参数 t, 这确保了当前 before-save-hook 是 local 的。
                             (add-hook 'before-save-hook
                                       (lambda()
                                         (save-excursion
                                           ;; (whitespace-cleanup)
                                           (delete-trailing-whitespace)))
                                       nil t)
                             ))

(add-hook 'text-mode-hook 'goto-address-mode)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'sgml-mode-hook 'display-line-numbers-mode)
(add-hook 'feature-mode-hook 'display-line-numbers-mode)
(add-hook 'yaml-mode-hook 'display-line-numbers-mode)

(delete-selection-mode t)                       ;选区替换模式.

(setq auto-revert-interval 0.5)
;; (setq auto-revert-verbose nil)

(add-hook 'prog-mode-hook 'goto-address-mode)

;; 这个同时看两个 buffer 很有用.
;; (follow-mode t)

;; (add-hook 'prog-mode-hook 'hs-minor-mode)
;; 这里额外启用了 :box t 属性使得提示更加明显
;; (defconst hideshow-folded-face '((t (:inherit 'font-lock-comment-face :box t))))

(windmove-default-keybindings) ;; Shift+方向键选择窗口q
;; 将所有开启的窗口布局, 放入一个 stack, 可以通过 C-c <left> 或 C-c <right> 浏览 stack
(winner-mode t)

(blink-cursor-mode -1)                  ;光标不闪
(transient-mark-mode t)
(column-number-mode t)                  ;显示列号码
(tooltip-mode -1)                       ;在minibuffer显示菜单帮助提示
;; (display-time-mode t)                   ;显示时间
(show-paren-mode t)                     ;一直显示括号
(auto-image-file-mode t)                     ;对于图片扩展名的文件显示图片

;; 这个鬼设置有啥用？ 忘记了。
;; (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)

(electric-pair-mode t)
;; electric-pair-text-pairs 中的字符，仅仅在 comment/string 中生效。
(add-to-list 'electric-pair-text-pairs '(?` . ?'))
(add-to-list 'electric-pair-text-pairs '(?（ . ?）))
(remove-hook 'sh-mode-hook 'sh-electric-here-document-mode)

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

;; (file-name-shadow-mode 1)
;; (minibuffer-depth-indicate-mode 1)                   ;显示minibuffer深度

(visual-line-mode 1)
(setq visual-line-fringe-indicators '(nil right-curly-arrow))
(setq what-cursor-show-names t)
(minibuffer-electric-default-mode t)
(minibuffer-depth-indicate-mode t)
;; 开启minibuffer递归调用.
(setq enable-recursive-minibuffers t)
(setq toggle-truncate-lines nil)
(setq comment-auto-fill-only-comments t)

(provide 'base_init)

;;; base_init.el ends here
