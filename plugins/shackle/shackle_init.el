;; ;; -*- lexical-binding: t; -*-

(require 'shackle)

;; TODO: 还有一些 treemacs 下面的固定窗口，记得设定 shackle 和 popper.
;; https://github.com/emacs-lsp/lsp-treemacs?tab=readme-ov-file#views

;; gpt 编写的 C-g 自动关闭弹出窗口的方案。(无须指定 autoclose)
(with-eval-after-load 'shackle
  ;; 只记录“允许 C-g 自动关闭”的窗口（栈：最新在最前）
  (defvar zw/shackle-autoclose-windows nil
    "Stack of live shackle popup windows that can be closed by C-g.")

  (defun zw/shackle--cleanup-autoclose-windows ()
    (setq zw/shackle-autoclose-windows
          (cl-remove-if-not #'window-live-p zw/shackle-autoclose-windows)))

  (defun zw/shackle--mark-autoclose-window (win)
    (when (window-live-p win)
      (set-window-parameter win 'zw/shackle-autoclose t)
      (setq zw/shackle-autoclose-windows
            (cons win (delq win zw/shackle-autoclose-windows)))))

  (defun zw/shackle--display-buffer-track-autoclose (orig buffer alist plist)
    (let ((win (funcall orig buffer alist plist)))
      (when (and (window-live-p win)
                 (plist-get plist :autoclose) ;; 仅当 rule plist 里 :autoclose t
                 ;; (not (plist-get plist :select)) ;; 且没有 :select t 才标记
                 )
        (zw/shackle--mark-autoclose-window win))
      win))

  ;; 避免重复叠加 advice（你反复 eval 配置时很常见）
  (advice-remove #'shackle-display-buffer #'zw/shackle--display-buffer-track-autoclose)
  (advice-add    #'shackle-display-buffer :around #'zw/shackle--display-buffer-track-autoclose)

  (defun zw/shackle-close-autoclose-popup ()
    "Close current shackle autoclose popup window, or the latest one.
Return non-nil if a window is closed."
    (zw/shackle--cleanup-autoclose-windows)
    (let* ((sel (selected-window))
           (win (cond
                 ;; 优先关当前窗口（如果它被标记为 autoclose）
                 ((window-parameter sel 'zw/shackle-autoclose) sel)
                 ;; 否则关最近弹出的 autoclose 窗口
                 (t (car zw/shackle-autoclose-windows)))))
      (when (and (window-live-p win)
                 ;; 只有一个 window 时别乱删，把 C-g 留给原本语义
                 (not (one-window-p t)))
        (delete-window win)
        (setq zw/shackle-autoclose-windows (delq win zw/shackle-autoclose-windows))
        t)))

  (defun zw/shackle--keyboard-quit-close-autoclose (orig &rest args)
    (cond
     ;; 有选区时，C-g 保持原语义：取消选区
     ((region-active-p) (apply orig args))
     ;; minibuffer 里别抢 C-g
     ((minibufferp) (apply orig args))
     ;; 否则先尝试关 autoclose popup，关不掉再走原来的
     ((zw/shackle-close-autoclose-popup) nil)
     (t (apply orig args))))

  (advice-remove #'keyboard-quit #'zw/shackle--keyboard-quit-close-autoclose)
  (advice-add    #'keyboard-quit :around #'zw/shackle--keyboard-quit-close-autoclose)

  (setq
   shackle-default-size 0.618  ;; 这个仅仅在设定了 :align ??? 之后才作为默认值有效。
   shackle-default-alignment 'below ;; 这个仅在指定 :align t 的时候生效。
   shackle-select-reused-windows t
   ;; shackle-default-rule nil ;; 这是默认值
   )

  (setq shackle-rules
        '(
          (("*Warnings*" "*Messages*" "*Completions*" "*Alerts*" "*Compile-Log*") :align t :size 0.33 :autoclose t)
          ;; ("^\\*.*Shell Command.*\\*$" :regexp t :align t :size 0.33 :autoclose t)
          ;; (("*lsp session*" "*LSP Error List*") :align t :size 0.33 :autoclose t)
          ;; (("*DAP Templates*" dap-server-log-mode hover-mode) :align t :size 0.33 :autoclose t)
          ;; *Crystal-Context* 和 *Crystal-spec* 不工作
          ;; (("*Crystal-Expand*" "*Crystal-Implementations") :align right :autoclose t)
          ;; (("\\*RuboCop ") :regexp t :size 0.3 :autoclose t)

          (("*Help*" "*Backtrace*" "*Ibuffer*" "*Buffer List*" "*color-rg*" "*quickrun*") :select t :align right :size 0.5 :autoclose t)
          (" *undo-tree*" :select t :align right :size 0.33)
          ("\\*mu4e-" :regexp t :select t :align right :size 0.5)
          (vc-annotate-mode :select t :align right :size 0.33 :autoclose t)
          ("*vc-change-log*" :select t :align right :size 0.33 :autoclose t)
          (special-mode :select t :align t)
          ;; "*Apropos*" "*Occur*" "*lsp-help*"
          ;; ((compilation-mode rustic-compilation-mode
          ;;                    rustic-cargo-clippy-mode rustic-cargo-test-mode
          ;;                    cargo-process-mode)
          ;;  :select t :align t :size 0.5)
          ;; ((flycheck-error-list-mode flymake-diagnostics-buffer-mode) :select t :align t :size 0.33)
          (("*vterm*" inf-ruby-mode) :select t :align above :popup t)
          ;; ((grep-mode rg-mode deadgrep-mode ag-mode pt-mode) :select t :align t)
          ;; (("*shell*" "*eshell*" "*ielm*") :select t :align t)
          ;; (("*Org Agenda*" " *Agenda Commands*" " *Org todo*" "*Org Dashboard*" "*Org Select*") :select t)
          ;; (("\\*Capture\\*" "^CAPTURE-.*\\.org*") :regexp t :select t)
          ;; (("*rustfmt*" "*Gofmt Errors*" "*Go Test*") :select t)
          ;; (profiler-report-mode :select t :size 0.5 )
          ))

  (with-eval-after-load 'helm
    ;; 让 helm 的弹出窗口有类似于 popwin 的效果, 并被 shackle 管理
    (setq helm-display-function 'pop-to-buffer) ; make helm play nice
    (add-to-list 'shackle-rules '("\\`\\*helm.*?\\*\\'" :regexp t :align right :size 0.4))
    (add-to-list 'shackle-rules '(" *Minibuf-1*"  :align right)) ;;i git-grep 以及 helm-ag
    (add-to-list 'shackle-rules '("\\`\\*hgrep.*?\\*\\'" :regexp t :align right))  ;; F3 使用 wgrep 编辑
    )

  (with-eval-after-load 'ibuffer
    (setq ibuffer-use-other-window t) ; 为了能让 shackle 管理 ibuffer, 必须
    )

  (shackle-mode t)
  )

(provide 'shackle_init)
;;; shackle_init.el ends here

;; (setq     '(
;;             (("*prettier errors*") :select t :size 0.3  )
;;             (" *Flycheck checkers*" :select t :size 0.3  )
;;             ("*ELP Profiling Restuls*" :select t :size 0.5 )
;;             (godoc-mode :select t :size 0.4  )
;;             ("*tldr*" :size 0.4  )
;;             ("*Finder*" :select t :size 0.3  )
;;             ("^\\*macro expansion\\**" :regexp t :size 0.4 )
;;             ("^\\*elfeed-entry" :regexp t :size 0.7  )
;;             (" *Install vterm* " :size 0.35 :same t )
;;             (("*Paradox Report*" "*package update results*") :size 0.2  )
;;             ("*Package-Lint*" :size 0.4  )
;;             ("*How Do You*" :select t :size 0.5  )
;;             (comint-mode :select t :size 0.4  )
;;             ("*Pp Eval Output*" :size 15  )
;;             ("\\*[Wo]*Man.*\\*" :regexp t :select t  )
;;             ("*Calendar*" :select t :size 0.3 )
;;             (("*shell*" "*eshell*" "*ielm*") :popup t :size 0.3 )
;;             ("^\\*vc-.*\\*$" :regexp t :size 0.3  )
;;             ("*gud-debug*" :select t :size 0.4  )
;;             ("\\*ivy-occur .*\\*" :regexp t :select t :size 0.3 )
;;             ("*ert*" :size 15  )
;;             (overseer-buffer-mode :size 15  )
;;             (Buffer-menu-mode :select t :size 0.5  )
;;             (gnus-article-mode :select t :size 0.7  )
;;             (devdocs-mode :select t :size 0.4  )
;;             ((process-menu-mode list-environment-mode) :select t :size 0.3 )
;;             (("*docker-containers*" "*docker-images*" "*docker-networks*" "*docker-volumes*") :size 0.4  )
;;             (bookmark-bmenu-mode :select t :size 0.4 )
;;             (tabulated-list-mode :size 0.4 )
;;             ((inferior-python-mode inf-ruby-mode swift-repl-mode) :size 0.4 )
;;             ("*prolog*" :size 0.4 )
;;             ))

;; (defconst zw/vterminal-buffer-name "*vterm*")

;; (defun zw/toggle-vterminal ()
;;   "Toggle a single dedicated vterm buffer."
;;   (interactive)
;;   (let* ((buf (get-buffer zw/vterminal-buffer-name))
;;          (win (and buf (get-buffer-window buf t))))
;;     (cond
;;      (win
;;       (delete-window win))
;;      (t
;;       (unless (buffer-live-p buf)
;;         ;; vterm 支持传入 buffer 名；如果你的版本不支持，这行会报错，那就告诉我你的 vterm 版本
;;         (setq buf (vterm zw/vterminal-buffer-name)))
;;       ;; 交给 shackle 去决定它在哪、占多大
;;       (pop-to-buffer buf)))))

;; (global-set-key (kbd "C-`") #'zw/toggle-vterminal)
