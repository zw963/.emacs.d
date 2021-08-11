(require 'lsp-mode)
(require 'lsp-completion)
(setq lsp-completion-provider :capf) ; 这是默认值, lsp 需要使用这个来 completion.
;; (setq lsp-enable-semantic-highlighting t)

;; try disable watch file for performance reason, don't know it impact yet.
;; 下面的两个一个注释，另一个取消注释.
;; (setq lsp-enable-file-watchers nil)
(setq lsp-file-watch-threshold 3000)

(setq lsp-ui-doc-position 'at-point)

(with-eval-after-load 'which-key
  (add-hook 'lsp-after-open-hook 'lsp-enable-which-key-integration))

(require 'lsp-modeline)
;; 用来在 minibuffer 显示 code actions 信息。
(lsp-modeline-code-actions-mode nil) ;; 默认开启的
;; (setq lsp-modeline-code-actions-segments '(count icon name))
;; (setq lsp-modeline-workspace-status-enable t)

(require 'lsp-diagnostics) ;; 在 mode-line 上显示错误数字图表.
(setq lsp-modeline-diagnostics-enable nil) ;; 默认开启的
;; (setq lsp-diagnostics-provider :flycheck) ; 硬编码为 flycheck.
;; (setq lsp-modeline-diagnostics-scope :project)

;; (setq lsp-enable-indentation nil)
;; (setq lsp-enable-on-type-formatting nil)

;; 如果退出 lsp buffer, 自动 kill 掉 lsp-server.
;; (setq lsp-keep-workspace-alive nil)

(require 'lsp-headerline)
;; 在顶部 tabbar 那里，显示一个 headline, 很好看.
(setq lsp-headerline-breadcrumb-enable t)
;; for more customize, check lsp-headerline-breadcrumb-segments
(setq lsp-headerline-breadcrumb-enable-symbol-numbers t)

(require 'lsp-lens)
(setq lsp-lens-enable t)

;; lsp-ui auto config completion, code-actions, breadcrumb, ‘flycheck’,
;;‘flymake’, ‘imenu’, symbol highlighting, lenses, links, and so on.

(require 'lsp-ui)
(with-eval-after-load 'lsp-ui
  ;; 关闭这个，会让 diagnostics(从 lsp-server 返回的诊断信息) 和 flycheck 信息在 minibuffer 合并显示.
  (setq lsp-ui-sideline-show-diagnostics nil)

  ;; 隐藏右边乱七八糟一大堆信息，暂时看不懂到底有什么用。
  (setq lsp-ui-sideline-show-hover nil) ; 这是默认值

  ;; code actions 是问题的修复策略.
  (setq lsp-ui-sideline-show-code-actions t) ; 这是默认值

  (setq lsp-ui-doc-enable t) ; 这是默认值
  (setq lsp-ui-doc-delay 3)

  (setq lsp-ui-imenu-enable t)

  ;; (require 'lsp-ui-flycheck)
  ;; (lsp-ui-flycheck-list-mode)

  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  )

(require 'lsp-iedit)
(with-eval-after-load 'lsp-iedit
  (define-key lsp-mode-map [(control \;)] #'lsp-iedit-highlights)
  )

;; (require 'lsp-ivy)
;; (with-eval-after-load 'lsp-ivy
;;   ;; (define-key lsp-mode-map [(control c) (s)] #'lsp-ivy-workspace-symbol)
;;   ;; (define-key lsp-mode-map [(control c) (S)] #'lsp-ivy-global-workspace-symbol)
;;   (define-key lsp-mode-map [remap xref-find-apropos] #'lsp-ivy-workspace-symbol)
;;   )

(require 'helm-lsp)
(with-eval-after-load 'helm-lsp
  ;; Ctrl+Alt+.
  (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol)

  ;; (define-key lsp-mode-map [(control c) (s)] #'lsp-ivy-workspace-symbol)
  ;; (define-key lsp-mode-map [(control c) (S)] #'lsp-ivy-global-workspace-symbol)
  )

(with-eval-after-load 'treemacs
  (require 'lsp-treemacs)
  (lsp-treemacs-sync-mode 1)
  ;; (add-hook 'treemacs-switch-workspace-hook 'lsp-treemacs-symbols)
  ;; (add-hook 'treemacs-select-hook 'lsp-ui-imenu)
  ;; (add-hook 'treemacs-switch-workspace-hook 'lsp-ui-imenu)
  )

(provide 'lsp-mode_init)

;;; lsp-mode_init.el ends here
