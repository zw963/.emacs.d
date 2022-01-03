(require 'lsp-mode)
(require 'lsp-headerline)
(require 'lsp-ui)
(require 'lsp-ivy)

;; (setq lsp-log-io t)

;; 尝试 guess root.
(setq lsp-auto-guess-root t)

;; try disable watch file for performance reason, don't know it impact yet.
;; 下面的两个一个注释，另一个取消注释.
;; (setq lsp-enable-file-watchers nil)
(setq lsp-file-watch-threshold 3000)

;; for more customize, check lsp-headerline-breadcrumb-segments
(setq lsp-headerline-breadcrumb-enable-symbol-numbers t)

(setq lsp-signature-function 'lsp-signature-posframe)

(setq lsp-modeline-code-actions-segments '(count icon name))

(setq lsp-semantic-tokens-enable t)

;; lsp-dart not support set this.
;; (setq lsp-use-plists t)

;; 下面是这些新的配置相对于老的配置的一些改动：

;; 1. lsp-modeline-code-actions-enable 从 nil 改为默认值 t
;; 2. lsp-ui-doc-position 从 at-point 改为默认值 top
;; 3. lsp-modeline-diagnostics-enable 从 nil 改为默认值 t
;; 4. lsp-ui-sideline-show-diagnostics 从 nil 改为默认值 t

;; (require 'lsp-dired)
;; (lsp-dired-mode t)

;; lsp-ui auto config completion, code-actions, breadcrumb, ‘flycheck’,
;;‘flymake’, ‘imenu’, symbol highlighting, lenses, links, and so on.
(with-eval-after-load 'lsp-ui
  ;; 关闭这个，会让 diagnostics(从 lsp-server 返回的诊断信息) 和 flycheck 信息在 minibuffer 合并显示.
  ;; (setq lsp-ui-sideline-show-diagnostics t)

  ;; 隐藏右边乱七八糟一大堆信息，暂时看不懂到底有什么用。
  ;; (setq lsp-ui-sideline-show-hover nil) ; 这是默认值

  ;; code actions 是问题的修复策略.
  ;; (setq lsp-ui-sideline-show-code-actions t) ; 这是默认值

  ;; (setq lsp-ui-doc-enable t) ; 这是默认值

  (setq lsp-ui-doc-delay 3)

  ;; (setq lsp-ui-imenu-enable t) ; 默认值

  ;; (require 'lsp-ui-flycheck)
  ;; (lsp-ui-flycheck-list-mode)

  ;; M-.
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  ;; M-?
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  )

;; 这个不是每个 backend 都支持
(require 'lsp-iedit)
(with-eval-after-load 'lsp-iedit
  (define-key lsp-mode-map [(control \;)] #'lsp-iedit-highlights)
  )

(with-eval-after-load 'lsp-ivy
  ;; (define-key lsp-mode-map [(control c) (S)] #'lsp-ivy-global-workspace-symbol)
  ;; Ctrl+Alt+.
  (define-key lsp-mode-map [remap xref-find-apropos] #'lsp-ivy-workspace-symbol)
  )

;; (require 'helm-lsp)
;; (with-eval-after-load 'helm-lsp
;;   (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol)
;;   )

(with-eval-after-load 'treemacs
  (require 'lsp-treemacs)
  (lsp-treemacs-sync-mode 1)
  (setq lsp-treemacs-error-list-current-project-only t)
  ;; (add-hook 'treemacs-switch-workspace-hook 'lsp-treemacs-symbols)
  ;; (add-hook 'treemacs-select-hook 'lsp-ui-imenu)
  ;; (add-hook 'treemacs-switch-workspace-hook 'lsp-ui-imenu)
  )

(with-eval-after-load 'company
  (add-hook 'lsp-mode-hook
            (lambda ()
              (setq-local company-format-margin-function #'company-vscode-dark-icons-margin))))

(with-eval-after-load 'which-key
  (add-hook 'lsp-after-open-hook 'lsp-enable-which-key-integration))

(provide 'lsp-mode_init)

;;; lsp-mode_init.el ends here
