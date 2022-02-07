(require 'lsp-mode)
(require 'lsp-completion)

;; 如果退出最后一个 lsp buffer, 自动 kill 掉 lsp-server，否则 Emacs 会很慢。
;; (setq lsp-keep-workspace-alive nil)

(require 'lsp-headerline)
(require 'lsp-diagnostics)
;; 必须手动 require headerline 和 diagnostics 两个，才会有 flycheck 的小红线提示错误信息。
;; 如果没有小红线，可能很难发现一些错误。

;; (setq lsp-log-io t)

;; 这个不开，跳转的时候可能也会出错。
(require 'lsp-modeline)

(require 'lsp-icons)

;; 尝试 guess root.
(setq lsp-auto-guess-root t)

;; try disable watch file for performance reason, don't know it impact yet.
;; 下面的两个一个注释，另一个取消注释.
;; (setq lsp-enable-file-watchers nil)
(setq lsp-file-watch-threshold 3000)

;; for more customize, check lsp-headerline-breadcrumb-segments
(setq lsp-headerline-breadcrumb-enable-symbol-numbers t)

(setq lsp-signature-function 'lsp-signature-posframe)

;; (setq lsp-enable-on-type-formatting nil)

(defun lsp-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    ;; (add-hook 'before-save-hook #'lsp-organize-imports t t)
    )

(add-hook 'go-mode-hook 'lsp-install-save-hooks)
(add-hook 'dart-mode-hook 'lsp-install-save-hooks)

;; 这个默认不打开，怀疑打开会很慢，先关闭
;; (setq lsp-semantic-tokens-enable t)

;; lsp-dart not support set this.
;; (setq lsp-use-plists t)

;; 下面是这些新的配置相对于老的配置的一些改动：

;; 1. lsp-modeline-code-actions-enable 从 nil 改为默认值 t
;; 2. lsp-ui-doc-position 从 at-point 改为默认值 top
;; 3. lsp-modeline-diagnostics-enable 从 nil 改为默认值 t

(require 'lsp-dired)
(lsp-dired-mode t)

;; lsp-ui auto config completion, code-actions, breadcrumb, ‘flycheck’,
;;‘flymake’, ‘imenu’, symbol highlighting, lenses, links, and so on.

(require 'lsp-ui)
(with-eval-after-load 'lsp-ui
  ;; 关闭这个，会让 diagnostics(从 lsp-server 返回的诊断信息) 和 flycheck 信息在 minibuffer 合并显示.
  (setq lsp-ui-sideline-show-diagnostics nil)

  ;; code actions 是问题的修复策略, 在右侧显示问题的修复策略。
  (setq lsp-ui-sideline-show-code-actions t)

  (setq lsp-ui-doc-delay 3)

  ;; (setq lsp-ui-sideline-show-hover nil) ; 这是默认值
  ;; (setq lsp-ui-doc-enable t) ; 这是默认值
  ;; (setq lsp-ui-imenu-enable t) ; 这是默认值

  ;; (require 'lsp-ui-flycheck)
  ;; (lsp-ui-flycheck-list-mode)

  ;; M-.
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  ;; M-?
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  )

;; 这个不是每个 backend 都支持
;; (require 'lsp-iedit)
;; (with-eval-after-load 'lsp-iedit
;;   (define-key lsp-mode-map [(control \;)] #'lsp-iedit-highlights)
;;   )

;; (require 'lsp-ivy)
(with-eval-after-load 'lsp-ivy
  (define-key lsp-mode-map [(control c) (S)] #'lsp-ivy-global-workspace-symbol)
  ;; Ctrl+Alt+.
  (define-key lsp-mode-map [remap xref-find-apropos] #'lsp-ivy-workspace-symbol)
  )

(require 'helm-lsp)
(with-eval-after-load 'helm-lsp
  (define-key lsp-mode-map [(control c) (S)] #'lsp-ivy-global-workspace-symbol)
  ;; Ctrl+Alt+.
  (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol)
  )

(with-eval-after-load 'treemacs
  (require 'lsp-treemacs)
  (lsp-treemacs-sync-mode 1)
  (setq lsp-treemacs-error-list-current-project-only t)
  ;; (add-hook 'treemacs-switch-workspace-hook 'lsp-treemacs-symbols)
  ;; (add-hook 'treemacs-select-hook 'lsp-ui-imenu)
  ;; (add-hook 'treemacs-switch-workspace-hook 'lsp-ui-imenu)
  )

;; (with-eval-after-load 'company
;;   (add-hook 'lsp-mode-hook
;;             (lambda ()
;;               (setq-local company-format-margin-function #'company-vscode-dark-icons-margin))))

(with-eval-after-load 'which-key
  (add-hook 'lsp-after-open-hook 'lsp-enable-which-key-integration))

(provide 'lsp-mode_init)

;;; lsp-mode_init.el ends here
