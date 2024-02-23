;; (require 'dap-mode_init)
(require 'lsp-mode)
(require 'lsp-semantic-tokens)
(require 'lsp-completion)
(require 'lsp-modeline) ;; 这个不开，跳转的时候可能也会出错?
(require 'lsp-lens)
;; 必须手动 require headerline 和 diagnostics 两个，才会有 flycheck 的小红线提示错误信息。
(require 'lsp-headerline) ;; 会 require lsp-icons
(require 'lsp-diagnostics)
(require 'lsp-ui)

(defun lsp-mode-common-hooks ()
  ;; (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (setq-local company-minimum-prefix-length 1)
  (lsp-diagnostics-mode t)  ;; Toggle LSP diagnostics integration.
  (lsp-completion-mode t)  ;; Toggle LSP completion support.
  (lsp-signature-mode t)  ;; show signature popup.
  (lsp-lens-mode t) ;; code-lens overlays.
  ;; (lsp-installation-buffer-mode)
  ;; (lsp-inlay-hints-mode)

  (lsp-semantic-tokens-mode t) ;; ;; 这个默认不打开，怀疑打开会很慢，先试试

  ;; 这两个在 lsp-ui 也存在同样的设置, 因此关掉
  ;; (lsp-modeline-code-actions-mode t) ;; code actions on modeline.
  ;; (lsp-modeline-diagnostics-mode t)

  (lsp-modeline-workspace-status-mode t) ;; workspace status on modeline.
  (lsp-headerline-breadcrumb-mode t) ;; breadcrumb on headerline.

  (lsp-ui-mode t)
  (lsp-ui-sideline-mode t)
  (lsp-ui-imenu-buffer-mode t)
  ;; (lsp-ui-peek-mode t)
  (lsp-ui-doc-mode t)
  ;; 关闭这个，会让 diagnostics(从 lsp-server 返回的诊断信息) 和 flycheck 信息在 minibuffer 合并显示.
  (setq lsp-ui-sideline-show-diagnostics t)
  ;; code actions 是问题的修复策略, 在右侧显示问题的修复策略。
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-doc-delay 1)
  ;; (setq lsp-ui-sideline-show-hover nil) ; 这是默认值

  ;; M-.
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  ;; M-?
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)

  (when (featurep 'treemacs) (save-selected-window (treemacs-select-window)))

  (lsp-deferred)
  )

(setq lsp-auto-configure nil)
(setq lsp-enable-dap-auto-configure t)

;; (defun lsp-mode-common-hooks ()
;;   (when (featurep 'treemacs) (save-selected-window (treemacs-select-window)))
;;   (lsp-deferred)
;;   )

;; ;; 下面是默认值
;; (setq
;;  lsp-enable-snippet t
;;  lsp-enable-folding t
;;  lsp-enable-links t
;;  lsp-enable-imenu t
;;  lsp-enable-symbol-highlighting t
;;  lsp-enable-xref t
;;  lsp-enable-indentation t
;;  lsp-enable-on-type-formatting t ;; 这个原来关闭的, 近期打开了, 试试看
;;  lsp-enable-text-document-color t
;;  lsp-enable-suggest-server-download t
;;  lsp-eldoc-enable-hover t
;;  lsp-modeline-code-actions-enable t
;;  lsp-modeline-diagnostics-enable t
;;  lsp-lens-enable t
;;  lsp-completion-enable t
;;  lsp-modeline-workspace-status-enable t
;;  lsp-headerline-breadcrumb-enable t
;;  lsp-inlay-hint-enable nil
;;  lsp-completion-enable-additional-text-edit t
;;  lsp-headerline-breadcrumb-enable-diagnostics t
;;  lsp-headerline-breadcrumb-icons-enable t
;;  )

(setq
 ;; 下面的两个一个注释，另一个取消注释.
 ;; lsp-enable-file-watchers nil
 lsp-file-watch-threshold 3000

 ;; for more customize, check lsp-headerline-breadcrumb-segments
 lsp-headerline-breadcrumb-enable-symbol-numbers t

 ;; 如果退出最后一个 lsp buffer, 自动 kill 掉 lsp-server，否则 Emacs 会很慢。
 ;; lsp-keep-workspace-alive nil

 ;; 尝试 guess root, 打开这个可能造成跳转入 lsp-dart 依赖的库文件之后，无法再次跳转。
 lsp-auto-guess-root t
 )



;; (setq lsp-log-io t)
;; (setq debug-on-error t)              ;需要调试时，开启这个。
;; (setq no-byte-compile t)

;; 这个 打开时，lsp-dart 非常卡，建议关闭
;; (setq lsp-signature-auto-activate t)

;; 使用 posframe 挺丑的
;; (setq lsp-signature-function 'lsp-signature-posframe)

;; lsp-dart not support set this.
;; (setq lsp-use-plists t)

;; 下面是这些新的配置相对于老的配置, 恢复成了默认值

;; 1. lsp-modeline-code-actions-enable 从 nil 改为默认值 t
;; 2. lsp-ui-doc-position 从 at-point 改为默认值 top
;; 3. lsp-modeline-diagnostics-enable 从 nil 改为默认值 t

(require 'lsp-dired)
(lsp-dired-mode t)

;; lsp-ui auto config completion, code-actions, breadcrumb, ‘flycheck’,
;;‘flymake’, ‘imenu’, symbol highlighting, lenses, links, and so on.
;; (with-eval-after-load 'lsp-ui
;;   (setq lsp-ui-sideline-enable t)
;;   (setq lsp-ui-imenu-enable t)
;;   ;; (setq lsp-ui-doc-enable t) ; 这是默认值
;;   ;; (setq lsp-ui-imenu-enable t) ; 这是默认值
;;   )

;; 这个不是每个 backend 都支持
;; (require 'lsp-iedit)
;; (define-key lsp-mode-map [(control \;)] #'lsp-iedit-highlights)

;; (require 'lsp-ivy)
(with-eval-after-load 'lsp-ivy
  (define-key lsp-mode-map [(control c) (S)] #'lsp-ivy-global-workspace-symbol)
  ;; Ctrl+Alt+.
  (define-key lsp-mode-map [remap xref-find-apropos] #'lsp-ivy-workspace-symbol)
  )

(require 'helm-lsp)
(with-eval-after-load 'helm-lsp
  (define-key lsp-mode-map [(control c) (S)] #'helm-lsp-global-workspace-symbol)
  ;; Ctrl+Alt+.
  (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol)
  )

(with-eval-after-load 'treemacs
  (require 'lsp-treemacs)
  (lsp-treemacs-sync-mode 1)
  (setq lsp-treemacs-error-list-current-project-only t)
  )

;; 想完成本 buffer 内的内容，需要加入 company-dabbrev-code
;; 但是，如果开启这个，会让 lsp 出现很多和上下文无关的结果。
;; 因此只是有选择的在几个没有开启 lsp 的模式下开启。

;; (add-hook 'prog-mode-hook
;;           (lambda ()
;;             ;; 如果没有使用 lsp, 返回多一点结果。
;;             (unless (bound-and-true-p lsp-mode)
;;               (add-to-list 'company-backends '(company-capf company-dabbrev-code company-keywords))
;;               )
;;             ))

;; (with-eval-after-load 'company
;;   (add-hook 'lsp-mode-hook
;;             (lambda ()
;;               (set (make-local-variable 'company-backends)
;;                    (delete '(company-dabbrev-code company-gtags company-etags company-keywords)
;;                            company-backends))

;;               (setq-local company-format-margin-function #'company-vscode-dark-icons-margin))
;;             ))

;; (setq lsp-disabled-clients '(html-ls emmet-ls eslint rubocop-ls semgrep-ls))

(with-eval-after-load 'which-key
  (add-hook 'lsp-after-open-hook 'lsp-enable-which-key-integration))

(provide 'lsp-mode_init)

;;; lsp-mode_init.el ends here
