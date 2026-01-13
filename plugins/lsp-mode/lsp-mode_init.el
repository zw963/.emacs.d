;; -*- lexical-binding: t; -*-

(setq lsp-auto-configure nil
      lsp-enable-dap-auto-configure nil
      lsp-idle-delay 0.8
      lsp-file-watch-threshold 3000
      )

(setq lsp-use-plists t) ;; 需要 early_init.el 中设定的 LSP_USE_PLISTS 环境变量

(defvar lsp-mode-common-hook-list nil
  "List of zero-arg setup thunks executed after `lsp-mode' is enabled in a buffer.")

(defmacro lsp-mode-common-add (&rest body)
  "Append a per-buffer setup thunk into `lsp-mode-common-hook-list`.
If BODY is empty, do nothing and return nil."
  (declare (indent 0))
  (when body
    `(add-to-list 'lsp-mode-common-hook-list
                  (lambda () ,@body)
                  t)))

(defun lsp-mode-common--apply ()
  "Run all per-buffer LSP setup thunks once LSP is on."
  (dolist (fn lsp-mode-common-hook-list)
    (funcall fn)))

(defun lsp-mode-common-hooks ()
  "Entry point added to major-mode hooks (e.g. rust-mode-hook)."
  ;; 关键：把你的配置延后到 lsp-mode 真正开启之后执行，避免你现在那种“还没 lsp-mode 就先开一堆子 mode”的不稳定行为。
  (when (bound-and-true-p eglot--managed-mode)
    (error "eglot and lsp-mode are both active in this buffer"))
  (add-hook 'lsp-mode-hook #'lsp-mode-common--apply nil t)
  (lsp-deferred))

(require 'lsp-mode)
;; (setq lsp-log-io t)
;; (setq debug-on-error t)              ;需要调试时，开启这个。
;; (setq no-byte-compile t)
(define-key lsp-signature-mode-map (kbd "M-n") nil)
(define-key lsp-signature-mode-map (kbd "M-p") nil)
(add-to-list 'lsp-language-id-configuration '("\\.erb$" . "html"))
(add-to-list 'lsp-language-id-configuration '("\\.ecr$" . "html"))
;; ;; 下面是默认值
;; (setq
;; lsp-auto-guess-root nil ;; 尝试 guess root, 打开这个可能造成跳转入 lsp-dart 依赖的库文件之后，无法再次跳转。
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
(lsp-mode-common-add
  (zw/company-remove-tabnine)
  (setq lsp-format-buffer-on-save t)
  ;; (setq lsp-format-buffer-on-save-list nil) ; 默认值, 所有模式都格式化
  (lsp-signature-mode t)  ;; show signature popup.
  ;; 这个 打开时，lsp-dart 非常卡，这里尝试 Crystal 模式打开。
  ;; (setq lsp-signature-auto-activate t)
  ;; 开启 lsp 的时候，同时打开 treemacs 窗口
  (when (featurep 'treemacs) (save-selected-window (treemacs-select-window)))
  )


(require 'lsp-semantic-tokens)
(lsp-mode-common-add
  (lsp-semantic-tokens-mode t) ;; ;; 这个默认不打开，怀疑打开会很慢，先试试
  )


(require 'lsp-completion)
(lsp-mode-common-add
  (lsp-completion-mode 1))


;; (require 'lsp-modeline)
(lsp-mode-common-add
  ;; (lsp-modeline-workspace-status-mode t) ;; workspace status on modeline.
  ;; 这两个在 lsp-ui 也存在同样的设置, 因此关掉
  ;; (lsp-modeline-code-actions-mode t) ;; code actions on modeline.
  ;; (lsp-modeline-diagnostics-mode t)
  )


(require 'lsp-lens)
(lsp-mode-common-add
  (lsp-lens-mode t) ;; code-lens overlays.
  )


;; 必须手动 require headline 和 diagnostics 两个，才会有 flycheck 的小红线提示错误信息。
(require 'lsp-headerline) ;; 会 require lsp-icons
(lsp-mode-common-add
  (lsp-headerline-breadcrumb-mode t) ;; breadcrumb on headerline.
  ;; for more customize, check lsp-headerline-breadcrumb-segments
  (setq lsp-headerline-breadcrumb-enable-symbol-numbers t)
  )


(require 'lsp-diagnostics)
(lsp-mode-common-add
  (lsp-diagnostics-mode t)  ;; 集成 flycheck
  )


(require 'lsp-dired)
(lsp-mode-common-add
  (lsp-dired-mode t)
  )


;; (require 'helm-lsp)
;; (with-eval-after-load 'helm-lsp
;;   (define-key lsp-mode-map [(control c) (S)] #'helm-lsp-global-workspace-symbol)
;;   ;; Ctrl+Alt+.
;;   (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol)
;;   )


;; lsp-ui auto config completion, code-actions, breadcrumb, ‘flycheck’,
;;‘flymake’, ‘imenu’, symbol highlighting, lenses, links, and so on.
(require 'lsp-ui)
(with-eval-after-load 'lsp-ui
  ;; 在右侧显示 diagnostics (从 lsp-server 返回的诊断信息)。
  ;; 关闭这个，diagnostics 和 flycheck 信息会在 minibuffer 合并显示。
  (setq lsp-ui-sideline-show-diagnostics nil)
  ;; code actions 是问题的修复策略, 在右侧显示问题的修复策略。
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-doc-delay 1)
  (setq lsp-ui-sideline-delay 0.5)
  (setq lsp-ui-sideline-update-mode 'line)
  ;; (setq lsp-ui-sideline-show-hover nil) ; 这是默认值
  ;; M-.
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  ;; M-?
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (define-key lsp-ui-mode-map [(control h) (d)] #'lsp-describe-thing-at-point)
  (lsp-mode-common-add
    (lsp-ui-mode t)
    (lsp-ui-sideline-mode t)
    (lsp-ui-imenu-buffer-mode t)
    (lsp-ui-doc-mode t)
    ))


(with-eval-after-load 'treemacs
  (require 'lsp-treemacs)
  (lsp-treemacs-sync-mode 1)
  (setq lsp-treemacs-error-list-current-project-only t)
  (add-hook 'treemacs-switch-workspace-hook 'lsp-treemacs-symbols)
  )

;; (lsp-installation-buffer-mode)
;; (lsp-inlay-hints-mode)

(setq
 ;; lsp-enable-file-watchers nil

 ;; 如果退出最后一个 lsp buffer, 自动 kill 掉 lsp-server，否则 Emacs 会很慢。
 ;; lsp-keep-workspace-alive nil

 )

;; 下面是这些新的配置相对于老的配置, 恢复成了默认值

;; 1. lsp-modeline-code-actions-enable 从 nil 改为默认值 t
;; 2. lsp-ui-doc-position 从 at-point 改为默认值 top
;; 3. lsp-modeline-diagnostics-enable 从 nil 改为默认值 t

;; 这个不是每个 backend 都支持
;; (require 'lsp-iedit)
;; (define-key lsp-mode-map [(control \;)] #'lsp-iedit-highlights)

;; lsp-ui-imenu 可能执行失败
;; (add-hook 'treemacs-switch-workspace-hook 'lsp-ui-imenu)
;; (add-hook 'treemacs-select-functions 'lsp-ui-imenu)

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

;; (require 'dap-mode_init)

(defun zw/company-remove-tabnine ()
  "Remove company-tabnine from `company-backends` buffer-locally."
  (when (boundp 'company-backends)
    (setq-local company-backends
                (delq nil
                      (mapcar (lambda (b)
                                (cond
                                 ((eq b 'company-tabnine) nil)
                                 ((listp b)
                                  (let ((x (remove 'company-tabnine b)))
                                    (and x x))) ; 空组变 nil
                                 (t b)))
                              company-backends)))))

(provide 'lsp-mode_init)
;;; lsp-mode_init.el ends here

;; =============== 不用的配置 ===============

;; (require 'lsp-ivy)
;; (with-eval-after-load 'lsp-ivy
;;   (define-key lsp-mode-map [(control c) (S)] #'lsp-ivy-global-workspace-symbol)
;;   ;; Ctrl+Alt+.
;;   (define-key lsp-mode-map [remap xref-find-apropos] #'lsp-ivy-workspace-symbol)
;;   )

;; (defun lsp--describe-thing-at-point! ()
;;   (interactive)
;;   (lsp-describe-thing-at-point)
;;   (with-current-buffer (get-buffer-create "*lsp-help*")
;;     (lsp-ui-doc-frame-mode)))
