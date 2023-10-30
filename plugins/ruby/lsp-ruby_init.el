;; (with-eval-after-load 'lsp-mode
;;   (require 'lsp-solargraph)

;;   ;; (setq lsp-solargraph-use-bundler t)
;;   ;; (setq lsp-solargraph-log-level "debug")
;;   ;; (setq lsp-print-io t)

;;   (defun zw/lsp-solargraph-tcp-connect-to-exists-port ()
;;     (list
;;      :connect (lambda (filter sentinel name _environment-fn)
;;                 (let* ((host "localhost")
;;                        (port 7658)
;;                        (tcp-proc (lsp--open-network-stream host port (concat name "::tcp"))))

;;                   (set-process-query-on-exit-flag tcp-proc nil)
;;                   (set-process-filter tcp-proc filter)
;;                   (set-process-sentinel tcp-proc sentinel)
;;                   (cons tcp-proc tcp-proc)))
;;      :test? (lambda () t)))

;;   (lsp-register-client
;;    (make-lsp-client
;;     :new-connection (zw/lsp-solargraph-tcp-connect-to-exists-port)
;;     :major-modes '(ruby-mode enh-ruby-mode)
;;     :priority -1
;;     :multi-root t
;;     :server-id 'ruby-ls
;;     :initialized-fn (lambda (workspace)
;;                       (with-lsp-workspace workspace
;;                         (lsp--set-configuration
;;                          (lsp-configuration-section "solargraph"))))))

(require 'lsp-mode_init)
;; (require 'lsp-solargraph)
(require 'lsp-ruby-syntax-tree)
;; (require 'rubocop_init) ;; lsp 里面如果开了保存自动格式化，这个也要 require, 否则很慢？

;; (add-to-list 'lsp-solargraph-library-directories '("~/utils/ruby_tools/app/gems"))

;; (dolist (hook (list
;;                'enh-ruby-mode-hook
;;                'ruby-mode
;;                ))
;;   (add-hook hook (lambda ()
;;                    ;; (unless (and rvm--current-ruby rvm--current-gemset)
;;                    ;;   (rvm-activate-corresponding-ruby))
;;                    (lsp-deferred)
;;                    )))

(defun zw/lsp-ruby-common-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (lsp-deferred)
  (setq-local lsp-enable-symbol-highlighting nil)
  (setq-local lsp-lens-enable nil)
  (setq-local lsp-ui-sideline-enable nil)
  (setq-local lsp-modeline-code-actions-enable nil)
  (setq-local lsp-ui-sideline-enable nil)
  (setq-local lsp-modeline-diagnostics-enable nil)
  (setq-local lsp-signature-render-documentation nil)
  (setq-local lsp-completion-provider :none)
  (setq-local lsp-completion-show-detail nil)
  (setq-local lsp-completion-show-detail nil)
  (setq-local lsp-completion-show-kind nil)
  )

(add-hook 'ruby-mode-hook 'zw/lsp-ruby-common-hooks)
(add-hook 'enh-ruby-mode-hook 'zw/lsp-ruby-common-hooks)

;; (require 'dap-ruby)
;; (dap-ruby-setup)

(provide 'lsp-ruby_init)

;;; lsp-ruby_init.el ends here
