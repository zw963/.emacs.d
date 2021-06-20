(with-eval-after-load 'lsp-mode
  (require 'lsp-solargraph)

  ;; (setq lsp-solargraph-use-bundler t)
  ;; (setq lsp-solargraph-log-level "debug")
  ;; (setq lsp-print-io t)

  (defun zw/lsp-solargraph-tcp-connect-to-exists-port ()
    (list
     :connect (lambda (filter sentinel name _environment-fn)
                (let* ((host "localhost")
                       (port 7658)
                       (tcp-proc (lsp--open-network-stream host port (concat name "::tcp"))))

                  (set-process-query-on-exit-flag tcp-proc nil)
                  (set-process-filter tcp-proc filter)
                  (set-process-sentinel tcp-proc sentinel)
                  (cons tcp-proc tcp-proc)))
     :test? (lambda () t)))

  (lsp-register-client
   (make-lsp-client
    :new-connection (zw/lsp-solargraph-tcp-connect-to-exists-port)
    :major-modes '(ruby-mode enh-ruby-mode)
    :priority -1
    :multi-root t
    :server-id 'ruby-ls
    :initialized-fn (lambda (workspace)
                      (with-lsp-workspace workspace
                        (lsp--set-configuration
                         (lsp-configuration-section "solargraph"))))))

  (dolist (hook (list
                 'enh-ruby-mode-hook
                 'ruby-mode
                 ))
    ;; 这里运行 lsp-deferred 会同时启动 server 以及 lsp-mode.
    (add-hook hook 'lsp-deferred))
  )

(provide 'ruby-mode-lsp_init)

;;; ruby-mode-lsp_init.el ends here
