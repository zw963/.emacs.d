;; (setq dap-utils-extension-path (expand-file-name ".extension" (file-name-directory (or load-file-name buffer-file-name))))
(require 'dap-mode)
(require 'dap-ui)
(require 'dap-mouse)

(dap-mode 1)
(dap-ui-mode 1)
(dap-tooltip-mode 1)

;; (require 'dap-ruby)
;; (dap-ruby-setup)

;; ;; Try to connect to ruby-debug-ide, but still working.

;; (dap-register-debug-provider
;;  "ruby"
;;  (lambda (conf)
;;    (plist-put conf :debugPort 1234)
;;    (plist-put conf :host "localhost")
;;    conf))

(provide 'dap-mode_init)
