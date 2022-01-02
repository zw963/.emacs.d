(setq dap-utils-extension-path (expand-file-name ".extension" (file-name-directory (or load-file-name buffer-file-name))))

(require 'dap-mode)
(dap-mode 1)

(require 'dap-ui)
(dap-ui-mode 1)

(require 'dap-mouse)
(dap-tooltip-mode 1)

(require 'dap-ruby)
(dap-ruby-setup)

(require 'dap-go)
(dap-go-setup)

;; Try to connect to ruby-debug-ide, but still working.

;; (dap-register-debug-provider
;;  "ruby"
;;  (lambda (conf)
;;    (plist-put conf :debugPort 1234)
;;    (plist-put conf :host "localhost")
;;    conf))

(provide 'dap-mode_init)
