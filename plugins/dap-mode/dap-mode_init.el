(setq dap-utils-extension-path (expand-file-name ".extension" (file-name-directory (or load-file-name buffer-file-name))))
(require 'dap-mode)
(require 'dap-ruby)
(require 'dap-ui)
(dap-ruby-setup)

;; Try to connect to ruby-debug-ide, but still working.

(dap-register-debug-provider
 "ruby"
 (lambda (conf)
   (plist-put conf :debugPort 1234)
   (plist-put conf :host "localhost")
   conf))

(dap-mode)
(dap-ui-mode)

(provide 'dap-mode_init)
