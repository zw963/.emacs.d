(setq dap-utils-extension-path (expand-file-name ".extension" (file-name-directory (or load-file-name buffer-file-name))))

(require 'dap-mouse)
(require 'dapui)

;; (dap-mode 1)
;; (dap-tooltip-mode 1)
;; (dap-ui-mode 1)

;; Delete top controll menu
;; (setq dap-auto-configure-features
;;       (delete 'controls
;;               dap-auto-configure-features))

;; (require 'dap-hydra)

;; Try to connect to ruby-debug-ide, but still working.

;; (dap-register-debug-provider
;;  "ruby"
;;  (lambda (conf)
;;    (plist-put conf :debugPort 1234)
;;    (plist-put conf :host "localhost")
;;    conf))

(provide 'dap-mode_init)
