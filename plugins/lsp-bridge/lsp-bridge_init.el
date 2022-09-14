(require 'lsp-bridge)

;; yay -S python-epc python-orjson

(delete 'dockerfile-mode lsp-bridge-default-mode-hooks)

(global-lsp-bridge-mode)

(provide 'lsp-bridge_init)

;;; lsp-bridge_init.el ends here
