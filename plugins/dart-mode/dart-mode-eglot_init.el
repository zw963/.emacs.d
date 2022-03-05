(require 'eglot_init)

(setq eglot-server-programs (remove '(dart-mode "dart_language_server") eglot-server-programs))
(add-to-list 'eglot-server-programs '(dart-mode . ("dart" "language-server")))

(add-hook 'dart-mode-hook 'eglot-ensure)

(provide 'dart-mode-eglot_init)

;;; dart-mode-eglot_init.el ends here
