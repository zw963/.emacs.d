(setq eglot-server-programs
      `((dart-mode . ("dart" "language-server"))))

(require 'eglot_init)

(add-hook 'dart-mode-hook 'eglot-ensure)

(provide 'dart-mode-eglot_init)

;;; dart-mode-eglot_init.el ends here
