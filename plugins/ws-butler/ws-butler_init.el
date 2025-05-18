;; -*- lexical-binding: t; -*-

(require 'ws-butler)

(add-hook 'prog-mode-hook #'ws-butler-mode)
(add-hook 'yaml-mode-hook #'ws-butler-mode)
(add-hook 'yaml-ts-mode-hook #'ws-butler-mode)

(provide 'ws-butler_init)

;;; ws-butler_init.el ends here
