;; -*- lexical-binding: t; -*-

(require 'indent-bars-ts)

(setq indent-bars-treesit-support t)

(dolist (hook '(coffee-mode-hook
                feature-mode-hook
                yaml-mode-hook
                yaml-ts-mode-hook
                python-mode-hook
                dart-mode-hook
                slim-mode-hook
                web-mode-hook
                crystal-mode-hook
                bash-ts-mode-hook
                ))
  (add-hook hook 'indent-bars-mode))


;; (dolist (hook '(
;;                 yaml-ts-mode-hook
;;                 bash-ts-mode-hook
;;                 ))
;;   (add-hook hook 'indent-bars-mode))

(provide 'indent-bars_init)

;;; indent-bars_init.el ends here
