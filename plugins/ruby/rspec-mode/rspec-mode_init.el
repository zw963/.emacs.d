(require 'rspec-mode)

(eval-after-load 'rspec-mode
  '(rspec-install-snippets))

;; (add-hook 'after-init-hook 'inf-ruby-switch-setup)
(setq rspec-use-rvm t)

;; (defadvice rspec-compile (around rspec-compile-around)
;;   "Use BASH shell for running the specs because of ZSH issues."
;;   (let ((shell-file-name "/bin/bash"))
;;     ad-do-it))
;; (ad-activate 'rspec-compile)

;; (setq compilation-scroll-output t)

;; (setq rspec-allow-multiple-compilation-buffers t)

(provide 'rspec-mode_init)

;;; rspec-mode_init.el ends here
