(require 'inf-ruby)

;; ;; 启动 console 之前, 初始化 rvm.
(advice-add 'inf-ruby-console-auto :before #'rvm-activate-corresponding-ruby)

(add-hook 'after-init-hook 'inf-ruby-switch-setup)

(run-ruby-mode-hook '(inf-ruby-minor-mode))

(add-hook 'inf-ruby-mode-hook
          '(lambda ()
             (define-key inf-ruby-mode-map [(control n)] 'comint-next-input)
             (define-key inf-ruby-mode-map [(control p)] 'comint-previous-input)
             ))

(add-hook 'compilation-filter-hook 'inf-ruby-auto-enter)

(provide 'inf-ruby_init)

;;; inf-ruby_init.el ends here
