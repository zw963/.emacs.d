;; -*- lexical-binding: t; -*-

(require 'inf-ruby)

;; FIXME: 似乎 inf-ruby 还是需要 RVM 插件，否则，在没有 ruby 的地方启动 emacs, 甚至找不到 irb
(with-eval-after-load 'rvm
  (advice-add 'inf-ruby-console-auto :before #'rvm-activate-corresponding-ruby))

(add-hook 'after-init-hook 'inf-ruby-switch-setup)

(run-ruby-mode-hook '(inf-ruby-minor-mode))

(add-hook 'inf-ruby-mode-hook
          (lambda ()
            (define-key inf-ruby-mode-map [(control n)] 'comint-next-input)
            (define-key inf-ruby-mode-map [(control p)] 'comint-previous-input)
            ))

(add-hook 'compilation-filter-hook 'inf-ruby-auto-enter)

(provide 'inf-ruby_init)

;;; inf-ruby_init.el ends here
