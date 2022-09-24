(require 'all-the-icons)

;; 使用 (all-the-icons-insert-icons-for 'alltheicon) 来测试 icons.

(require 'all-the-icons-ibuffer)

(add-hook 'ibuffer-mode-hook #'all-the-icons-ibuffer-mode)

(with-eval-after-load 'ivy
  (require 'all-the-icons-ivy-rich)
  (all-the-icons-ivy-rich-mode 1)
  (ivy-rich-mode 1)
  (setq ivy-rich-parse-remote-buffer nil)
  )

(provide 'all-the-icons_init)

;;; all-the-icons_init.el ends here
