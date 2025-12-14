;; -*- lexical-binding: t; -*-

(require 'all-the-icons)

;; 使用 (all-the-icons-insert-icons-for 'alltheicon) 来测试 icons.

(require 'all-the-icons-ibuffer)
(add-hook 'ibuffer-mode-hook #'all-the-icons-ibuffer-mode)

;; 这个有问题，会让 dired 无法打开，暂时关闭
 (require 'all-the-icons-dired)
 (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(with-eval-after-load 'treemacs
  ;; includes with treemacs
  (require 'treemacs-all-the-icons)
  (treemacs-load-theme "all-the-icons")
  )

(with-eval-after-load 'helm
  (require 'helm-x-icons)
  (customize-set-variable 'helm-x-icons-provider 'all-the-icons)
  )
;; (with-eval-after-load 'ivy
;;   (require 'all-the-icons-ivy-rich)
;;   (all-the-icons-ivy-rich-mode 1)
;;   (ivy-rich-mode 1)
;;   (setq ivy-rich-parse-remote-buffer nil)
;;   )

;; (require 'all-the-icons-completion)
;; (all-the-icons-completion-mode)

(provide 'all-the-icons_init)

;;; all-the-icons_init.el ends here
