(require 'posframe)

;; (with-eval-after-load 'ivy
;;   (require 'ivy-posframe)
;;   ;; display at `ivy-posframe-style'
;;   (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display)))
;;   (ivy-posframe-mode 1)
;;   )

(with-eval-after-load 'company
  (require 'company-posframe)
  (require 'desktop) ;this line is needed.
  (push '(company-posframe-mode . nil)
        desktop-minor-mode-table)
  )

;; (with-eval-after-load 'flycheck
;;   (require 'flycheck-posframe)
;;   (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode)
;;   )

(provide 'posframe_init)

;;; posframe_init.el ends here
