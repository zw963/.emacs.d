(require 'company-tabnine)

(defun add-tabnine-backend ()
  (make-local-variable 'company-backends)
  (add-to-list 'company-backends #'company-tabnine)
  )

(with-eval-after-load 'rust-mode (add-hook 'rust-mode-hook 'add-tabnine-backend))
(with-eval-after-load 'sh-mode (add-hook 'sh-mode-hook 'add-tabnine-backend))

(provide 'company-tabnine_init)

;;; company-tabnine_init.el ends here
