;; -*- lexical-binding: t; -*-

(require 'prescient)

;; The modes handle sorting and filtering by default.

(with-eval-after-load 'company
  (require 'company-prescient)
  (company-prescient-mode 1)
  )

(with-eval-after-load 'ivy
  (require 'ivy-prescient)
  (ivy-prescient-mode 1)
  )

(prescient-persist-mode 1)

(provide 'prescient_init)

;;; prescient_init.el ends here
