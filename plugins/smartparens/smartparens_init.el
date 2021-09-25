(require 'smartparens-config)

(sp-local-pair 'sh-mode "'" "'")
(sp-local-pair 'web-mode "<#" "#>")
(sp-pair "\\`" "\\'" :when '(sp-in-comment-p))

(let ((pairs '(("{" nil) ("[" nil) ("(" nil))))
  (mapc
   (lambda (pair)
     (sp-pair (-first-item pair)
              (-last-item pair)
              :post-handlers
              '(:add ("||\n[i]" "RET"))))
   pairs))

(add-hook 'prog-mode-hook 'smartparens-global-mode)

(provide 'smartparens_init)

;;; smartparens_init.el ends here
