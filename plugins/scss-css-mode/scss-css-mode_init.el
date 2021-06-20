(require 'scss-css-mode)
(setq css-indent-offset 2)
(setq scss-sass-command (expand-file-name "~/utils/scss/bin/scssc"))
(when (featurep 'flycheck)
  (setq flycheck-scss-executable (expand-file-name "~/utils/scss/bin/sassc")))
(define-key scss-css-mode-map [(control c) (control c)] 'format-buffer)
(define-key scss-css-mode-map [(control c) (?\r)] 'scss-compile)

(with-eval-after-load 'auto-complete (add-to-list 'ac-modes 'scss-css-mode))

(add-to-list 'auto-mode-alist '("\\.wxss\\'" . scss-css-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . scss-css-mode))
(add-to-list 'auto-mode-alist '("\\.sass\\'" . scss-css-mode))
(add-hook 'scss-css-mode-hook 'turn-on-format-buffer)
(add-hook 'scss-css-mode-hook 'ac-css-mode-setup)
(add-hook 'scss-css-mode-hook 'rainbow-mode)

(require 'rainbow-mode)
(add-to-list 'rainbow-html-colors-major-mode-list 'scss-mode)
(add-to-list 'rainbow-html-colors-major-mode-list 'scss-css-mode)
(add-to-list 'rainbow-html-colors-major-mode-list 'less-css-mode)
(add-to-list 'rainbow-html-colors-major-mode-list 'css-mode)

(provide 'scss-css-mode_init)
;;; scss-css-mode_init.el ends here
