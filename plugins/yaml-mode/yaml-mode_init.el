(autoload 'yaml-mode "yaml-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.\\(e?ya?\\|ra\\)ml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\.j2" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yml" . yaml-mode))
(add-to-list 'auto-mode-alist '("Procfile\\|Procfile.dev\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("Procfile.options\\|Procfile.dev.options\\'" . yaml-mode))

(provide 'yaml-mode_init)
;;; yaml-mode_init.el ends here
