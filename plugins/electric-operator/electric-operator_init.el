(require 'electric-operator)

(add-hook 'crystal-mode-hook #'electric-operator-mode)
(add-hook 'org-mode-hook #'electric-operator-mode)

(electric-operator-add-rules-for-mode 'org-mode
                                      (cons "#=>" "# => "))

(electric-operator-add-rules-for-mode 'crystal-mode
                                      (cons "#=>" "# => "))

(provide 'electric-operator_init)

;;; electric-operator_init.el ends here
