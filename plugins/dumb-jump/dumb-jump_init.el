(require 'dumb-jump)

(setq dumb-jump-force-searcher 'rg)

(defun set-dumb-jump-as-default ()
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate 90 t)
  (setq-local xref-show-definitions-function #'xref-show-definitions-completing-read)
  )

;; (add-hook 'crystal-mode-hook 'set-dumb-jump-as-default)
(add-hook 'prog-mode-hook 'set-dumb-jump-as-default)

(defhydra dumb-jump-hydra (:color blue :columns 3)
    "Dumb Jump"
    ("j" dumb-jump-go "Go")
    ("o" dumb-jump-go-other-window "Other window")
    ("e" dumb-jump-go-prefer-external "Go external")
    ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
    ("i" dumb-jump-go-prompt "Prompt")
    ("l" dumb-jump-quick-look "Quick look")
    ("b" dumb-jump-back "Back"))

(provide 'dumb-jump_init)

;;; dumb-jump_init.el ends here
