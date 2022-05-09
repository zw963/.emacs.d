(require 'color-rg)

(setq color-rg-search-ignore-rules "\
-g \"!*~\" \
-g \"!#*#\" \
-g \"!*.min.*\" \
-g \"!TAGS\" \
-g \"!tags\" \
-g \"!.git/\" \
-g \"!node_modules\" \
-g \"!dist\" \
-g \"!*.log\" \
")

(add-hook 'color-rg-mode-hook
          (lambda ()
            (define-key color-rg-mode-map (kbd "n") 'color-rg-jump-next-file)
            (define-key color-rg-mode-map (kbd "p") 'color-rg-jump-prev-file)
            ))

(global-set-key [(control meta r)] 'color-rg-search-symbol-in-project)


(provide 'color-rg_init)

;;; color-rg_init.el ends here
