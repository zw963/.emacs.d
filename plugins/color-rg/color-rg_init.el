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
            (define-key color-rg-mode-map (kbd "n") 'color-rg-next-file)
            (define-key color-rg-mode-map (kbd "p") 'color-rg-prev-file)
           ))


(provide 'color-rg_init)

;;; color-rg_init.el ends here
