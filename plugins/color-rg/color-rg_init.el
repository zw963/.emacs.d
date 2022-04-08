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

(provide 'color-rg_init)

;;; color-rg_init.el ends here
