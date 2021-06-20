(require 'coffee-mode)
;; 这个干嘛用的？
;; (setq iced-coffee-cs-keywords '("await" "defer" "autocb"))
(setq coffee-tab-width 2)
(add-to-list 'auto-mode-alist '("\\.coffee\\.erb\\'" . coffee-mode))

(provide 'coffee-mode_init)
;;;  neotree_init.el ends here
