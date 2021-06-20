(require 'workgroups)
(setq wg-prefix-key (kbd "C-c w"))
(setq wg-switch-on-load nil)
(setq wg-morph-on nil)
(workgroups-mode)
(wg-load (expand-file-name "workspace"))

(provide 'workgroups_init)
;;; workgroups_init.el ends here
