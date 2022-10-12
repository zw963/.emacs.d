(require 'bm)

(setq bm-cycle-all-buffers t)
(setq temporary-bookmark-p t)
(setq-default bm-buffer-persistence t)
(setq bm-highlight-style 'bm-highlight-only-fringe)

;; save bookmark
(add-hook 'kill-buffer-hook #'bm-buffer-save)
(add-hook 'kill-emacs-hook #'(lambda nil
                               (bm-buffer-save-all)
                               (bm-repository-save)))

(global-set-key (kbd "<left-fringe> <mouse-5>") 'bm-next-mouse)
(global-set-key (kbd "<left-fringe> <mouse-4>") 'bm-previous-mouse)
(global-set-key (kbd "<left-fringe> <mouse-1>") 'bm-toggle-mouse)

(setq bm-marker 'bm-marker-right)

(add-hook 'after-init-hook #'bm-repository-load)
(add-hook 'find-file-hook #'bm-buffer-restore)
(add-hook 'after-revert-hook #'bm-buffer-restore)

;; (global-set-key (kbd "<f2>")   'bm-next)
;; (global-set-key (kbd "<S-f2>") 'bm-previous)
;; (global-set-key (kbd "<C-f2>") 'bm-toggle)

(provide 'bm_init)

;;; bm_init.el ends here
