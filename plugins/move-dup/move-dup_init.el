(require 'move-dup)

(global-move-dup-mode t)

(defun move-dup-duplicate-up-then-comment ()
  (interactive)
  (move-dup-duplicate-up 1)
  (comment-region (line-beginning-position) (line-end-position))
  (forward-line 1))

;; (global-set-key [(shift return)] 'move-dup-duplicate-up)
;; (global-set-key [(shift ?\r)] 'move-dup-duplicate-up)
;; (global-set-key [(shift meta return)] 'move-dup-duplicate-up-then-comment)

(global-set-key [(control c) (d)] 'move-dup-duplicate-up)
(global-set-key [(control c) (meta d)] 'move-dup-duplicate-up-then-comment)

(provide 'move-dup_init)
;;; move-dup_init.el ends here
