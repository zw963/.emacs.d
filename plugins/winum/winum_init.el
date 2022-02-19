(require 'winum)

;; (setq winum-keymap
;;       (let ((map (make-sparse-keymap)))
;;         ;; (define-key map (kbd "C-`") 'winum-select-window-by-number)
;;         ;; (define-key map (kbd "C-²") 'winum-select-window-by-number)
;;         (define-key map (kbd "M-0") 'winum-select-window-0-or-10)
;;         (define-key map (kbd "M-1") 'winum-select-window-1)
;;         (define-key map (kbd "M-2") 'winum-select-window-2)
;;         (define-key map (kbd "M-3") 'winum-select-window-3)
;;         (define-key map (kbd "M-4") 'winum-select-window-4)
;;         (define-key map (kbd "M-5") 'winum-select-window-5)
;;         (define-key map (kbd "M-6") 'winum-select-window-6)
;;         (define-key map (kbd "M-7") 'winum-select-window-7)
;;         (define-key map (kbd "M-8") 'winum-select-window-8)
;;         map))

(global-set-key (kbd "M-1") 'winum-select-window-1)
(global-set-key (kbd "M-2") 'winum-select-window-2)
(global-set-key (kbd "M-3") 'winum-select-window-3)
(global-set-key (kbd "M-4") 'winum-select-window-4)
(global-set-key (kbd "M-5") 'winum-select-window-5)
(global-set-key (kbd "M-6") 'winum-select-window-6)
(global-set-key (kbd "M-7") 'winum-select-window-7)
(global-set-key (kbd "M-8") 'winum-select-window-8)

(defun winum-assign-0-to-treemacs ()
  (when (string-match-p " \\*Treemacs-Scoped-Buffer.*\\*" (buffer-name)) 10))
(add-to-list 'winum-assign-functions #'winum-assign-0-to-treemacs)

(set-face-attribute 'winum-face nil :weight 'bold)

(setq winum-format " wm[%s] ")
;; (setq winum-scope 'frame-local)
;; (add-hook 'winum-mode-hook (lambda ()
;;                              ;; (setq winum-ignored-buffers-regexp '())
;;                              ;; (delete " \\*Treemacs-Scoped-Buffer-" winum-ignored-buffers-regexp)
;;                              ))

(winum-mode)

(provide 'winum_init)

;;; winum_init.el ends here
