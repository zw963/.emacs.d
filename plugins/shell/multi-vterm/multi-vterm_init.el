;; -*- lexical-binding: t; -*-

(require 'multi-vterm)

(defun toggle-drop-down-term ()
  "Open a drop-down terminal in the same directory as the current file."
  (interactive)
  (require 'multi-vterm)
  (let ((win (get-local-window-for-buffer-name "*vterminal")))
    (if win
        (delete-window win)
      (let ((vterm-buffer
             (or
              (get-local-buffer-for-buffer-name "*vterminal")
              (get-buffer-create (multi-vterm-get-buffer))
              )))
        (with-current-buffer vterm-buffer
          (unless (derived-mode-p 'vterm-mode)
            (setq multi-vterm-buffer-list (nconc multi-vterm-buffer-list (list vterm-buffer)))
            (set-buffer vterm-buffer)
            (multi-vterm-project)))
        (setq win
              (display-buffer-in-side-window
               vterm-buffer
               '((side . top)
                 (window-height . 0.5)
                 )))
        (select-window win)))))

(global-set-key (kbd "C-`") 'toggle-drop-down-term)

(require 'vterm-edit-command)

(add-hook 'vterm-mode-hook
          (lambda ()
            (define-key vterm-mode-map [(control shift k)] 'vterm-clear)
            (define-key vterm-mode-map [(meta w)] 'kill-ring-save)
            (define-key vterm-mode-map [(control v)] 'scroll-up-command)
            (define-key vterm-mode-map [(meta v)] 'scroll-down-command)
            (define-key vterm-mode-map (kbd "M-0") 'multi-vterm-prev)
            (define-key vterm-mode-map (kbd "M-9") 'multi-vterm-next)
            (define-key vterm-mode-map [(control x) (\2)] 'split-window-below-then-switch-to-new-vterm)
            (define-key vterm-mode-map [(control x) (\3)] 'split-window-right-then-switch-to-new-vterm)
            (define-key vterm-mode-map [(shift control t)] 'multi-vterm)
            (define-key vterm-mode-map [(control x) (control e)] 'vterm-edit-command-action)
            ;; (dirtrack-mode 1)
            ))

;; (add-to-list 'display-buffer-alist '
;;              (
;;               "^\\*vterminal"
;;               (display-buffer-reuse-window display-buffer-in-side-window)
;;               (side . top)
;;               (window-height . 0.5)
;;               ))

(defun split-window-below-then-switch-to-new-vterm (&optional size)
  (interactive)
  (split-window-below size)
  (other-window 1)
  (multi-vterm)
  )
(defun split-window-right-then-switch-to-new-vterm (&optional size)
  (interactive)
  (split-window-right size) (other-window 1) (multi-vterm))

;; (setq multi-vterm-dedicated-window-height 50)
;; ;; 可以使用 Ctrl+x j, Ctrl+x l 来切换 buffer 和 vterm.
;; (global-set-key [(shift control t)] 'multi-vterm-dedicated-toggle)

(provide 'multi-vterm_init)

;;; multi-vterm_init.el ends here
