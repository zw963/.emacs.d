(setq kill-do-not-save-duplicates t)

(require 'browse-kill-ring)
(setq browse-kill-ring-highlight-current-entry t)
(setq browse-kill-ring-highlight-inserted-item 'cedet)
(setq browse-kill-ring-separator "\f")
(browse-kill-ring-default-keybindings)

;; (defun yank-pop-secondary-ring (&optional arg)
;;   (interactive)
;;   (let ((browse-kill-ring-current-ring 'secondary-selection-ring))
;;     (call-interactively 'yank-pop)))

(global-set-key "\C-cy" (lambda ()
                          (interactive)
                          (popup-menu 'yank-menu)))

(add-hook 'browse-kill-ring-mode-hook
          (lambda ()
            (define-key browse-kill-ring-mode-map [(control \8)] 'kill-buffer-and-window)
            (define-key browse-kill-ring-mode-map [(O)] 'browse-kill-ring-occur)
            (define-key browse-kill-ring-mode-map (kbd "RET") 'browse-kill-ring-insert-move-and-quit)
            (define-key browse-kill-ring-mode-map [(control g)] 'browse-kill-ring-quit)
            ;; 为了让 M-y 和 global-hl-line-mode 一起工作,
            ;; 必须在 browse-king-ring-mode 中关闭 hl-line-mode
            ;; 否则 n, p 快捷键不工作.
            (make-local-variable 'global-hl-line-mode)
            (setq global-hl-line-mode nil)
            ))

(defun copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring.
      Ease of use features:
      - Move to start of next line.
      - Appends the copy on sequential calls.
      - Use newline as last char even on the last line of the buffer.
      - If region is active, copy its lines."
  (interactive "p")
  (let ((beg (line-beginning-position))
        (end (line-end-position arg)))
    (when mark-active
      (if (> (point) (mark))
          (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
        (setq end (save-excursion (goto-char (mark)) (line-end-position)))))
    (if (eq last-command 'kill-ring-save)
        (kill-append (buffer-substring beg end) (< end beg))
      (copy-region-as-kill beg end)))
  (kill-append "\n" nil)
  (beginning-of-line (or (and arg (1+ arg)) 2))
  (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))

;; (require 'popup-kill-ring)
;; (global-set-key [(meta y)] 'popup-kill-ring)

(provide 'browse-kill-ring_init)
;;; browse-kill-ring_init.el ends here
