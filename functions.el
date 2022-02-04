;; stolen from rinari
(defun self-defined-highlight-keywords (keywords &optional face)
  "Highlight the passed KEYWORDS in current buffer.
Use `font-lock-add-keywords' in case of `ruby-mode' or
`ruby-extra-keywords' in case of Enhanced Ruby Mode."
  (font-lock-add-keywords
   nil
   (list (list
          (concat "\\(^\\|[^_:.@$]\\|\\.\\.\\)\\b"
                  (regexp-opt keywords t)
                  (eval-when-compile (if (string-match "\\_>" "ruby")
                                         "\\_>"
                                       "\\>"))
                  )
          (list 2 (or face 'font-lock-builtin-face))))))

(require 'sgml-mode)
(defun format-xml ()
  (interactive)
  (save-excursion
    (sgml-pretty-print (point-min) (point-max))
    (indent-region (point-min) (point-max))))

;; ============================== 快捷键相关 ==============================

(defun kill-buffer-enhanced ()
  (interactive)
  (if (equal major-mode 'org-mode)
      (progn
        (call-interactively 'save-buffer)
        (call-interactively 'bury-buffer))
    (progn
      (setq menu-updating-frame nil)
      (kill-buffer (current-buffer)))))

;; (defun set-webpack-watch-dog ()
;;   (let ((watch-dog-file (getenv "WEBPACK_WATCH_DOG")))
;;     (print watch-dog-file)
;;     (when (and
;;            watch-dog-file
;;            (not (string= watch-dog-file (buffer-file-name)))
;;            (string-match "/app/views/" (buffer-file-name))
;;            )
;;       (shell-command-to-string (concat "add_webpack_watch_dog " watch-dog-file)))))
;; (add-hook 'before-save-hook 'set-webpack-watch-dog nil t)

(defun my-find-file-check-make-large-file-read-only-hook ()
  "If a file is over a given size (about 1M), make the buffer read only."
  (when (> (buffer-size) (* 1024 1024 10))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    ;; (ad-deactivate 'kill-ring-save)
    (fundamental-mode)
    ;; (when (boundp 'auto-indent-mode) (auto-indent-mode -1))
    ;; (when (boundp 'undo-tree-mode) (undo-tree-mode -1))
    ;; (when (boundp 'show-paren-mode) (show-paren-mode -1))
    ;; (when (boundp 'hl-line-mode) (hl-line-mode -1))
    ;; (when (boundp 'delete-selection-mode) (delete-selection-mode -1))
    ;; (when (boundp 'electric-indent-mode) (electric-indent-mode -1))
    ;; (when (boundp 'electric-pair-mode) (electric-pair-mode -1))
    ;; (when (boundp 'font-lock-mode) (font-lock-mode -1))
    ;; (when (boundp 'hi-lock-mode) (hi-lock-mode -1))
    ;; (when (boundp 'auto-complete-mode) (auto-complete-mode -1))
    ;; (when (boundp 'git-gutter-mode) (git-gutter-mode -1))
    ;; (when (boundp 'flycheck-mode) (flycheck-mode -1))
    ;; (when (boundp 'save-place-mode) (save-place-mode -1))
    ;; (when (boundp 'super-save-mode) (super-save-mode -1))
    ;; (when (boundp 'yafolding-mode) (yafolding-mode -1))
    ;; (when (boundp 'yas-minor-mode) (yas-minor-mode -1))
    ;; (when (boundp 'line-number-mode) (line-number-mode -1))
    ;; (when (boundp 'column-number-mode) (column-number-mode -1))
    ;; (when (boundp 'auto-revert-mode) (auto-revert-mode -1))
    ;; (when (boundp 'page-break-lines-mode) (page-break-lines-mode -1))
    ))
(add-hook 'find-file-hook 'my-find-file-check-make-large-file-read-only-hook)
