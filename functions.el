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

(defun dired-filter-by-name(filter-regexp)
  (interactive "s(only show matched):")
  (let ((dired-marker-char 16)
        (files (directory-files default-directory t)))
    ;;(dired-unmark-all-files dired-marker-char)
    (save-excursion
      (dolist (file files)
        (when (and (dired-goto-file (expand-file-name file))
                   (not (string= "" filter-regexp))
                   (string-match filter-regexp (file-name-nondirectory file)))
          (dired-mark 1)
          )))
    (dired-toggle-marks)
    (dired-do-kill-lines nil (concat "Filter:'" filter-regexp "' omitted %d line%s"))
    (dired-move-to-filename)))

(defun translate-this-word-or-region ()
  (interactive)
  (if (use-region-p)
      (run-process "trans" "-b" "en:zh-CN" (buffer-substring (region-beginning) (region-end)))
    (run-process "dict1" (current-word t t))
    ))

;; ============================== 快捷键相关 ==============================
(defun backtab-space (&optional indent-count)
  "remove 4 spaces from beginning of of line"
  (interactive)
  (save-excursion
    (save-match-data
      (beginning-of-line)
      ;; get rid of tabs at beginning of line
      (when (looking-at "^\\s-+")
        (untabify (match-beginning 0) (match-end 0)))
      (when (looking-at (concat "^" (make-string (or indent-count 2) ? )))
        (replace-match "")))))

(defun copy-current-buffer-file-name ()
  (interactive)
  (if (eq major-mode 'dired-mode)
      (dired-copy-filename-as-kill 0)
    (let ((root (if (locate-dominating-file default-directory ".git")
                    (expand-file-name (locate-dominating-file default-directory ".git"))
                  "")))
      (kill-new (replace-regexp-in-string (regexp-quote root) "" (concat (buffer-file-name) ":" (number-to-string (line-number-at-pos)))))
      )))

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (basename (file-name-nondirectory filename)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " (file-name-directory filename) basename nil basename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun transpose-current-char-backward (&optional arg)
  "Move current word left."
  (interactive "p")
  (forward-char 1)
  (transpose-chars (- 1))
  (forward-char -1))

(defun transpose-current-char (&optional arg)
  "Move current char right."
  (interactive "p")
  (forward-char 1)
  (transpose-chars 1)
  (forward-char -1))

(defun other-window-move-up (&optional arg)
  "Other window move-up 1 lines."
  (interactive "p")
  (scroll-other-window arg))
(defun other-window-move-down (&optional arg)
  "Other window move-down 2 lines."
  (interactive "P")
  (if arg
      (scroll-other-window-down arg)
    (scroll-other-window-down 2)))

(defun window-move-up (&optional arg)
  "Window move-up 2 lines."
  (interactive "P")
  (if arg
      (scroll-up arg)
    (scroll-up 2)))
(defun window-move-down (&optional arg)
  "Window move-down 3 lines."
  (interactive "P")
  (if arg
      (scroll-down arg)
    (scroll-down 3)))

(defun split-window-below-then-switch-to (&optional size)
  (interactive)
  (split-window-below size) (other-window 1))
(defun split-window-right-then-switch-to (&optional size)
  (interactive)
  (split-window-right size) (other-window 1))

(defun kill-buffer-enhanced ()
  (interactive)
  (if (equal major-mode 'org-mode)
      (progn
        (call-interactively 'save-buffer)
        (call-interactively 'bury-buffer))
    (progn
      (setq menu-updating-frame nil)
      (kill-buffer (current-buffer)))))

(defun run-ruby-mode-hook (func)
  (dolist (hook '(ruby-mode-hook enh-ruby-mode-hook))
    (add-hook hook `(lambda ()
                      ,func
                      ))))

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
