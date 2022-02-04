(require 'crux)

(global-set-key [(control x) (\4) (t)] 'crux-transpose-windows)
(global-set-key [(control k)] 'crux-smart-kill-line)
(global-set-key [(control shift return)] 'crux-smart-open-line-above)
(global-set-key [(control return)] 'crux-smart-open-line)
(global-set-key [(control c) (r)] 'crux-rename-file-and-buffer)
(global-set-key [(control c) (D)] 'crux-delete-file-and-buffer)
(global-set-key [(control c) (c)] 'crux-copy-file-preserve-attributes)
(global-set-key [(control c) (b)] 'crux-switch-to-previous-buffer)
(global-set-key [(meta o)] 'crux-other-window-or-switch-buffer)
(global-set-key [(control c) (k)] 'crux-kill-other-buffers)
(global-set-key [(control c) (tab)] 'crux-indent-rigidly-and-copy-to-clipboard)
(global-set-key [(control ?^)] 'crux-top-join-line)
(global-set-key [(control backspace)] 'crux-kill-line-backwards)
(global-set-key [(control ?\d)] 'crux-kill-line-backwards)
(global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line)
(global-set-key [remap kill-whole-line] #'crux-kill-whole-line)
(global-set-key [(control c) (control k)] 'save-buffer-and-kill-buffer-and-window)

(dolist (hook '(prog-mode-hook
                yaml-mode-hook
                elixir-mode-hook
                web-mode-hook
                conf-mode-hook))
  (add-hook hook (lambda ()
                   (local-set-key [(control c) (control c)] 'crux-cleanup-buffer-or-region)
                   (local-set-key [(?\,)] 'input-comma-with-space)
                   (local-set-key [(?\;)] 'input-semicolon-with-space)
                   (local-set-key [(control c) (?\t)] 'crux-indent-rigidly-and-copy-to-clipboard)
                   (global-set-key [(meta c) (\.)] 'input-rocket-with-space)
                   (global-set-key [(meta c) (=)] 'input-add-equal)
                   )))

(crux-reopen-as-root-mode 1)

;; functions

(defun crux-insert-date ()
  "Insert a timestamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%F %T %Z" (current-time))))

(defun input-rocket-with-space ()
  (interactive)
  (insert " => "))

(defun input-add-equal ()
  (interactive)
  (insert " += "))

(defun input-comma-with-space ()
  (interactive)
  (if (or
       ;; (fourth (syntax-ppss))
       ;; (member (fourth (syntax-ppss)) (list ?\/))
       (and (member major-mode '(sh-mode
                                 conf-space-mode
                                 conf-unix-mode
                                 emacs-lisp-mode
                                 lisp-interaction-mode
                                 snippet-mode))
            ;; (fifth (syntax-ppss)) is comment
            (not (fifth (syntax-ppss)))))
      (insert ",")
    (insert ", ")))

(defun input-semicolon-with-space ()
  (interactive)
  (if (fourth (syntax-ppss))
      (insert ";")
    (if (not (looking-back "; " (line-beginning-position)))
        (insert "; ")
      (progn
        (backward-delete-char 1)
        (insert ";")))))

(defun format-buffer ()
  "Perform a bunch of operations of a buffer."
  (interactive)
  (untabify (point-min) (point-max))
  ;; (delete-trailing-whitespace)
  (whitespace-cleanup)
  (indent-region (point-min) (point-max))
  ;; (unless dont-indent-line
  ;;   )
  )

(defun save-buffer-and-kill-buffer-and-window ()
  "Simple convenience function.
  Saves the buffer of the current day's entry and kills the window
  Similar to org-capture like behavior"
  (interactive)
  (save-buffer)
  (kill-buffer-and-window))

;; (defun textmate-next-line ()
;;   "Inserts an indented newline after the current line and moves the point to it."
;;   (interactive)
;;   (end-of-line)
;;   (cond
;;    ((member major-mode '(rust-mode rustic-mode)) (insert ";")
;;     ))
;;   ;; (electric-newline-and-maybe-indent)
;;   ;; (newline-and-indent)

;;   (if (fifth (syntax-ppss))
;;       ;; 记住: 总可以通过Ctrl+Alt+j 执行下面的功能.(注释中新行自动注释)
;;       (default-indent-new-line)
;;     (reindent-then-newline-and-indent)))

;; (defun input-comment-with-rocket ()
;;   (interactive)
;;   (cond
;;    ((member major-mode '(ruby-mode enh-ruby-mode)) (insert "# => "))
;;    ((member major-mode '(js2-mode cc-mode rust-mode rustic-mode)) (insert "// => ")))
;;   )

(setq save-abbrevs 'silently)
(setq-default abbrev-mode t)

(provide 'crux_init)

;;; crux_init.el ends here
