;; -*- lexical-binding: t; -*-

(defcustom crystal-spec-keywords '("describe" "pending" "context")
  "Keywords to highlight in Crystal spec/test files."
  :type '(repeat string))

(defvar-local crystal--spec-font-lock-added nil)

(defun crystal--maybe-add-spec-keywords ()
  (when (and (buffer-file-name)
             (string-match-p "\\(?:_spec\\|_test\\)\\.cr\\'" (buffer-file-name))
             (not crystal--spec-font-lock-added))
    (setq crystal--spec-font-lock-added t)
    (font-lock-add-keywords
     nil
     `((,(concat "\\_<" (regexp-opt crystal-spec-keywords t) "\\_>")
        0 font-lock-keyword-face)))
    (font-lock-flush)
    (font-lock-ensure)))

(add-hook 'crystal-mode-hook #'crystal--maybe-add-spec-keywords)

(provide 'crystal-mode_keyword_highlight_init)

;;; crystal-mode_keyword_highlight_init.el ends here
