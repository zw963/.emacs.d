(defcustom crystal-spec-keywords
  '(
    "pending"
    "context"
    )
  "List of keywords to highlight for crystal spec."
  :group 'rinari
  :type '(repeat string)
  )

(defun crystal-highlight-keywords (keywords &optional face)
  "Highlight the passed KEYWORDS FACE in current buffer.
Use `font-lock-add-keywords' in case of `ruby-mode' or
`ruby-extra-keywords' in case of Enhanced Ruby Mode."
  (font-lock-add-keywords
   nil
   (list (list
          (concat "\\(^\\|[^_:.@$]\\|\\.\\.\\)\\b"
                  (regexp-opt keywords t)
                  (eval-when-compile (if (string-match "\\_>" "crystal")
                                         "\\_>"
                                       "\\>")))
          (list 2 (or face 'font-lock-keyword-face))))))

(defun crystal-apply-keywords-for-file-type ()
  "Apply extra font lock keywords specific to models, controllers etc."
  (when (buffer-file-name)
    (cl-loop for (re keywords) in
             `(
               (".+_spec\\.cr$\\|.+_test\.cr$" ,crystal-spec-keywords)
               )
             do (when (string-match-p re (buffer-file-name))
                  (crystal-highlight-keywords keywords 'font-lock-keyword-face)
                  ))))

(add-hook 'crystal-mode-hook 'crystal-apply-keywords-for-file-type)


(provide 'ruby-mode_keyword_highlight_init)

;;; ruby-mode_keyword_highlight_init_init.el ends here
