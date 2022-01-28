(defun no-moving-newline-and-indent ()
  (interactive)
  (save-excursion
    (newline-and-indent)))

(defun transform-parens-fixed (begin end)
  (interactive)
  (search-backward-regexp begin (line-beginning-position) t)
  (forward-char)
  (newline-and-indent)
  (search-forward-regexp end (line-end-position) t)
  (backward-char)
  (no-moving-newline-and-indent)
  (newline-and-indent)
  )

;; (defun ruby-do-end-to-brace-fixed (orig end)
;;   (let (beg-marker end-marker beg-pos end-pos)
;;     (goto-char (- end 3))
;;     (when (looking-at ruby-block-end-re)
;;       (delete-char 3)
;;       (setq end-marker (point-marker))
;;       (insert "}")
;;       (goto-char orig)
;;       (delete-char 2)
;;       ;; Maybe this should be customizable, let's see if anyone asks.
;;       (insert "{")
;;       (setq beg-marker (point-marker))
;;       (when (looking-at "\\s +|")
;;         (delete-char (- (match-end 0) (match-beginning 0) 1))
;;         (forward-char)
;;         (re-search-forward "|" (line-end-position) t))
;;       (save-excursion
;;         (skip-chars-forward " \t\n\r")
;;         (setq beg-pos (point))
;;         (goto-char end-marker)
;;         (skip-chars-backward " \t\n\r")
;;         (setq end-pos (point)))
;;       (when (or
;;              (< end-pos beg-pos)
;;              (and (= (line-number-at-pos beg-pos) (line-number-at-pos end-pos))
;;                   (< (+ (current-column) (- end-pos beg-pos) 2) fill-column)))
;;         (just-one-space -1)
;;         (goto-char end-marker)
;;         (just-one-space -1))
;;       (goto-char beg-marker))))

(defun next-line-empty-p ()
  (save-excursion
    (forward-line)
    (looking-at "[[:space:]]*$")))

(defun ruby-toggle-block-fixed ()
  "Toggle block type from do-end to braces or back.
The block must begin on the current line or above it and end after the point.
If the result is do-end block, it will always be multiline."
  (interactive)
  (let ((start (point)) beg end)
    (end-of-line)
    (unless
        (if (and (re-search-backward "\\(?:[^#]\\)\\({\\)\\|\\(\\_<do\\_>\\)")
                 (progn
                   (goto-char (or (match-beginning 1) (match-beginning 2)))
                   (setq beg (point))
                   (save-match-data
                     (if (eql major-mode 'enh-ruby-mode)
                         (enh-ruby-forward-sexp)
                       (forward-sexp)))
                   (setq end (point))
                   (> end start)))
            (if (match-beginning 1)
                (progn
                  (if (eql major-mode 'enh-ruby-mode)
                      (enh-ruby-brace-to-do-end beg end)
                    (ruby-brace-to-do-end beg end))
                  (end-of-line)
                  (if (next-line-empty-p)
                      (progn
                        (forward-line)
                        (indent-for-tab-command))
                    (newline-and-indent)))
              (ruby-do-end-to-brace beg end)
              )))))

(defun ruby-meta-return ()
  "toggle block, if can not do it, run newline-and-indent."
  (interactive)
  (cond
   ;; inside a string
   ((cl-fourth (syntax-ppss)) (call-interactively 'newline-and-indent))

   ((and
     ;; inside a comment.
     (cl-fifth (syntax-ppss))
     (eq (point) (line-end-position)))
    (call-interactively 'indent-new-comment-line))

   ;; 在 {|} [|] (|)
   ((and (member (char-before) '(123 91 40)) ; { [ (
         (member (char-after) '(125 93 41))) ; } ] )
    (progn
      (no-moving-newline-and-indent)
      (newline-and-indent))
    )

   ;; 在哈希定义的第一行, 输入逗号之后
   ;; 例如：x = {x: 100, |}
   ;; 将变成：
   ;; x = {
   ;;   x: 100,
   ;;   |
   ;; }
   ((and
     (looking-back "{.*=>.*,\\s-*\\|{.*:.*,\\s-*" (line-beginning-position))
     (looking-at "\\s-*}"))
    (transform-parens-fixed "{" "}")
    )

   ;; 在 [| 或 |] 或 (| 或 |), 变换类似上面 {
   ((and (looking-back "[[(].*,\\s-*" (line-beginning-position))
         (looking-at "\\s-*[])]"))
    (transform-parens-fixed "[[(]" "[])]"))

   (t (ruby-toggle-block-fixed))))

(defun ruby-meta-return-init()
  (interactive)
  (local-set-key [(meta return)] 'ruby-meta-return)
  (local-set-key [(meta ?\r)] 'ruby-meta-return)
  )

(add-hook 'ruby-mode-hook 'ruby-meta-return-init)
(add-hook 'enh-ruby-mode-hook 'ruby-meta-return-init)

(provide 'ruby-meta-return_init)

;;; ruby-meta-return_init.el ends here.
