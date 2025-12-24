;; -*- lexical-binding: t; -*-

(require 'ruby-toggle-block_init)

(defun no-moving-newline-and-indent ()
  (interactive)
  (save-excursion
    (newline-and-indent)))

(defun transform-parens-fixed (begin end)
  (interactive)
  (let ((bol (line-beginning-position))
        (eol (line-end-position)))
    (when (search-backward-regexp begin bol t)
      (forward-char 1)
      (newline-and-indent)
      (when (search-forward-regexp end eol t)
        (backward-char 1)
        (no-moving-newline-and-indent)
        (newline-and-indent)))))

(defun meta-return-hacked ()
  "toggle block, if can not do it, run newline-and-indent."
  (interactive)
  (let* ((ppss (syntax-ppss)))
    (cond
     (;; inside a string
      (cl-fourth ppss)
      (call-interactively 'newline-and-indent))

     (;; inside a comment.
      (and (cl-fifth ppss) (eq (point) (line-end-position)))
      (call-interactively 'indent-new-comment-line))

     ((and (memq (char-before) '(?\{ ?\[ ?\())
                 (memq (char-after) '(?\} ?\] ?\))))
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

     ;; 在 [| 或 |] 或 (| 或 |), 做类似于上面的变换 {
     ((and (looking-back "[[(].*,\\s-*" (line-beginning-position))
           (looking-at "\\s-*[])]"))
      (transform-parens-fixed "[[(]" "[])]"))

     ((member major-mode '(ruby-mode enh-ruby-mode crystal-mode))
      (ruby-toggle-block-fixed))

     ((member major-mode '(dart-mode))
      (flutter-unwrap-function-body))
     )))

(defun meta-return-hacked-init()
  (interactive)
  (local-set-key [(meta return)] 'meta-return-hacked)
  (local-set-key [(meta ?\r)] 'meta-return-hacked)
  )

(add-hook 'prog-mode-hook 'meta-return-hacked-init)

(provide 'meta-return-hack_init)

;;; meta-return-hack_init.el ends here
