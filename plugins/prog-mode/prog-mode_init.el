(require 'prog-mode)
(require 'hideshow_init)
(require 'meta-return-hack_init)

(dolist (hook '(prog-mode-hook
                yaml-mode-hook
                elixir-mode-hook
                web-mode-hook
                conf-mode-hook))
  (add-hook hook (lambda ()
                   (local-set-key [(control c) (control c)] 'format-buffer)
                   (local-set-key [(?\,)] 'input-comma-with-space)
                   (local-set-key [(?\;)] 'input-semicolon-with-space)
                   (global-set-key [(meta c) (\.)] 'input-rocket-with-space)
                   (global-set-key [(meta c) (=)] 'input-add-equal)
                   )))

(add-hook 'prog-mode-hook (lambda ()
                            (font-lock-add-keywords
                             nil '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\|NOTICE\\|WARN\\):"
                                    1 font-lock-warning-face t)))
                            (subword-mode)           ; 不要全局开启 subword-mode，对 ido 有影响。
                            (goto-address-prog-mode)
                            (display-fill-column-indicator-mode) ;; 全局开启会造成 helm 也显示.
                            (setq-local indent-tabs-mode nil)      ;禁止 insert \t 字符.
                            ;; (context-menu-mode t)

                            ;; 注意最后一个参数 t, 这确保了当前 before-save-hook 是 local 的。
                            ;; (add-hook 'before-save-hook
                            ;;           (lambda()
                            ;;             (save-excursion
                            ;;               ;; (whitespace-cleanup)
                            ;;               (delete-trailing-whitespace)))
                            ;;           nil t)
                            ))

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

(defun input-rocket-with-space ()
  (interactive)
  (insert " => "))

(defun input-add-equal ()
  (interactive)
  (insert " += "))

;; (defun input-comment-with-rocket ()
;;   (interactive)
;;   (cond
;;    ((member major-mode '(ruby-mode enh-ruby-mode)) (insert "# => "))
;;    ((member major-mode '(js2-mode cc-mode rust-mode rustic-mode)) (insert "// => ")))
;;   )

(provide 'prog-mode_init)

;;; prog-mode_init.el ends here
