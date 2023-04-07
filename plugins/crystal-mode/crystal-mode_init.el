(require 'crystal-mode)
(require 'lsp-crystal_init)
(require 'flycheck-crystal)
(require 'crystal-mode_keyword_highlight_init)

(require 'ameba)
(require 'flycheck-ameba)
(flycheck-ameba-setup)

(defun crystal-project-compile ()
  (interactive)
  (let* ((cmd "git rev-parse --show-toplevel")
         (topdir (with-temp-buffer
                   (call-process-shell-command cmd nil t nil)
                   (goto-char (point-min))
                   (if (re-search-forward "^\\(.+\\)$" nil t)
                       (match-string 1)))))
    (quickrun :source `((:command . "shards")
                        (:default-directory . ,topdir)
                        (:exec . ("%c build --no-color"))))))

(defun crystal-project-run ()
  (interactive)
  (let* ((cmd "git rev-parse --show-toplevel")
         (topdir (with-temp-buffer
                   (call-process-shell-command cmd nil t nil)
                   (goto-char (point-min))
                   (if (re-search-forward "^\\(.+\\)$" nil t)
                       (match-string 1)))))
    (quickrun :source `((:command . "shards")
                        (:default-directory . ,topdir)
                        (:exec . ("%c run --no-color"))))))

(add-hook 'crystal-mode-hook
          (lambda ()
            ;; 如果开启了 lsp 里面的 lsp-format-buffer, 则关闭这个。
            ;; (add-hook 'before-save-hook #'crystal-tool-format nil 'local)
            (define-key crystal-mode-map [(meta \.) ] #'crystal-tool-imp)
            (define-key crystal-mode-map [(control meta \.) ] #'crystal-tool-expand)
            (define-key crystal-mode-map [(control meta \,) ] #'crystal-tool-context)
            (define-key crystal-mode-map [(control t) ] #'crystal-spec-switch)
            (define-key crystal-mode-map [(control meta ?\s)] 'ruby-mark-sexp-or-block)
            (define-key crystal-mode-map [(control c) (tab)] 'crystal-project-compile)
            (define-key crystal-mode-map [(control c) (return)] 'crystal-project-run)
            ;; (local-set-key [(kbd "RET")] 'reindent-then-newline-and-indent)
            ;; (local-set-key [(return)] 'reindent-then-newline-and-indent)
            ))

(provide 'crystal-mode_init)

;;; crystal-mode_init.el ends here
