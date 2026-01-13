;; -*- lexical-binding: t; -*-

(require 'crystal-mode)
(require 'lsp-crystal_init)
(require 'flycheck-crystal)
(require 'crystal-mode_keyword_highlight_init)

(require 'ameba)
(require 'flycheck-ameba)
(with-eval-after-load 'flycheck (flycheck-ameba-setup))


;; (defun crystal-project-compile ()
;;   (interactive)
;;   (let* ((cmd "git rev-parse --show-toplevel")
;;          (topdir (with-temp-buffer
;;                    (call-process-shell-command cmd nil t nil)
;;                    (goto-char (point-min))
;;                    (if (re-search-forward "^\\(.+\\)$" nil t)
;;                        (match-string 1)))))
;;     (quickrun :source `((:command . "shards")
;;                         (:default-directory . ,topdir)
;;                         (:exec . ("%c build --no-color"))))))

;; (defun crystal-project-run ()
;;   (interactive)
;;   (let* ((cmd "git rev-parse --show-toplevel")
;;          (topdir (with-temp-buffer
;;                    (call-process-shell-command cmd nil t nil)
;;                    (goto-char (point-min))
;;                    (if (re-search-forward "^\\(.+\\)$" nil t)
;;                        (match-string 1)))))
;;     (quickrun :source `((:command . "shards")
;;                         (:default-directory . ,topdir)
;;                         (:exec . ("%c run --no-color"))))))

(with-eval-after-load 'crystal-mode
  (define-key crystal-mode-map [(control meta f) ] #'crystal-forward-sexp)
  (define-key crystal-mode-map [(control meta b) ] #'crystal-backward-sexp)
  (define-key crystal-mode-map [(control t) ] #'crystal-spec-switch)
  ;; (define-key crystal-mode-map [(meta \.) ] #'crystal-tool-imp)
  ;; (define-key crystal-mode-map [(control meta \.) ] #'crystal-tool-expand)
  ;; (define-key crystal-mode-map [(control meta \,) ] #'crystal-tool-context)

  ;; (define-key crystal-mode-map [(control meta f) ] #'ruby-end-of-block)
  ;; (define-key crystal-mode-map [(control meta b) ] #'ruby-beginning-of-block)

  ;; (define-key crystal-mode-map [(control meta ?\s)] 'mark-sexp)
  ;; (define-key crystal-mode-map [(control c) (tab)] 'crystal-project-compile)
  ;; (define-key crystal-mode-map [(control c) (return)] 'crystal-project-run)
  )

(defun my/crystal-enable-treesit-parser ()
  "Create a tree-sitter parser for Crystal in the current buffer."
  (when (treesit-ready-p 'crystal)
    (treesit-parser-create 'crystal)))

(add-hook 'crystal-mode-hook
          (lambda ()
            ;; 如果开启了 lsp 里面的 lsp-format-buffer, 则关闭这个。
            ;; (add-hook 'before-save-hook #'crystal-tool-format nil 'local)

            (my/crystal-enable-treesit-parser)
            ))

(provide 'crystal-mode_init)

;;; crystal-mode_init.el ends here

;; ;; 检测是否支持 tree-sitter
;; ;; (treesit-language-available-p 'crystal)

;; ;; 告诉 Emacs：Crystal grammar 从哪里拉、用哪个分支、源码子目录在哪
;; ;; treesit-language-source-alist 的格式是 (LANG . (URL REV SOURCE-DIR CC C++))，后面几项可省略。:contentReference[oaicite:4]{index=4}

;; ;; 然后执行一次安装（交互式）：
;; ;; M-x treesit-install-language-grammar 回车
;; ;; 输入 crystal 回车

;; (add-to-list 'treesit-language-source-alist
;;              '(crystal "https://github.com/crystal-lang-tools/tree-sitter-crystal"
;;                        "main" "src"))
;; (require 'crystal-ts-mode)
;; (add-to-list 'major-mode-remap-alist '(crystal-mode . crystal-ts-mode))
