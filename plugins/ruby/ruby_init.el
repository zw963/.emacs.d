;; (require 'enh-ruby-mode_init)
(require 'ruby-mode_init)
(require 'ruby-ffap)
(require 'rvm_init)
(require 'ruby-test-mode_init)
(require 'ruby-mode_keyword_highlight_init)
(require 'rbs-mode_init)
(require 'robe-mode_init)
(require 'yard-mode_init)

;; rbtagger 和 ctags-update，lsp 冲突。
;; (require 'rbtagger_init)

;; (require 'ctags-update_init)
;; (require 'ruby-mode-lsp_init)

;; (require 'rspec-mode_init)
;; (require 'yari_init)

(let ((preferred-ruby-mode (if (fboundp 'enh-ruby-mode) 'enh-ruby-mode 'ruby-mode)))
  (add-to-list 'auto-mode-alist
               `("\\.\\(rb\\|rake\\|ru\\|gemspec\\|jbuilder\\|rjs\\|xlsx\.axlsx\\)\\'"
                 . ,preferred-ruby-mode))
  (add-to-list 'auto-mode-alist
               `("\\(Rakefile\\|Gemfile\\|Capfile\\|Appraisals\\|Guardfile\\)\\'"
                 . ,preferred-ruby-mode))
  (add-to-list 'interpreter-mode-alist `("ruby" . ,preferred-ruby-mode)))

(defun ruby-mark-sexp-or-block (arg)
  (interactive "p")
  (push-mark nil t t)
  (cond
   ((looking-at "['\"]") (ruby-forward-sexp))
   (
    ;; (looking-at (concat (regexp-opt ruby-block-start-keywords) "\\s-+\\(.+\\s-+\\)*do[\s\t\n]+"))
    ;; Emacs 的正则很奇怪,
    ;; 例如: 要匹配 ], 它必须是在第一个的位置, 要匹配 -, 它必须在最后的位置.
    (looking-at (concat "[]a-zA-Z_.[0-9(){}:/ \"'=>-]*" "\\s-+\\(.+\\s-+\\)*do[\s\t\n]+"))
    (if (featurep 'enh-ruby-mode)
        (enh-ruby-end-of-block)
      (progn
        (ruby-end-of-block)
        (forward-word 1))))
   ;; ((fourth (syntax-ppss)) (er/expand-region arg))
   (t
    (if (featurep 'enh-ruby-mode)
        (enh-ruby-forward-sexp)
      (ruby-forward-sexp)))))

(defun seeing-is-believing ()
  "Replace the current region (or the whole buffer, if none) with the output
of seeing_is_believing."
  (interactive)
  (let ((beg (if (region-active-p) (region-beginning) (point-min)))
        (end (if (region-active-p) (region-end) (point-max)))
        (origin (point)))
    (unless (save-excursion (next-line) (beginning-of-line) (looking-at "\\s-*# =>"))
      (save-excursion
        (end-of-line)
        (newline-and-indent)
        (insert "# =>"))
      )
    (shell-command-on-region beg end "seeing_is_believing -x" nil 'replace)
    (goto-char origin))
  (when (featurep 'enh-ruby-mode)
    (sleep-for 0.5)
    (call-interactively 'enh-ruby-fontify-buffer)))

(defun ruby-mode-common-init()
  (interactive)
  ;; 标记一个 func 是 C-M-h, 这是默认，记住！
  ;; enh-ruby-mode 中, 是 enh-ruby-mark-defun

  (local-set-key [(control meta ?\s)] 'ruby-mark-sexp-or-block)
  (local-set-key [(kbd "RET")] 'reindent-then-newline-and-indent)
  (local-set-key [(return)] 'reindent-then-newline-and-indent)

  ;; (local-set-key [(control meta d)] 'smie-down-list) ;; 这个是默认
  (local-set-key [remap ruby-send-last-sexp] 'seeing-is-believing)
  )

(run-ruby-mode-hook '(ruby-mode-common-init))

;; ============================== 函数定义 ==============================

(defun rinari-root (&optional dir home)
  "Return the root directory of the project within which DIR is found.
Optional argument HOME is ignored."
  (let ((default-directory (or dir default-directory)))
    (when (file-directory-p default-directory)
      (if (file-exists-p (expand-file-name "environment.rb" (expand-file-name "config")))
          default-directory
        ;; regexp to match windows roots, tramp roots, or regular posix roots
        (unless (string-match "\\(^[[:alpha:]]:/$\\|^/[^\/]+:/?$\\|^/$\\)" default-directory)
          (rinari-root (expand-file-name (file-name-as-directory ".."))))))))

(defun ruby-testfile-p ()
  (let ((name (or (buffer-file-name) (buffer-name))))
    (string-match "_test[0-9]*\.rb$" name)))

(defun ruby-specfile-p ()
  (let ((name (or (buffer-file-name) (buffer-name))))
    (string-match "_spec[0-9]*\.rb$" name)))

(provide 'ruby_init)

;;; ruby_init.el ends here
